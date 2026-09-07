open! Core

let draw_centered_text context ~font ~fill ~size ~baseline_y ~left ~right text =
  let rendered_text = Font.render_text font text ~size in
  Drawing.Text.blit_rendered_text
    context
    ~fill
    ~origin_x:(left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
    ~baseline_y
    rendered_text
;;

let fahrenheit_text = function
  | None -> "??"
  | Some celsius ->
    (celsius *. 9. /. 5.) +. 32. |> Float.iround_nearest_exn |> Int.to_string
;;

let moon_fill
      ~light_fill
      ~dark_fill
      ~center:(center_x, center_y)
      ~radius
      ~phase
      ((pixel_x, pixel_y) as point)
  =
  let x = Float.of_int (pixel_x - center_x) in
  let y = pixel_y - center_y in
  let half_width = Float.sqrt (Float.of_int ((radius * radius) - (y * y))) in
  let terminator_x = Float.cos (2. *. Float.pi *. phase) *. half_width in
  let is_light =
    match Float.compare phase 0. <= 0 || Float.compare phase 1. >= 0 with
    | true -> false
    | false when Float.compare phase 0.5 <= 0 -> Float.compare x terminator_x >= 0
    | false -> Float.compare x (-.terminator_x) <= 0
  in
  match is_light with
  | true -> light_fill point
  | false -> dark_fill point
;;

let draw_sun_moon
      context
      ~light_fill
      ~dark_fill
      ~is_night
      ~weather
      ~font
      ~text_fill
      ~status_text_size
      ~center:((center_x, center_y) as center)
      ~radius
  =
  let open Drawing.O in
  let fill =
    match is_night, weather.Weather_info.moon_phase with
    | false, _ | true, None -> light_fill
    | true, Some phase -> moon_fill ~light_fill ~dark_fill ~center ~radius ~phase
  in
  (match is_night with
   | true -> ()
   | false ->
     let tick_stroke = Stroke.create light_fill 5 in
     let point distance angle =
       ( Float.of_int center_x +. (Float.cos angle *. Float.of_int distance)
       , Float.of_int center_y +. (Float.sin angle *. Float.of_int distance) )
     in
     List.iter (List.range 0 8) ~f:(fun index ->
       let angle = Float.of_int index *. Float.pi /. 4. in
       draw_line
         context
         ~stroke:tick_stroke
         (point (radius + 10) angle)
         (point (radius + 25) angle)));
  circle context ~fill ~center ~radius;
  let temperature_text = fahrenheit_text weather.current_temperature_celsius
  and low_high_text =
    [ "l" ^ fahrenheit_text weather.low_temperature_celsius
    ; "h" ^ fahrenheit_text weather.high_temperature_celsius
    ]
    |> String.concat ~sep:"  "
  and temperature_size = 70.
  and text_spacing = 4 in
  let rendered_temperature = Font.render_text font temperature_text ~size:temperature_size
  and rendered_low_high = Font.render_text font low_high_text ~size:status_text_size
  and rendered_uv =
    Option.bind weather.maximum_uv_index ~f:(fun uv ->
      match Float.compare uv 6. > 0 with
      | false -> None
      | true ->
        let text = "uv " ^ (uv |> Float.iround_nearest_exn |> Int.to_string) in
        Some (text, Font.render_text font text ~size:status_text_size))
  in
  let uv_height =
    Option.value_map rendered_uv ~default:0 ~f:(fun (_, rendered_uv) ->
      text_spacing + rendered_uv.height)
  in
  let text_top =
    center_y
    - ((rendered_temperature.height + text_spacing + rendered_low_high.height + uv_height)
       / 2)
  in
  draw_centered_text
    context
    ~font
    ~fill:text_fill
    ~size:temperature_size
    ~baseline_y:(text_top + rendered_temperature.baseline_y)
    ~left:(center_x - radius)
    ~right:(center_x + radius)
    temperature_text;
  draw_centered_text
    context
    ~font
    ~fill:text_fill
    ~size:status_text_size
    ~baseline_y:
      (text_top
       + rendered_temperature.height
       + text_spacing
       + rendered_low_high.baseline_y)
    ~left:(center_x - radius)
    ~right:(center_x + radius)
    low_high_text;
  Option.iter rendered_uv ~f:(fun (uv_text, rendered_uv) ->
    draw_centered_text
      context
      ~font
      ~fill:text_fill
      ~size:status_text_size
      ~baseline_y:
        (text_top
         + rendered_temperature.height
         + text_spacing
         + rendered_low_high.height
         + text_spacing
         + rendered_uv.baseline_y)
      ~left:(center_x - radius)
      ~right:(center_x + radius)
      uv_text)
;;

let draw_cloud
      context
      ~fill
      ~graph_style
      ~zone
      ~padding
      ~center_x
      ~base_y
      (precipitation : Weather_info.Precipitation.t option)
  =
  let graph_height = 74 in
  let graph_width = 150 in
  let open Drawing.O in
  let rain, snow, thunderstorm =
    match precipitation with
    | None -> false, false, false
    | Some { precipitation = { kind; thunder }; _ } ->
      (match kind with
       | Rain -> true, false, thunder
       | Snow -> false, true, thunder
       | Rain_and_snow -> true, true, thunder)
  in
  let height = graph_height + (2 * padding) in
  let width = graph_width + (4 * padding) + 2 in
  let left = center_x - (width / 2) in
  let right = left + width in
  let top = base_y - height in
  rounded_polygon
    context
    ~radius:(height / 2)
    ~fill
    [ left, base_y; right, base_y; right, top; left, top ];
  List.iter
    [ left + padding, height / 3, padding
    ; center_x - (height / 3), height * 3 / 5, 0
    ; center_x + (height * 4 / 9), height / 2, 0
    ]
    ~f:(fun (x, radius, offset_y) ->
      let y = top + (radius / 2) + padding + offset_y in
      circle context ~fill ~center:(x, y) ~radius);
  circle
    context
    ~fill
    ~center:(right - padding, base_y - (height / 3) - (2 * padding))
    ~radius:(height / 3);
  Option.iter precipitation ~f:(fun precipitation ->
    let samples = precipitation.samples in
    match samples, List.last samples with
    | [], _ | _, None -> ()
    | first :: _, Some last when Time_ns.compare first.time last.time >= 0 -> ()
    | first :: _, Some last ->
      let graph_starts_at = first.time
      and graph_ends_at = last.time in
      let duration = Time_ns.diff graph_ends_at graph_starts_at |> Time_ns.Span.to_sec in
      let points =
        List.map samples ~f:(fun sample ->
          let x_frac =
            Time_ns.Span.to_sec (Time_ns.diff sample.time graph_starts_at) /. duration
          in
          let y_frac =
            Option.map sample.probability_frac ~f:(fun y_frac ->
              (* Keep small nonzero probabilities visibly above the x-axis. *)
              match Float.(y_frac > 0.) with
              | true -> Float.max 0.1 y_frac
              | false -> y_frac)
          in
          { Graph.Point.x_frac; y_frac })
      in
      let tick time =
        { Graph.Tick.position_frac =
            Time_ns.Span.to_sec (Time_ns.diff time graph_starts_at) /. duration
        ; label = Some (Time_ns_unix.format time "%H:%M" ~zone)
        }
      in
      let hourly_ticks =
        List.range 1 (Float.iround_up_exn (duration /. 3600.))
        |> List.map ~f:(fun hour ->
          { Graph.Tick.position_frac = Float.of_int hour *. 3600. /. duration
          ; label = None
          })
      in
      let labeled_tick_extra_length =
        graph_style.Graph.Style.labeled_tick_length - graph_style.tick_length
      in
      Graph.draw
        context
        ~bottom_center:(center_x, base_y - padding - labeled_tick_extra_length)
        ~size:(graph_width, graph_height)
        ~style:graph_style
        ~x_ticks:(tick graph_starts_at :: tick graph_ends_at :: hourly_ticks)
        ~y_ticks:
          [ { position_frac = 0.5; label = Some "50%" }
          ; { position_frac = 1.; label = Some "100%" }
          ]
        ~points);
  let symbol_top = base_y + 12 in
  (match rain with
   | false -> ()
   | true ->
     let drop_radius = 9 in
     let drop_spacing = (2 * drop_radius) + 2 in
     List.iter
       [ 1. /. 4., 0
       ; (1. /. 4.) +. (1. /. 12.), drop_spacing
       ; 2. /. 3., 0
       ; (2. /. 3.) +. (1. /. 12.), drop_spacing
       ]
       ~f:(fun (fraction_from_left, offset_y) ->
         let center_x =
           left + Float.iround_nearest_exn (fraction_from_left *. Float.of_int width)
         in
         let top = symbol_top + offset_y in
         let circle_center_y = top + (2 * drop_radius) + 1 in
         let shoulder_y = circle_center_y - (drop_radius / 2) in
         polygon
           context
           ~fill
           [ center_x, top
           ; center_x + drop_radius - 1, shoulder_y
           ; center_x - drop_radius + 1, shoulder_y
           ];
         circle context ~fill ~center:(center_x, circle_center_y) ~radius:drop_radius));
  (match thunderstorm with
   | false -> ()
   | true ->
     let lightning_width = 24
     and lightning_height = 69 in
     let half_width = lightning_width / 2 in
     let tip_offset = 3 * lightning_width / 8 in
     let waist_half_width = Int.max 1 (lightning_width / 24) in
     let upper_waist_y = (lightning_height - tip_offset) / 2 in
     let lower_waist_y = lightning_height - upper_waist_y in
     polygon
       context
       ~fill
       [ center_x + tip_offset, symbol_top
       ; center_x + waist_half_width, symbol_top + upper_waist_y
       ; center_x + half_width, symbol_top + upper_waist_y
       ; center_x - tip_offset, symbol_top + lightning_height
       ; center_x - waist_half_width, symbol_top + lower_waist_y
       ; center_x - half_width, symbol_top + lower_waist_y
       ]);
  (match snow with
   | false -> ()
   | true ->
     let snowflake_radius = 17 in
     let stroke = Stroke.create fill 4 in
     List.iter
       [ left + (width / 6), base_y + 30
       ; left + (5 * width / 12), base_y + 10
       ; left + (4 * width / 5), base_y + 15
       ]
       ~f:(fun (center_x, top) ->
         star
           context
           ~stroke
           ~radius:snowflake_radius
           ~center:(center_x, top + snowflake_radius)));
  center_x, base_y - (height / 2)
;;
