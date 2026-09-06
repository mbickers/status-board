open! Core
open! Async

let max_width_message = String.make 14 'W'
let status_text_size = 31.
let font = lazy (Font.create ~ttf_file:"status_board/fonts/inter_medium.ttf")

let render_message message =
  let%bind.Or_error () =
    match String.for_all message ~f:(fun character -> Char.to_int character < 128) with
    | true -> Ok ()
    | false -> Or_error.error_string "messages must contain only ASCII characters"
  in
  let%bind.Or_error font = Lazy.force font in
  let rendered = Font.render_text font message ~size:status_text_size in
  let limit = Font.render_text font max_width_message ~size:status_text_size in
  match rendered.width <= limit.width with
  | true -> Ok rendered
  | false ->
    Or_error.errorf "message too wide (%d of %d pixel limit)" rendered.width limit.width
;;

let draw_centered_text context ~font ~fill ~size ~baseline_y ~left ~right text =
  let rendered_text = Font.render_text font text ~size in
  Drawing.blit_rendered_text
    context
    ~fill
    ~origin_x:(left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
    ~baseline_y
    rendered_text
;;

module Draw_inputs = struct
  type t =
    { device_status : Status_board.Device_status.t
    ; message : string option
    ; weather : Weather_info.t
    ; bridge_status : Citibike_status.t
    ; roebling_status : Citibike_status.t
    ; vesey_status : Citibike_status.t
    ; west_status : Citibike_status.t
    ; barclay_status : Citibike_status.t
    ; fulton_status : Citibike_status.t
    ; bedford_status : [ `J | `L | `M ] Subway_status.t
    ; marcy_status : [ `J | `L | `M ] Subway_status.t
    ; now : Time_ns.t
    }
end

let display_zone = Time_ns_unix.Zone.find_exn "America/New_York"

let day_night_phase weather ~at =
  let twilight = Time_ns.Span.of_min 15. in
  let time_of_day_frac time =
    Time_ns.to_ofday time ~zone:display_zone
    |> Time_ns.Ofday.to_span_since_start_of_day
    |> Time_ns.Span.to_sec
    |> fun seconds -> seconds /. Time_ns.Span.to_sec Time_ns.Span.day
  in
  let sunrise = time_of_day_frac (Time_ns.sub weather.Weather_info.sunrise twilight)
  and sunset = time_of_day_frac (Time_ns.add weather.sunset twilight)
  and at_frac = time_of_day_frac at in
  let is_night =
    Float.compare at_frac sunset >= 0 || Float.compare at_frac sunrise <= 0
  in
  let start, finish =
    match is_night with
    | true -> sunset, sunrise
    | false -> sunrise, sunset
  in
  let elapsed_since start time =
    let elapsed = time -. start in
    match Float.compare elapsed 0. >= 0 with
    | true -> elapsed
    | false -> elapsed +. 1.
  in
  is_night, elapsed_since start at_frac /. elapsed_since start finish
;;

let fahrenheit_text = function
  | None -> "??"
  | Some celsius ->
    (celsius *. 9. /. 5.) +. 32. |> Float.iround_nearest_exn |> Int.to_string
;;

let celsius_of_fahrenheit fahrenheit = (fahrenheit -. 32.) *. 5. /. 9.

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
      ~moon_phase
      ~center:((center_x, center_y) as center)
      ~radius
  =
  let open Drawing.O in
  let fill =
    match is_night, moon_phase with
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
  circle context ~fill ~center ~radius
;;

let draw_bird context ~wing_width ~center:(x, y) =
  let x = Float.of_int x
  and y = Float.of_int y in
  let wing_width = Float.of_int wing_width in
  let stroke = Drawing.Stroke.solid `b 2 in
  List.iter [ -1.; 1. ] ~f:(fun direction ->
    Drawing.draw_quadratic_curve
      context
      ~stroke
      ( (x, y)
      , (x +. (direction *. wing_width *. 0.4), y -. 8.)
      , (x +. (direction *. wing_width), y -. 6.) ))
;;

let draw_cloud
      context
      ~fill
      ~graph_style
      ~padding
      ~center_x
      ~base_y
      ~graph_height
      ~random
      ~diameter_frac_range:(min_diameter_frac, max_diameter_frac)
      ~outer_angle_range:(min_outer_angle, max_outer_angle)
      ~outer_diameter_frac
      (precipitation : Weather_info.Precipitation.t option)
  =
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
  let rectangle_width = graph_width + (2 * padding) in
  let rectangle_left = center_x - (rectangle_width / 2) in
  let rectangle_right = rectangle_left + rectangle_width in
  rounded_polygon
    context
    ~radius:25
    ~fill
    ~round_corner:(fun index -> index >= 2)
    [ rectangle_left, base_y
    ; rectangle_right, base_y
    ; rectangle_right, base_y - height
    ; rectangle_left, base_y - height
    ];
  let circles =
    List.concat_map
      [ -1., rectangle_left; 1., rectangle_right ]
      ~f:(fun (direction, side_x) ->
        let diameter_frac =
          Random.State.float_range random min_diameter_frac max_diameter_frac
        in
        let outer_angle =
          Random.State.float_range random min_outer_angle max_outer_angle
        in
        let radius = Float.of_int height *. diameter_frac /. 2. in
        let x = Float.of_int side_x in
        let y = Float.of_int base_y -. radius in
        let dx = direction *. Float.cos outer_angle in
        let dy = -.Float.sin outer_angle in
        let outer_radius = Float.of_int height *. outer_diameter_frac /. 2. in
        [ x, y, radius; x +. (radius *. dx), y +. (radius *. dy), outer_radius ])
    |> List.map ~f:(fun (x, y, radius) ->
      ( Float.iround_nearest_exn x
      , Float.iround_nearest_exn y
      , Float.iround_nearest_exn radius ))
  in
  let left, right, top =
    List.fold
      circles
      ~init:(rectangle_left, rectangle_right, base_y - height)
      ~f:(fun (left, right, top) (x, y, radius) ->
        circle context ~fill ~center:(x, y) ~radius;
        Int.min left (x - radius), Int.max right (x + radius), Int.min top (y - radius))
  in
  let width = right - left in
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
        ; label = Some (Time_ns_unix.format time "%H:%M" ~zone:display_zone)
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
  (left, top), (right, base_y)
;;

let draw ~font draw_inputs =
  let { Draw_inputs.device_status
      ; message
      ; weather
      ; bridge_status
      ; roebling_status
      ; vesey_status
      ; west_status
      ; barclay_status
      ; fulton_status
      ; bedford_status
      ; marcy_status
      ; now
      }
    =
    draw_inputs
  in
  let open Drawing.O in
  let base_padding = 8 in
  let screen_edge_padding = base_padding in
  let is_night, sun_moon_progress_frac = day_night_phase weather ~at:now in
  let base_color =
    match is_night with
    | true -> `b
    | false -> `w
  in
  let inverse_base_color =
    match base_color with
    | `b -> `w
    | `w -> `b
  in
  let label_size = 17. in
  let graph_style =
    let tick_length = 3 in
    { Graph.Style.font
    ; label_size
    ; label_fill = solid `b
    ; label_halo = Some (2, solid `w)
    ; stroke = Stroke.solid `b 2
    ; tick_length
    ; labeled_tick_length = tick_length + 2
    }
  in
  let status_box_style =
    Status_box.Style.create
      ~label_size
      ~font
      ~base_padding
      ~primary_font_size:40.
      ~error_fill:(Fill.bayer_exn ~white_frac:0.7)
  in
  let w = 800
  and h = 480 in
  let voltage_text =
    match device_status.Status_board.Device_status.battery_voltage with
    | Some battery_voltage ->
      [%string "voltage %{Float.to_string_hum battery_voltage ~decimals:1}V"]
    | None -> "voltage unknown"
  and updated_text =
    [ "updated"; Time_ns_unix.format now "%H:%M" ~zone:display_zone ]
    |> String.concat ~sep:" "
  in
  let status_text_padding = base_padding in
  let rendered_voltage = Font.render_text font voltage_text ~size:status_text_size
  and rendered_updated = Font.render_text font updated_text ~size:status_text_size
  and rendered_status_height = Font.render_text font "Ag" ~size:status_text_size in
  let status_text_baseline = status_text_padding + rendered_status_height.baseline_y in
  let%bind.Or_error message =
    match message with
    | None -> Ok None
    | Some message ->
      let message =
        String.map message ~f:(fun character ->
          match Char.is_whitespace character with
          | true -> ' '
          | false -> character)
        |> String.strip
      in
      (match String.is_empty message with
       | true -> Ok None
       | false -> render_message message |> Or_error.map ~f:Option.some)
  in
  let bitmap = Bitmap.create ~width:w ~height:h in
  let context = Context.create bitmap in
  let black = Fill.solid `b in
  let alt_fill : Fill.t = Fill.bayer_exn ~size:16 ~white_frac:0.79 in
  let moon_dark_fill : Fill.t = Fill.bayer_exn ~size:16 ~white_frac:(3. /. 8.) in
  let day_land_fill : Fill.t = Fill.bayer_exn ~size:16 ~white_frac:(254. /. 256.) in
  let land_fill =
    match is_night with
    | true -> alt_fill
    | false -> day_land_fill
  and geo_stroke = Stroke.solid `b 8 in
  rect context ~fill:(Fill.solid base_color) (0, 0) (w, h);
  let manhattan_w = 220
  and manhattan_inset = 43 in
  let maximum_citibike_count_width, _ =
    Font.max_width
      font
      [ `Number (0, 99) ]
      ~size:(Status_box.Style.primary_font_size status_box_style)
  in
  let maximum_citibike_count_width = Float.iround_up_exn maximum_citibike_count_width in
  let inter_subway_padding = 0
  and available_bike_status_size =
    ( (2 * base_padding)
      + Status_box.Style.horizontal_padding_between_text status_box_style
      + (2 * maximum_citibike_count_width)
    , 62 )
  and parking_status_size = 69, 52 in
  let available_bike_status_width, available_bike_status_height =
    available_bike_status_size
  and parking_status_width, parking_status_height = parking_status_size
  and subway_status_width = Subway_status.width status_box_style in
  let geo_stroke_safe_padding = Stroke.safe_padding geo_stroke in
  let manhattan_left = screen_edge_padding + geo_stroke_safe_padding
  and manhattan_bottom = h - screen_edge_padding - geo_stroke_safe_padding in
  let parking_grid_left = manhattan_left + geo_stroke_safe_padding + base_padding
  and parking_grid_bottom = manhattan_bottom - geo_stroke_safe_padding - base_padding in
  let parking_grid_top = parking_grid_bottom - (2 * parking_status_height) - base_padding
  and parking_grid_right_column =
    parking_grid_left + parking_status_width + base_padding
  in
  let parking_grid_left_column_top =
    parking_grid_top - ((parking_status_height + base_padding) / 2)
  in
  let parking_grid_right = parking_grid_right_column + parking_status_width in
  let l_fill : Drawing.Fill.t = Drawing.Fill.bayer_exn ~white_frac:(9. /. 16.)
  and j_fill : Drawing.Fill.t = Drawing.Fill.bayer_exn ~white_frac:(1. /. 16.)
  and m_fill : Drawing.Fill.t =
    Drawing.Fill.bayer_exn ~offset:(1, 1) ~white_frac:(10. /. 16.)
  in
  let route_fill = function
    | `L -> l_fill
    | `J -> j_fill
    | `M -> m_fill
  in
  let display_route_text = function
    | `L -> "L"
    | `J -> "J"
    | `M -> "M"
  in
  let bedford_status_height = Subway_status.height status_box_style bedford_status
  and marcy_status_height = Subway_status.height status_box_style marcy_status in
  let brooklyn_height =
    base_padding
    + screen_edge_padding
    + geo_stroke_safe_padding
    + bedford_status_height
    + base_padding
    + marcy_status_height
  in
  let brooklyn_top = h - brooklyn_height in
  let fade_out_height = 20 in
  let map_top = brooklyn_top in
  let map_faded_top = map_top - fade_out_height in
  let north_fade fill =
    fade_to
      ~color:base_color
      ~color_frac:(fun (_, y) ->
        1. -. (Float.of_int (y - map_faded_top) /. Float.of_int fade_out_height))
      fill
  in
  let subway_casing = Stroke.create (north_fade (Fill.solid `w)) 12 in
  let subway_stroke fill = Stroke.create ~casing:subway_casing fill 8 in
  let subway_stroke_safe_padding = Stroke.safe_padding (subway_stroke black) in
  let manhattan_corner_radius = 20 in
  let manhattan_top = map_faded_top - manhattan_corner_radius in
  let manhattan_path =
    Path_resolver_step.resolve
      [ Path_resolver_step.Point (manhattan_left, manhattan_top)
      ; Point (manhattan_left, manhattan_bottom - manhattan_inset)
      ; Offset (manhattan_inset, manhattan_inset)
      ; Point (manhattan_w - manhattan_inset, manhattan_bottom)
      ; Offset (manhattan_inset, -manhattan_inset)
      ; Point (manhattan_w, map_top + manhattan_inset + 20)
      ; Offset (-manhattan_inset, -manhattan_inset)
      ; Offset (0, -20)
      ; Point (manhattan_w - manhattan_inset, manhattan_top)
      ]
  in
  let subway_status_right = w - screen_edge_padding
  and bedford_status_top = brooklyn_top + geo_stroke_safe_padding + screen_edge_padding in
  let subway_status_left = subway_status_right - subway_status_width
  and bedford_status_bottom = bedford_status_top + bedford_status_height in
  let marcy_status_top = bedford_status_bottom + base_padding in
  let marcy_status_bottom = marcy_status_top + marcy_status_height in
  let j_y =
    marcy_status_bottom
    - available_bike_status_height
    - base_padding
    - subway_stroke_safe_padding
  and j_x = parking_grid_right + base_padding + subway_stroke_safe_padding - 3 in
  let m_y =
    j_y - subway_stroke_safe_padding - inter_subway_padding - subway_stroke_safe_padding
  and m_middle_vertical_x = (parking_grid_right + manhattan_w) / 2
  and m_vert_x =
    parking_grid_left + parking_status_width + base_padding + subway_stroke_safe_padding
  in
  let bridge_status_bottom = m_y - base_padding - subway_stroke_safe_padding in
  let bridge_status_top = bridge_status_bottom - available_bike_status_height in
  let l_y = bridge_status_top - base_padding - subway_stroke_safe_padding in
  let brooklyn_start =
    w
    - subway_status_width
    - available_bike_status_width
    - (3 * base_padding)
    - geo_stroke_safe_padding
  in
  let brooklyn_foot = 50
  and brooklyn_corner_radius = 20 in
  let brooklyn_path =
    [ w + brooklyn_corner_radius, brooklyn_top
    ; brooklyn_start, brooklyn_top
    ; brooklyn_start, h - brooklyn_foot
    ; brooklyn_start - brooklyn_foot - brooklyn_corner_radius, h + brooklyn_corner_radius
    ]
  in
  let water_fill (x, y) =
    let wave_x = (x + (y / 12 % 2 * 12)) % 24 in
    let distance_from_center = Int.abs (wave_x - 12) in
    match y % 12 = 5 - (distance_from_center * distance_from_center / 48) with
    | true -> inverse_base_color
    | false -> base_color
  in
  let faded_water_fill = north_fade water_fill in
  let sun_moon_radius = 69 in
  let sun_moon_left = screen_edge_padding + sun_moon_radius
  and sun_moon_right = w - screen_edge_padding - sun_moon_radius
  and sun_moon_peak_y = status_text_baseline + screen_edge_padding + sun_moon_radius
  and sun_moon_endpoint_y =
    Int.max
      (status_text_baseline + screen_edge_padding + sun_moon_radius)
      (map_faded_top - screen_edge_padding - sun_moon_radius)
  in
  let sun_moon_x =
    Float.of_int sun_moon_left
    +. (sun_moon_progress_frac *. Float.of_int (sun_moon_right - sun_moon_left))
  in
  let distance_from_peak = sun_moon_progress_frac -. 0.5 in
  let sun_moon_y =
    Float.of_int sun_moon_peak_y
    +. (4.
        *. Float.of_int (sun_moon_endpoint_y - sun_moon_peak_y)
        *. distance_from_peak
        *. distance_from_peak)
  in
  let sun_moon_center =
    Float.iround_nearest_exn sun_moon_x, Float.iround_nearest_exn sun_moon_y
  in
  let sun_moon_center_x, sun_moon_center_y = sun_moon_center in
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
    sun_moon_center_y
    - ((rendered_temperature.height + text_spacing + rendered_low_high.height + uv_height)
       / 2)
  in
  rect context ~fill:faded_water_fill (0, map_faded_top) (w, h);
  rounded_polygon
    context
    ~radius:manhattan_corner_radius
    ~fill:(north_fade land_fill)
    ~stroke:(Stroke.create (north_fade black) 8)
    manhattan_path;
  rounded_polygon
    context
    ~radius:brooklyn_corner_radius
    ~fill:land_fill
    ~stroke:geo_stroke
    (brooklyn_path @ [ w + brooklyn_corner_radius, h + brooklyn_corner_radius ]);
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke l_fill)
    [ ( manhattan_left
        + geo_stroke_safe_padding
        + base_padding
        + subway_stroke_safe_padding
      , l_y )
    ; w, l_y
    ];
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke j_fill)
    [ w, j_y
    ; j_x, j_y
    ; ( j_x
      , manhattan_bottom
        - 8
        - geo_stroke_safe_padding
        - base_padding
        - subway_stroke_safe_padding )
    ];
  let m_houston_y = parking_grid_top - base_padding - subway_stroke_safe_padding in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke (north_fade m_fill))
    [ w, m_y
    ; m_middle_vertical_x, m_y
    ; m_middle_vertical_x, m_houston_y
    ; m_vert_x, m_houston_y
    ; m_vert_x, map_faded_top
    ];
  Subway_status.draw
    context
    ~anchor:(Anchor.Ur (subway_status_right, bedford_status_top))
    ~style:status_box_style
    ~label:"bedford"
    ~display_route_text
    ~route_fill
    bedford_status;
  Subway_status.draw
    context
    ~anchor:(Anchor.Ur (subway_status_right, marcy_status_top))
    ~style:status_box_style
    ~label:"marcy"
    ~display_route_text
    ~route_fill
    marcy_status;
  let bike_status_rx = subway_status_left - base_padding in
  Citibike_status.draw_availability
    context
    ~anchor:(Anchor.Lr (bike_status_rx, bridge_status_bottom))
    ~style:status_box_style
    ~label:"bridge"
    ~box_size:available_bike_status_size
    bridge_status;
  Citibike_status.draw_availability
    context
    ~anchor:(Anchor.Ur (bike_status_rx, j_y + subway_stroke_safe_padding + base_padding))
    ~style:status_box_style
    ~label:"roeb"
    ~box_size:available_bike_status_size
    roebling_status;
  Citibike_status.draw_parking
    context
    ~anchor:(Anchor.Ul (parking_grid_left, parking_grid_left_column_top))
    ~style:status_box_style
    ~label:"ves"
    ~box_size:parking_status_size
    vesey_status;
  Citibike_status.draw_parking
    context
    ~anchor:
      (Anchor.Ul
         ( parking_grid_left
         , parking_grid_left_column_top + parking_status_height + base_padding ))
    ~style:status_box_style
    ~label:"west"
    ~box_size:parking_status_size
    west_status;
  Citibike_status.draw_parking
    context
    ~anchor:(Anchor.Ul (parking_grid_right_column, parking_grid_top))
    ~style:status_box_style
    ~label:"barc"
    ~box_size:parking_status_size
    barclay_status;
  Citibike_status.draw_parking
    context
    ~anchor:
      (Anchor.Ul
         ( parking_grid_right_column
         , parking_grid_top + parking_status_height + base_padding ))
    ~style:status_box_style
    ~label:"ful"
    ~box_size:parking_status_size
    fulton_status;
  draw_sun_moon
    context
    ~light_fill:alt_fill
    ~dark_fill:moon_dark_fill
    ~is_night
    ~moon_phase:weather.moon_phase
    ~center:sun_moon_center
    ~radius:sun_moon_radius;
  text
    context
    ~font
    ~fill:alt_fill
    ~origin_x:(status_text_padding + rendered_voltage.origin_x)
    ~baseline_y:status_text_baseline
    ~size:status_text_size
    voltage_text;
  text
    context
    ~font
    ~fill:alt_fill
    ~origin_x:
      (w - status_text_padding - rendered_updated.width + rendered_updated.origin_x)
    ~baseline_y:status_text_baseline
    ~size:status_text_size
    updated_text;
  Option.iter message ~f:(fun message ->
    blit_rendered_text
      context
      ~fill:(Fill.solid inverse_base_color)
      ~baseline_y:status_text_baseline
      ~origin_x:(((w - message.width) / 2) + message.origin_x)
      message);
  draw_centered_text
    context
    ~font
    ~fill:black
    ~size:temperature_size
    ~baseline_y:(text_top + rendered_temperature.baseline_y)
    ~left:(sun_moon_center_x - sun_moon_radius)
    ~right:(sun_moon_center_x + sun_moon_radius)
    temperature_text;
  draw_centered_text
    context
    ~font
    ~fill:black
    ~size:status_text_size
    ~baseline_y:
      (text_top
       + rendered_temperature.height
       + text_spacing
       + rendered_low_high.baseline_y)
    ~left:(sun_moon_center_x - sun_moon_radius)
    ~right:(sun_moon_center_x + sun_moon_radius)
    low_high_text;
  Option.iter rendered_uv ~f:(fun (uv_text, rendered_uv) ->
    draw_centered_text
      context
      ~font
      ~fill:black
      ~size:status_text_size
      ~baseline_y:
        (text_top
         + rendered_temperature.height
         + text_spacing
         + rendered_low_high.height
         + text_spacing
         + rendered_uv.baseline_y)
      ~left:(sun_moon_center_x - sun_moon_radius)
      ~right:(sun_moon_center_x + sun_moon_radius)
      uv_text);
  let farther_wall_x, sun_moon_near_side_x =
    match sun_moon_center_x < w / 2 with
    | true -> w, sun_moon_center_x + sun_moon_radius
    | false -> 0, sun_moon_center_x - sun_moon_radius
  in
  let random = Random.State.make [| Time_ns.hash now |] in
  let cloud_center_x = (sun_moon_near_side_x + farther_wall_x) / 2 in
  let cloud_base_y = h / 3 in
  let cloud_bounds =
    match weather.conditions with
    | Weather_info.Conditions.Not_cloudy -> None
    | Cloudy precipitation ->
      Some
        (draw_cloud
           context
           ~fill:alt_fill
           ~graph_style
           ~padding:base_padding
           ~center_x:cloud_center_x
           ~base_y:cloud_base_y
           ~graph_height:74
           ~random
           ~diameter_frac_range:(0.5, 0.7)
           ~outer_angle_range:(0., Float.pi /. 2.)
           ~outer_diameter_frac:0.5
           precipitation)
  in
  let choose_sky_spot ~radius ~offset:(offset_x, offset_y) ~index ~count =
    let padding = radius + 2 in
    let left = screen_edge_padding + padding in
    let right = w - screen_edge_padding - offset_x - padding in
    let top = status_text_baseline + screen_edge_padding + padding - Int.min 0 offset_y in
    let bottom = map_faded_top - screen_edge_padding - padding in
    let height = (bottom - top + 1) / count in
    let y = top + (index * height) + Random.State.int random height in
    let positions =
      List.range left (right + 1)
      |> List.filter ~f:(fun x ->
        let clears_center (center_x, center_y) =
          let dx =
            Int.max
              0
              (Int.max (x - padding - center_x) (center_x - x - offset_x - padding))
          in
          let dy =
            Int.max
              0
              (Int.max
                 (y + Int.min 0 offset_y - padding - center_y)
                 (center_y - y - Int.max 0 offset_y - padding))
          in
          (dx * dx) + (dy * dy) >= 70 * 70
        in
        clears_center sun_moon_center
        &&
        match cloud_bounds with
        | None -> true
        | Some ((left, top), (right, bottom)) ->
          x + offset_x + padding < left
          || x - padding > right
          || y + Int.max 0 offset_y + padding < top
          || y + Int.min 0 offset_y - padding > bottom)
    in
    let x = List.nth_exn positions (Random.State.int random (List.length positions)) in
    x, y
  in
  (match is_night with
   | true ->
     let radius = 3 in
     let stroke = Stroke.solid `w 2 in
     let count = 10 in
     List.iter (List.range 0 count) ~f:(fun index ->
       let center = choose_sky_spot ~radius ~offset:(0, 0) ~index ~count in
       star context ~stroke ~radius ~center)
   | false ->
     let wing_width = 10 in
     let spacing = 28 in
     let x, y =
       choose_sky_spot ~radius:wing_width ~offset:(spacing, -4) ~index:0 ~count:1
     in
     draw_bird context ~wing_width ~center:(x, y);
     draw_bird context ~wing_width ~center:(x + spacing, y - 4));
  Ok bitmap
;;

module Preset = struct
  type t =
    | Dense_text_night
    | Day_stormy
    | Errors_alerts
  [@@deriving enumerate]

  let to_string = function
    | Dense_text_night -> "dense text + night"
    | Day_stormy -> "day + stormy"
    | Errors_alerts -> "errors"
  ;;

  let of_string name =
    match List.find all ~f:(fun preset -> String.equal (to_string preset) name) with
    | Some preset -> Ok preset
    | None -> Or_error.errorf "Unknown debug preset %S" name
  ;;
end

let weather_coordinates =
  { Feeds.Weather.Coordinates.latitude = 40.7128; longitude = -74.006 }
;;

let query_weather cache = Feeds.Weather.query cache ~coordinates:weather_coordinates

let live_draw_inputs cache ~device_status ~message ~now =
  let bedford_rows =
    [ { Subway_status.Selection.display_route = `L
      ; route_ids = [ "L" ]
      ; minimum_minutes = 11
      ; westbound_mta_direction = Feeds.Mta_subway.Direction.North
      }
    ]
  and marcy_rows =
    [ { Subway_status.Selection.display_route = `J
      ; route_ids = [ "J"; "Z" ]
      ; minimum_minutes = 5
      ; westbound_mta_direction = Feeds.Mta_subway.Direction.South
      }
    ; { display_route = `M
      ; route_ids = [ "M" ]
      ; minimum_minutes = 5
      ; westbound_mta_direction = Feeds.Mta_subway.Direction.North
      }
    ]
  in
  let%bind citibike_result = Feeds.Citibike.query cache
  and mta_subway_status_result =
    Feeds.Mta_subway.query
      cache
      ~which_feeds:[ Feeds.Mta_subway.Realtime_feed.Line_L; Lines_J_Z; Lines_B_D_F_M ]
  and weather_result = query_weather cache in
  return
    (let%bind.Or_error citibike_stations =
       Feeds.Latest_result.latest_success citibike_result
       |> Or_error.map ~f:(fun completed -> completed.value)
     and forecast =
       Feeds.Latest_result.latest_success weather_result
       |> Or_error.map ~f:(fun completed -> fst completed.value)
     and mta_subway_status = mta_subway_status_result in
     let find_station = Map.find_or_error citibike_stations in
     let%map.Or_error weather = Weather_info.create ~look_forward_hours:8 ~now ~forecast
     and bridge_status =
       find_station "66dc8768-0aca-11e7-82f6-3863bb44ef7c"
       |> Or_error.map ~f:Citibike_status.create
     and roebling_status =
       find_station "66dced76-0aca-11e7-82f6-3863bb44ef7c"
       |> Or_error.map ~f:Citibike_status.create
     and vesey_status =
       find_station "66db8d89-0aca-11e7-82f6-3863bb44ef7c"
       |> Or_error.map ~f:Citibike_status.create
     and west_status =
       find_station "2170352212111402482" |> Or_error.map ~f:Citibike_status.create
     and barclay_status =
       find_station "66dbf73d-0aca-11e7-82f6-3863bb44ef7c"
       |> Or_error.map ~f:Citibike_status.create
     and fulton_status =
       find_station "66db79a3-0aca-11e7-82f6-3863bb44ef7c"
       |> Or_error.map ~f:Citibike_status.create
     and bedford_status =
       Subway_status.create mta_subway_status ~now ~station_id:"L08" ~rows:bedford_rows
     and marcy_status =
       Subway_status.create mta_subway_status ~now ~station_id:"M16" ~rows:marcy_rows
     in
     { Draw_inputs.device_status
     ; message
     ; weather
     ; bridge_status
     ; roebling_status
     ; vesey_status
     ; west_status
     ; barclay_status
     ; fulton_status
     ; bedford_status
     ; marcy_status
     ; now
     })
;;

let preset_draw_inputs ~font ~message ~now ~weather =
  let _, widest_two_digit_number = Font.max_width font [ `Number (12, 99) ] ~size:20. in
  let widest_two_digit_number = Int.of_string widest_two_digit_number in
  let citibike_status =
    { Citibike_status.availability =
        Citibike_status.Availability.Renting
          { classic_bikes_available = widest_two_digit_number
          ; electric_bikes_available = widest_two_digit_number
          }
    ; parking = Accepting_returns { docks_available = widest_two_digit_number }
    ; bikes_available_frac = 2. /. 3.
    }
  in
  let row display_route =
    let minutes = List.init 3 ~f:(fun _ -> widest_two_digit_number) in
    { Subway_status.Row.display_route
    ; westbound_minutes = minutes
    ; eastbound_minutes = minutes
    }
  in
  let bedford_status = { Subway_status.rows = [ row `L ]; has_alert = false }
  and marcy_status = { Subway_status.rows = [ row `J; row `M ]; has_alert = false } in
  { Draw_inputs.device_status = { battery_voltage = None }
  ; message
  ; weather
  ; bridge_status = citibike_status
  ; roebling_status = citibike_status
  ; vesey_status = citibike_status
  ; west_status = citibike_status
  ; barclay_status = citibike_status
  ; fulton_status = citibike_status
  ; bedford_status
  ; marcy_status
  ; now
  }
;;

let render input cache ~message =
  let now = Time_ns.now () in
  let%bind.Deferred.Or_error font = Lazy.force font |> return in
  let%bind.Deferred.Or_error draw_inputs =
    match input with
    | Status_board.Input.Device device_status ->
      live_draw_inputs cache ~device_status ~message ~now
    | Preview None ->
      live_draw_inputs
        cache
        ~message
        ~device_status:{ Status_board.Device_status.battery_voltage = Some 4.1 }
        ~now
    | Preview (Some preset) ->
      let%bind.Deferred.Or_error preset = return (Preset.of_string preset) in
      (match preset with
       | Preset.Dense_text_night ->
         let now =
           Time_ns.occurrence
             `First_after_or_at
             now
             ~ofday:(Time_ns.Ofday.create ~hr:22 ())
             ~zone:display_zone
         in
         let weather =
           { Weather_info.current_temperature_celsius = Some (celsius_of_fahrenheit 104.)
           ; low_temperature_celsius = Some (celsius_of_fahrenheit 99.)
           ; high_temperature_celsius = Some (celsius_of_fahrenheit 109.)
           ; maximum_uv_index = Some 10.
           ; conditions = Weather_info.Conditions.Not_cloudy
           ; moon_phase = Some 0.7
           ; sunrise =
               Time_ns.occurrence
                 `First_after_or_at
                 now
                 ~ofday:(Time_ns.Ofday.create ~hr:6 ())
                 ~zone:display_zone
           ; sunset =
               Time_ns.occurrence
                 `First_after_or_at
                 now
                 ~ofday:(Time_ns.Ofday.create ~hr:20 ())
                 ~zone:display_zone
           }
         in
         return
           (Ok (preset_draw_inputs ~font ~message:(Some max_width_message) ~now ~weather))
       | Day_stormy ->
         let hour_start =
           Time_ns.occurrence
             `First_after_or_at
             now
             ~ofday:(Time_ns.Ofday.create ~hr:15 ())
             ~zone:display_zone
         in
         let now = Time_ns.add hour_start (Time_ns.Span.of_min 30.) in
         let hourly =
           List.init 10 ~f:(fun hour ->
             let precipitation =
               Some { Feeds.Weather.Precipitation.kind = Rain_and_snow; thunder = true }
             in
             let preceding_hour_precipitation_probability =
               match hour with
               | 3 -> None
               | _ -> Some (100 - (Int.abs (4 - hour) * 20))
             in
             { Feeds.Weather.Hourly.time =
                 Time_ns.add hour_start (Time_ns.Span.of_int_hr hour)
             ; temperature_2m = Some (celsius_of_fahrenheit 32.)
             ; preceding_hour_precipitation_probability
             ; conditions = Some (Cloudy precipitation)
             ; uv_index = None
             })
         in
         let forecast =
           { Feeds.Weather.Forecast.timezone = "America/New_York"
           ; current =
               { time = now
               ; interval_seconds = 900
               ; temperature_2m = Some (celsius_of_fahrenheit 32.)
               ; conditions = None
               ; uv_index = None
               }
           ; hourly
           ; daily =
               [ { date = Time_ns.to_date now ~zone:display_zone
                 ; sunrise =
                     Time_ns.occurrence
                       `First_after_or_at
                       now
                       ~ofday:(Time_ns.Ofday.create ~hr:6 ())
                       ~zone:display_zone
                     |> Option.some
                 ; sunset =
                     Time_ns.occurrence
                       `First_after_or_at
                       now
                       ~ofday:(Time_ns.Ofday.create ~hr:20 ())
                       ~zone:display_zone
                     |> Option.some
                 ; moon_phase = Some 0.7
                 }
               ]
           }
         in
         let weather = Weather_info.create ~look_forward_hours:8 ~now ~forecast in
         return
           (Or_error.map weather ~f:(fun weather ->
              preset_draw_inputs ~font ~message ~now ~weather))
       | Errors_alerts ->
         let weather =
           { Weather_info.current_temperature_celsius = None
           ; low_temperature_celsius = None
           ; high_temperature_celsius = None
           ; maximum_uv_index = None
           ; conditions = Weather_info.Conditions.Not_cloudy
           ; moon_phase = None
           ; sunrise =
               Time_ns.occurrence
                 `First_after_or_at
                 now
                 ~ofday:(Time_ns.Ofday.create ~hr:6 ())
                 ~zone:display_zone
           ; sunset =
               Time_ns.occurrence
                 `First_after_or_at
                 now
                 ~ofday:(Time_ns.Ofday.create ~hr:20 ())
                 ~zone:display_zone
           }
         in
         let citibike_status =
           { Citibike_status.availability = Not_renting
           ; parking = Not_accepting_returns
           ; bikes_available_frac = 0.
           }
         in
         let row display_route =
           { Subway_status.Row.display_route
           ; westbound_minutes = []
           ; eastbound_minutes = []
           }
         in
         return
           (Ok
              { Draw_inputs.device_status = { battery_voltage = None }
              ; message
              ; weather
              ; bridge_status = citibike_status
              ; roebling_status = citibike_status
              ; vesey_status = citibike_status
              ; west_status = citibike_status
              ; barclay_status = citibike_status
              ; fulton_status = citibike_status
              ; bedford_status = { Subway_status.rows = [ row `L ]; has_alert = true }
              ; marcy_status =
                  { Subway_status.rows = [ row `J; row `M ]; has_alert = true }
              ; now
              }))
  in
  return (draw ~font draw_inputs)
;;

let status_board =
  { Status_board.refresh_interval = Time_ns.Span.of_sec 30.
  ; debug_presets = List.map Preset.all ~f:Preset.to_string
  ; render
  }
;;
