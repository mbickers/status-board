open! Core
open! Async

let draw_centered_text context ~font ~fill ~size ~baseline_y ~left ~right text =
  let rendered_text = Graphics.Font.render_text font text ~size in
  Graphics.Drawing.text
    context
    ~font
    ~fill
    ~origin_x:(left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
    ~baseline_y
    ~size
    text
;;

module Draw_inputs = struct
  type t =
    { device_status : Status_board.Device_status.t
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
  | None -> "--"
  | Some celsius ->
    (celsius *. 9. /. 5.) +. 32. |> Float.iround_nearest_exn |> Int.to_string
;;

let celsius_of_fahrenheit fahrenheit = (fahrenheit -. 32.) *. 5. /. 9.

let draw
      ~font
      ~device_status
      ~weather
      ~bridge_status
      ~roebling_status
      ~vesey_status
      ~west_status
      ~barclay_status
      ~fulton_status
      ~bedford_status
      ~marcy_status
      ~now
  =
  let open Graphics.Drawing.O in
  let base_padding = 8 in
  let screen_edge_padding = base_padding in
  let secondary_text_size = 31. in
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
  let status_box_style =
    Status_box.Style.create
      ~font
      ~base_padding
      ~primary_font_size:40.
      ~error_fill:(Fill.bayer_exn ~white_frac:0.7)
  in
  (* Short geometry variable names keep the function legible. *)
  let w = 800
  and h = 480 in
  let image = Image.create_grey ~max_val:1 w h in
  let context = Context.create image in
  let black = Fill.solid `b
  and land_fill =
    Fill.bayer_exn
      ~size:16
      ~white_frac:
        (match is_night with
         | true -> 0.8
         | false -> 254. /. 256.)
  and geo_stroke = Stroke.solid `b 8 in
  rect context ~fill:(Fill.solid base_color) (0, 0) (w, h);
  let manhattan_w = 220
  and manhattan_inset = 43 in
  let maximum_citibike_count_width, _ =
    Graphics.Font.max_width
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
  let l_fill : Graphics.Drawing.Fill.t =
    Graphics.Drawing.Fill.bayer_exn ~white_frac:(9. /. 16.)
  and j_fill : Graphics.Drawing.Fill.t =
    Graphics.Drawing.Fill.bayer_exn ~white_frac:(1. /. 16.)
  and m_fill : Graphics.Drawing.Fill.t =
    Graphics.Drawing.Fill.bayer_exn ~offset:(1, 1) ~white_frac:(10. /. 16.)
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
  let manhattan_path =
    Path_resolver_step.resolve
      [ Path_resolver_step.Point (manhattan_left, map_faded_top)
      ; Point (manhattan_left, manhattan_bottom - manhattan_inset)
      ; Offset (manhattan_inset, manhattan_inset)
      ; Point (manhattan_w - manhattan_inset, manhattan_bottom)
      ; Offset (manhattan_inset, -manhattan_inset)
      ; Point (manhattan_w, map_top + manhattan_inset + 20)
      ; Offset (-manhattan_inset, -manhattan_inset)
      ; Offset (0, -20)
      ; Point (manhattan_w - manhattan_inset, map_faded_top)
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
    [ w, brooklyn_top
    ; brooklyn_start, brooklyn_top
    ; brooklyn_start, h - brooklyn_foot
    ; brooklyn_start - brooklyn_foot, h
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
  let brooklyn_fill (x, y) =
    let x_distance = x - (brooklyn_start + brooklyn_corner_radius)
    and y_distance = y - (brooklyn_top + brooklyn_corner_radius) in
    match
      x < brooklyn_start + brooklyn_corner_radius
      && y < brooklyn_top + brooklyn_corner_radius
      && (x_distance * x_distance) + (y_distance * y_distance)
         > brooklyn_corner_radius * brooklyn_corner_radius
    with
    | true -> faded_water_fill (x, y)
    | false -> land_fill (x, y)
  in
  let sun_moon_radius = 69 in
  let sun_moon_left = screen_edge_padding + sun_moon_radius
  and sun_moon_right = w - screen_edge_padding - sun_moon_radius
  and sun_moon_peak_y = screen_edge_padding + sun_moon_radius
  and sun_moon_endpoint_y = map_faded_top - screen_edge_padding - sun_moon_radius in
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
  circle
    context
    ~fill:(Fill.bayer_exn ~size:16 ~white_frac:0.79)
    ~center:sun_moon_center
    ~radius:sun_moon_radius;
  let sun_moon_center_x, sun_moon_center_y = sun_moon_center in
  let temperature_text = fahrenheit_text weather.current_temperature_celsius
  and low_high_text =
    [ "l" ^ fahrenheit_text weather.low_temperature_celsius
    ; "h" ^ fahrenheit_text weather.high_temperature_celsius
    ]
    |> String.concat ~sep:"  "
  and temperature_size = 70.
  and text_spacing = 4 in
  let rendered_temperature =
    Graphics.Font.render_text font temperature_text ~size:temperature_size
  and rendered_low_high =
    Graphics.Font.render_text font low_high_text ~size:secondary_text_size
  and rendered_uv =
    Option.bind weather.maximum_uv_index ~f:(fun uv ->
      match Float.compare uv 6. > 0 with
      | false -> None
      | true ->
        let text = "uv " ^ (uv |> Float.iround_nearest_exn |> Int.to_string) in
        Some (text, Graphics.Font.render_text font text ~size:secondary_text_size))
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
    ~size:secondary_text_size
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
      ~size:secondary_text_size
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
  rect context ~fill:faded_water_fill (0, map_faded_top) (w, h);
  polygon context ~fill:(north_fade land_fill) manhattan_path;
  polygon context ~fill:brooklyn_fill (brooklyn_path @ [ w, h ]);
  rounded_path
    context
    ~radius:20
    ~stroke:(Stroke.create (north_fade black) 8)
    manhattan_path;
  rounded_path context ~radius:brooklyn_corner_radius ~stroke:geo_stroke brooklyn_path;
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
    ~title:"bedford"
    ~display_route_text
    ~route_fill
    bedford_status;
  Subway_status.draw
    context
    ~anchor:(Anchor.Ur (subway_status_right, marcy_status_top))
    ~style:status_box_style
    ~title:"marcy"
    ~display_route_text
    ~route_fill
    marcy_status;
  let bike_status_rx = subway_status_left - base_padding in
  Citibike_status.draw_availability
    context
    ~anchor:(Anchor.Lr (bike_status_rx, bridge_status_bottom))
    ~style:status_box_style
    ~title:"bridge"
    ~box_size:available_bike_status_size
    bridge_status;
  Citibike_status.draw_availability
    context
    ~anchor:(Anchor.Ur (bike_status_rx, j_y + subway_stroke_safe_padding + base_padding))
    ~style:status_box_style
    ~title:"roeb"
    ~box_size:available_bike_status_size
    roebling_status;
  Citibike_status.draw_parking
    context
    ~anchor:(Anchor.Ul (parking_grid_left, parking_grid_left_column_top))
    ~style:status_box_style
    ~title:"ves"
    ~box_size:parking_status_size
    vesey_status;
  Citibike_status.draw_parking
    context
    ~anchor:
      (Anchor.Ul
         ( parking_grid_left
         , parking_grid_left_column_top + parking_status_height + base_padding ))
    ~style:status_box_style
    ~title:"west"
    ~box_size:parking_status_size
    west_status;
  Citibike_status.draw_parking
    context
    ~anchor:(Anchor.Ul (parking_grid_right_column, parking_grid_top))
    ~style:status_box_style
    ~title:"barc"
    ~box_size:parking_status_size
    barclay_status;
  Citibike_status.draw_parking
    context
    ~anchor:
      (Anchor.Ul
         ( parking_grid_right_column
         , parking_grid_top + parking_status_height + base_padding ))
    ~style:status_box_style
    ~title:"ful"
    ~box_size:parking_status_size
    fulton_status;
  let voltage_text =
    match device_status.Status_board.Device_status.battery_voltage with
    | Some battery_voltage ->
      [%string "voltage %{Float.to_string_hum battery_voltage ~decimals:1}V"]
    | None -> "voltage unknown"
  and updated_text =
    [ "updated"; Time_ns_unix.format now "%H:%M" ~zone:display_zone ]
    |> String.concat ~sep:" "
  and status_text_padding = 8 in
  let status_text_fill = Fill.bayer_exn ~white_frac:0.5 in
  let draw_status_text ~origin_x string =
    let rendered_text = Graphics.Font.render_text font string ~size:secondary_text_size in
    text
      context
      ~font
      ~fill:status_text_fill
      ~origin_x:(origin_x rendered_text)
      ~baseline_y:(status_text_padding + rendered_text.baseline_y)
      ~size:secondary_text_size
      string
  in
  draw_status_text voltage_text ~origin_x:(fun rendered_text ->
    status_text_padding + rendered_text.origin_x);
  draw_status_text updated_text ~origin_x:(fun rendered_text ->
    w - status_text_padding - rendered_text.width + rendered_text.origin_x);
  image
;;

let dense_text_night_preset = "dense text + night"

let weather_coordinates =
  { Feeds.Weather.Coordinates.latitude = 40.7128; longitude = -74.006 }
;;

let query_weather cache = Feeds.Weather.query cache ~coordinates:weather_coordinates

let live_draw_inputs cache ~device_status ~now =
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
     let%map.Or_error weather = Weather_info.create ~look_forward_hours:24 ~now ~forecast
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

let render input cache =
  let now = Time_ns.now () in
  let%bind.Deferred.Or_error font =
    Graphics.Font.create ~ttf_file:"server/fonts/inter_medium.ttf" |> return
  in
  let%bind.Deferred.Or_error draw_inputs =
    match input with
    | Status_board.Input.Device device_status ->
      live_draw_inputs cache ~device_status ~now
    | Preview None ->
      live_draw_inputs
        cache
        ~device_status:{ Status_board.Device_status.battery_voltage = Some 4.1 }
        ~now
    | Preview (Some preset) when String.equal preset dense_text_night_preset ->
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
      let _, widest_two_digit_number =
        Graphics.Font.max_width font [ `Number (12, 99) ] ~size:20.
      in
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
      let bedford_status = { Subway_status.rows = [ row `L ] }
      and marcy_status = { Subway_status.rows = [ row `J; row `M ] } in
      return
        (Ok
           { Draw_inputs.device_status = { battery_voltage = None }
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
           })
    | Preview (Some preset) -> Deferred.Or_error.errorf "Unknown debug preset %S" preset
  in
  let { Draw_inputs.device_status
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
  let buffer =
    draw
      ~font
      ~device_status
      ~weather
      ~bridge_status
      ~roebling_status
      ~vesey_status
      ~west_status
      ~barclay_status
      ~fulton_status
      ~bedford_status
      ~marcy_status
      ~now
  in
  return (Ok buffer)
;;

let status_board =
  { Status_board.refresh_interval = Time_ns.Span.of_sec 30.
  ; debug_presets = [ dense_text_night_preset ]
  ; render
  }
;;
