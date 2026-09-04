open! Core
open! Async

let draw_centered_text context ~font ~fill ~size ~baseline_y ~left ~right text =
  let rendered_text = Font.render_text font text ~size in
  Drawing.text
    context
    ~font
    ~fill
    ~origin_x:(left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
    ~baseline_y
    ~size
    text
;;

let availability_status
      context
      ~style
      ~title
      ~box_size
      ~(station : Citibike.Station.t)
      ~is_enabled
      ~f
      anchor
  =
  let upper_left, lower_right = Drawing.Anchor.resolve anchor ~size:box_size
  and usable_capacity =
    station.capacity - station.bikes_disabled - station.docks_disabled
  in
  let frac =
    match usable_capacity <= 0 with
    | true -> 0.
    | false -> Float.of_int station.bikes_available /. Float.of_int usable_capacity
  in
  Status_box.draw
    context
    upper_left
    lower_right
    ~style
    ~title
    ~fill:
      (match is_enabled with
       | true -> Drawing.Fill.fractional ~frac ~frontier_angle_degrees:15.
       | false -> fun _ -> Status_box.Style.error_fill style)
    ~f
;;

let available_bike_status
      context
      ~style
      ~title
      ~box_size
      ~(station : Citibike.Station.t)
      anchor
  =
  let font = Status_box.Style.font style in
  availability_status
    context
    ~style
    ~title
    ~box_size
    ~station
    ~is_enabled:station.is_renting
    ~f:(fun context ~fill ->
      let width, height = Drawing.Context.size context in
      let count_size = Status_box.Style.primary_font_size style
      and base_padding = Status_box.Style.base_padding style
      and horizontal_padding_between_text =
        Status_box.Style.horizontal_padding_between_text style
      in
      let baseline_y = height - Status_box.Style.baseline_padding style
      and left = base_padding
      and right = width - base_padding in
      match station.is_renting with
      | true ->
        let fill = Drawing.Fill.invert fill in
        let middle = (left + right) / 2 in
        let bikes_right = middle - (horizontal_padding_between_text / 2)
        and ebikes_left = middle + (horizontal_padding_between_text / 2) in
        let bikes_available = station.bikes_available - station.ebikes_available
        and ebikes_available = Int.to_string station.ebikes_available in
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left
          ~right:bikes_right
          (Int.to_string bikes_available);
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left:ebikes_left
          ~right
          ebikes_available;
        let ebike_label_size = 22. in
        let rendered_ebikes = Font.render_text font ebikes_available ~size:count_size
        and rendered_ebike_label = Font.render_text font "e" ~size:ebike_label_size in
        draw_centered_text
          context
          ~font
          ~fill
          ~size:ebike_label_size
          ~baseline_y:
            (baseline_y
             - rendered_ebikes.baseline_y
             - 5
             - rendered_ebike_label.height
             + rendered_ebike_label.baseline_y)
          ~left:ebikes_left
          ~right
          "e"
      | false -> ())
    anchor
;;

let parking_status context ~style ~title ~box_size ~(station : Citibike.Station.t) anchor =
  let font = Status_box.Style.font style in
  availability_status
    context
    ~style
    ~title
    ~box_size
    ~station
    ~is_enabled:station.is_returning
    ~f:(fun context ~fill ->
      match station.is_returning with
      | false -> ()
      | true ->
        let width, height = Drawing.Context.size context in
        let base_padding = Status_box.Style.base_padding style in
        draw_centered_text
          context
          ~font
          ~fill:(Drawing.Fill.invert fill)
          ~size:(Status_box.Style.primary_font_size style)
          ~baseline_y:(height - Status_box.Style.baseline_padding style)
          ~left:base_padding
          ~right:(width - base_padding)
          (Int.to_string station.docks_available))
    anchor
;;

module Draw_inputs = struct
  type t =
    { device_status : Renderer.Device_status.t
    ; weather : Weather.Summary.t
    ; bridge_station : Citibike.Station.t
    ; roebling_station : Citibike.Station.t
    ; vesey_station : Citibike.Station.t
    ; west_station : Citibike.Station.t
    ; barclay_station : Citibike.Station.t
    ; fulton_station : Citibike.Station.t
    ; bedford_status : Mta_subway.Stop_status.t
    ; marcy_status : Mta_subway.Stop_status.t
    ; now : Time_ns.t
    }
end

let day_night_phase weather ~at =
  let twilight = Time_ns.Span.of_min 15. in
  let time_of_day_frac time =
    Time_ns.to_ofday time ~zone:weather.Weather.Summary.zone
    |> Time_ns.Ofday.to_span_since_start_of_day
    |> Time_ns.Span.to_sec
    |> fun seconds -> seconds /. Time_ns.Span.to_sec Time_ns.Span.day
  in
  let sunrise = time_of_day_frac (Time_ns.sub weather.sunrise twilight)
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

let draw
      ~font
      ~device_status
      ~weather
      ~bridge_station
      ~roebling_station
      ~vesey_station
      ~west_station
      ~barclay_station
      ~fulton_station
      ~bedford_status
      ~marcy_status
      ~now
  =
  let open Drawing.O in
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
  and subway_status_width = Subway_status_box.width status_box_style in
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
  let l_fill : Fill.t = Fill.bayer_exn ~white_frac:(9. /. 16.)
  and j_fill : Fill.t = Fill.bayer_exn ~white_frac:(1. /. 16.)
  and m_fill : Fill.t = Fill.bayer_exn ~offset:(1, 1) ~white_frac:(10. /. 16.) in
  let bedford_rows =
    [ { Subway_status_box.Row.bullet = "L", l_fill
      ; route_ids = [ "L" ]
      ; minimum_minutes = 11
      ; westbound_mta_direction = "N"
      }
    ]
  and marcy_rows =
    [ { Subway_status_box.Row.bullet = "J", j_fill
      ; route_ids = [ "J"; "Z" ]
      ; minimum_minutes = 5
      ; westbound_mta_direction = "S"
      }
    ; { bullet = "M", m_fill
      ; route_ids = [ "M" ]
      ; minimum_minutes = 5
      ; westbound_mta_direction = "N"
      }
    ]
  in
  let bedford_status_height =
    Subway_status_box.height status_box_style ~row_count:(List.length bedford_rows)
  and marcy_status_height =
    Subway_status_box.height status_box_style ~row_count:(List.length marcy_rows)
  in
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
  let uv_forecast_ends_at = Time_ns.add now (Time_ns.Span.of_hr 8.) in
  let max_uv =
    Option.to_list weather.current_uv_index
    @ List.filter_map weather.uv_indices ~f:(fun (time, uv_index) ->
      match
        Time_ns.compare time now >= 0 && Time_ns.compare time uv_forecast_ends_at <= 0
      with
      | true -> Some uv_index
      | false -> None)
    |> List.max_elt ~compare:Float.compare
  in
  let temperature_text = fahrenheit_text weather.current_temperature_celsius
  and low_high_text =
    [ "l" ^ fahrenheit_text weather.low_temperature_celsius
    ; "h" ^ fahrenheit_text weather.high_temperature_celsius
    ]
    |> String.concat ~sep:"  "
  and temperature_size = 70.
  and text_spacing = 4 in
  let rendered_temperature = Font.render_text font temperature_text ~size:temperature_size
  and rendered_low_high = Font.render_text font low_high_text ~size:secondary_text_size
  and rendered_uv =
    Option.bind max_uv ~f:(fun uv ->
      match Float.compare uv 6. > 0 with
      | false -> None
      | true ->
        let text = "uv " ^ (uv |> Float.iround_nearest_exn |> Int.to_string) in
        Some (text, Font.render_text font text ~size:secondary_text_size))
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
  Subway_status_box.draw
    context
    ~anchor:(Anchor.Ur (subway_status_right, bedford_status_top))
    ~style:status_box_style
    ~title:"bedford"
    ~now
    ~stop_status:bedford_status
    ~rows:bedford_rows;
  Subway_status_box.draw
    context
    ~anchor:(Anchor.Ur (subway_status_right, marcy_status_top))
    ~style:status_box_style
    ~title:"marcy"
    ~now
    ~stop_status:marcy_status
    ~rows:marcy_rows;
  let bike_status_rx = subway_status_left - base_padding in
  available_bike_status
    context
    ~style:status_box_style
    ~title:"bridge"
    ~box_size:available_bike_status_size
    ~station:bridge_station
    (Anchor.Lr (bike_status_rx, bridge_status_bottom));
  available_bike_status
    context
    ~style:status_box_style
    ~title:"roeb"
    ~box_size:available_bike_status_size
    ~station:roebling_station
    (Anchor.Ur (bike_status_rx, j_y + subway_stroke_safe_padding + base_padding));
  parking_status
    context
    ~style:status_box_style
    ~title:"ves"
    ~box_size:parking_status_size
    ~station:vesey_station
    (Anchor.Ul (parking_grid_left, parking_grid_left_column_top));
  parking_status
    context
    ~style:status_box_style
    ~title:"west"
    ~box_size:parking_status_size
    ~station:west_station
    (Anchor.Ul
       ( parking_grid_left
       , parking_grid_left_column_top + parking_status_height + base_padding ));
  parking_status
    context
    ~style:status_box_style
    ~title:"barc"
    ~box_size:parking_status_size
    ~station:barclay_station
    (Anchor.Ul (parking_grid_right_column, parking_grid_top));
  parking_status
    context
    ~style:status_box_style
    ~title:"ful"
    ~box_size:parking_status_size
    ~station:fulton_station
    (Anchor.Ul
       (parking_grid_right_column, parking_grid_top + parking_status_height + base_padding));
  let voltage_text =
    match device_status.Renderer.Device_status.battery_voltage with
    | Some battery_voltage ->
      [%string "voltage %{Float.to_string_hum battery_voltage ~decimals:1}V"]
    | None -> "voltage unknown"
  and updated_text =
    [ "updated"
    ; Time_ns_unix.format
        now
        "%H:%M"
        ~zone:(Time_ns_unix.Zone.find_exn "America/New_York")
    ]
    |> String.concat ~sep:" "
  and status_text_padding = 8 in
  let status_text_fill = Fill.bayer_exn ~white_frac:0.5 in
  let draw_status_text ~origin_x string =
    let rendered_text = Font.render_text font string ~size:secondary_text_size in
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

type debug_preset = Dense_text_offset_time

let weather_coordinates = { Weather.Coordinates.latitude = 40.7128; longitude = -74.006 }
let query_weather cache = Weather.query cache ~coordinates:weather_coordinates

let live_draw_inputs cache ~device_status ~now =
  let%bind citibike_result = Citibike.query cache
  and mta_subway_status_result =
    Mta_subway.query
      cache
      ~which_feeds:[ Mta_subway.Realtime_feed.Line_L; Lines_J_Z; Lines_B_D_F_M ]
  and weather_result = query_weather cache in
  return
    (let%bind.Or_error citibike_stations =
       Latest_result.latest_success citibike_result
       |> Or_error.map ~f:(fun completed -> completed.value)
     and weather =
       Latest_result.latest_success weather_result
       |> Or_error.map ~f:(fun completed -> completed.value)
     and mta_subway_status = mta_subway_status_result in
     let find_station = Map.find_or_error citibike_stations in
     let%map.Or_error bridge_station = find_station "66dc8768-0aca-11e7-82f6-3863bb44ef7c"
     and roebling_station = find_station "66dced76-0aca-11e7-82f6-3863bb44ef7c"
     and vesey_station = find_station "66db8d89-0aca-11e7-82f6-3863bb44ef7c"
     and west_station = find_station "2170352212111402482"
     and barclay_station = find_station "66dbf73d-0aca-11e7-82f6-3863bb44ef7c"
     and fulton_station = find_station "66db79a3-0aca-11e7-82f6-3863bb44ef7c"
     and bedford_status = Map.find_or_error mta_subway_status.stop_status_by_stop_id "L08"
     and marcy_status =
       Map.find_or_error mta_subway_status.stop_status_by_stop_id "M16"
     in
     { Draw_inputs.device_status
     ; weather
     ; bridge_station
     ; roebling_station
     ; vesey_station
     ; west_station
     ; barclay_station
     ; fulton_station
     ; bedford_status
     ; marcy_status
     ; now
     })
;;

let render input cache =
  let now = Time_ns.now () in
  let%bind.Deferred.Or_error font =
    Font.create ~ttf_file:"server/fonts/inter_medium.ttf" |> return
  in
  let%bind.Deferred.Or_error draw_inputs =
    match input with
    | Renderer.Input.Device device_status -> live_draw_inputs cache ~device_status ~now
    | Preview None ->
      live_draw_inputs
        cache
        ~device_status:{ Renderer.Device_status.battery_voltage = Some 4.1 }
        ~now
    | Preview (Some Dense_text_offset_time) ->
      let now = Time_ns.add now (Time_ns.Span.of_hr 16.) in
      let%bind weather_result = query_weather cache in
      let%bind.Deferred.Or_error weather =
        Latest_result.latest_success weather_result
        |> Or_error.map ~f:(fun completed -> completed.value)
        |> return
      in
      let _, widest_two_digit_number =
        Font.max_width font [ `Number (12, 99) ] ~size:20.
      in
      let widest_two_digit_number = Int.of_string widest_two_digit_number in
      let station =
        { Citibike.Station.station_id = "dense"
        ; name = "dense"
        ; latitude = 0.
        ; longitude = 0.
        ; capacity = 3 * widest_two_digit_number
        ; bikes_available = 2 * widest_two_digit_number
        ; ebikes_available = widest_two_digit_number
        ; bikes_disabled = 0
        ; docks_available = widest_two_digit_number
        ; docks_disabled = 0
        ; is_installed = true
        ; is_renting = true
        ; is_returning = true
        ; last_reported = now
        }
      in
      let arrivals ~route_id ~stop_id =
        [ "N"; "S" ]
        |> List.concat_map ~f:(fun direction ->
          List.init 3 ~f:(fun _ ->
            { Mta_subway.Arrival.route_id
            ; trip_id = None
            ; stop_id = stop_id ^ direction
            ; arrives_at =
                Time_ns.add now (Time_ns.Span.of_int_min widest_two_digit_number)
            }))
      in
      let bedford_status =
        { Mta_subway.Stop_status.upcoming_arrivals = arrivals ~route_id:"L" ~stop_id:"L08"
        ; alerts = []
        }
      and marcy_status =
        { Mta_subway.Stop_status.upcoming_arrivals =
            arrivals ~route_id:"J" ~stop_id:"M16" @ arrivals ~route_id:"M" ~stop_id:"M16"
        ; alerts = []
        }
      in
      return
        (Ok
           { Draw_inputs.device_status = { battery_voltage = None }
           ; weather =
               { weather with
                 current_temperature_celsius = Some 40.
               ; current_uv_index = Some 10.
               }
           ; bridge_station = station
           ; roebling_station = station
           ; vesey_station = station
           ; west_station = station
           ; barclay_station = station
           ; fulton_station = station
           ; bedford_status
           ; marcy_status
           ; now
           })
  in
  let { Draw_inputs.device_status
      ; weather
      ; bridge_station
      ; roebling_station
      ; vesey_station
      ; west_station
      ; barclay_station
      ; fulton_station
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
      ~bridge_station
      ~roebling_station
      ~vesey_station
      ~west_station
      ~barclay_station
      ~fulton_station
      ~bedford_status
      ~marcy_status
      ~now
  in
  return (Ok { Renderer.Render.buffer; time_until_refresh = Time_ns.Span.of_sec 30. })
;;

let renderer =
  { Renderer.debug_presets = [ Dense_text_offset_time ]
  ; debug_preset_name = (fun Dense_text_offset_time -> "dense text + offset time")
  ; render
  }
;;
