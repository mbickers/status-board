open! Core
open! Async

let max_width_message = String.make 14 'W'
let status_text_size = 31.
let font = lazy (Drawing.Font.create ~ttf_file:"status_board/fonts/inter_medium.ttf")

let render_message ~fill message =
  let%bind.Or_error () =
    match String.for_all message ~f:(fun character -> Char.to_int character < 128) with
    | true -> Ok ()
    | false -> Or_error.error_string "messages must contain only ASCII characters"
  in
  let%bind.Or_error font = Lazy.force font in
  let element = Drawing.Primitives.text ~font ~fill message ~size:status_text_size in
  let limit =
    Drawing.Primitives.text ~font ~fill max_width_message ~size:status_text_size
  in
  let width, _ = Drawing.Element.size element
  and max_width, _ = Drawing.Element.size limit in
  match width <= max_width with
  | true -> Ok element
  | false -> Or_error.errorf "message too wide (%d of %d pixel limit)" width max_width
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
    ; bedford_status : Subway.Status.t
    ; marcy_status : Subway.Status.t
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

let draw_bird context ~wing_width ~center:(x, y) =
  let x = Float.of_int x
  and y = Float.of_int y in
  let wing_width = Float.of_int wing_width in
  let stroke = Drawing.Primitives.Stroke.solid `b 2 in
  List.iter [ -1.; 1. ] ~f:(fun direction ->
    Drawing.Primitives.draw_quadratic_curve
      context
      ~stroke
      ( (x, y)
      , (x +. (direction *. wing_width *. 0.4), y -. 8.)
      , (x +. (direction *. wing_width), y -. 6.) ))
;;

let battery_percentage ~voltage =
  let breakpoint_percentage = 11.25 in
  let percentage =
    match Float.compare voltage 3.4 <= 0 with
    | true -> breakpoint_percentage *. (voltage -. 2.7) /. 0.7
    | false ->
      breakpoint_percentage +. ((100. -. breakpoint_percentage) *. (voltage -. 3.4) /. 0.7)
  in
  Float.min 100. (Float.max 0. percentage)
;;

let draw
      ~font
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
      }
  =
  let open Drawing.O in
  let w = 800
  and h = 480 in
  let bitmap = Drawing.Bitmap.create ~width:w ~height:h in
  let context = Context.create bitmap in
  let is_night, sun_moon_progress_frac = day_night_phase weather ~at:now in
  (* define fills together to make editing easier *)
  let background_color =
    match is_night with
    | true -> `b
    | false -> `w
  in
  let inverse_background_color =
    match background_color with
    | `b -> `w
    | `w -> `b
  in
  let light_fill = bayer_exn ~size:16 ~white_frac:0.79 in
  let moon_dark_fill = bayer_exn ~size:16 ~white_frac:(3. /. 8.) in
  let device_status_text_fill =
    match is_night with
    | true -> invert (light_fill ?offset:None)
    | false -> light_fill ?offset:None
  in
  let cloud_fill =
    match is_night with
    | true -> moon_dark_fill
    | false -> light_fill
  in
  let land_fill =
    match is_night with
    | true -> moon_dark_fill
    | false -> bayer_exn ~size:16 ~white_frac:(254. /. 256.)
  in
  (* define shared sizing together to make editing easier *)
  let base_padding = 8 in
  let screen_edge_padding = base_padding in
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
      ~error_fill:(bayer_exn ~white_frac:0.7)
  in
  let battery_text =
    match device_status.battery_voltage with
    | Some battery_voltage ->
      let percentage = battery_percentage ~voltage:battery_voltage in
      [%string "%{Float.to_string_hum percentage ~decimals:0}%"]
    | None -> "??%"
  and updated_text =
    [ "updated"; Time_ns_unix.format now "%H:%M" ~zone:display_zone ]
    |> String.concat ~sep:" "
  in
  let status_text_padding = base_padding in
  let rendered_battery =
    text ~font ~fill:device_status_text_fill battery_text ~size:status_text_size
  and rendered_updated =
    text ~font ~fill:device_status_text_fill updated_text ~size:status_text_size
  and rendered_status_height =
    Drawing.Font.render_text font "Ag" ~size:status_text_size
  in
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
       | false ->
         render_message ~fill:(solid inverse_background_color) message
         |> Or_error.map ~f:Option.some)
  in
  let geo_stroke = Stroke.solid `b 8 in
  rect context ~fill:(solid background_color) (0, 0) (w, h);
  let manhattan_w = 220
  and manhattan_inset = 43 in
  let inter_subway_padding = 0 in
  let bedford_status =
    Subway.Status.element ~style:status_box_style ~label:"bedford" bedford_status
  and marcy_status =
    Subway.Status.element ~style:status_box_style ~label:"marcy" marcy_status
  and bridge_status =
    Citibike_status.availability ~style:status_box_style ~label:"bridge" bridge_status
  and roebling_status =
    Citibike_status.availability ~style:status_box_style ~label:"roeb" roebling_status
  and vesey_status =
    Citibike_status.parking ~style:status_box_style ~label:"ves" vesey_status
  and west_status =
    Citibike_status.parking ~style:status_box_style ~label:"west" west_status
  and barclay_status =
    Citibike_status.parking ~style:status_box_style ~label:"barc" barclay_status
  and fulton_status =
    Citibike_status.parking ~style:status_box_style ~label:"ful" fulton_status
  in
  let available_bike_status_width, available_bike_status_height =
    Element.size bridge_status
  and parking_status_width, parking_status_height = Element.size vesey_status
  and subway_status_width, bedford_status_height = Element.size bedford_status
  and _, marcy_status_height = Element.size marcy_status in
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
      ~color:background_color
      ~color_frac:(fun (_, y) ->
        1. -. (Float.of_int (y - map_faded_top) /. Float.of_int fade_out_height))
      fill
  in
  let subway_casing_fill = north_fade (solid background_color) in
  let manhattan_corner_radius = 20 in
  let manhattan_top = map_faded_top - manhattan_corner_radius in
  let manhattan_path =
    Path_resolver.resolve
      [ Point (manhattan_left, manhattan_top)
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
    - Subway.stroke_safe_padding
  and j_x = parking_grid_right + base_padding + Subway.stroke_safe_padding - 3 in
  let m_y =
    j_y - Subway.stroke_safe_padding - inter_subway_padding - Subway.stroke_safe_padding
  and m_middle_vertical_x = (parking_grid_right + manhattan_w) / 2
  and m_vert_x =
    parking_grid_left + parking_status_width + base_padding + Subway.stroke_safe_padding
  in
  let bridge_status_bottom = m_y - base_padding - Subway.stroke_safe_padding in
  let bridge_status_top = bridge_status_bottom - available_bike_status_height in
  let l_y = bridge_status_top - base_padding - Subway.stroke_safe_padding in
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
    | true -> inverse_background_color
    | false -> background_color
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
  let sun_moon_center_x = fst sun_moon_center in
  rect context ~fill:faded_water_fill (0, map_faded_top) (w, h);
  rounded_polygon
    context
    ~radius:manhattan_corner_radius
    ~fill:(north_fade land_fill)
    ~stroke:(Stroke.create (north_fade (solid `b)) 8)
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
    ~stroke:(Subway.stroke ~casing_fill:subway_casing_fill (Subway.Line.fill L))
    [ ( manhattan_left
        + geo_stroke_safe_padding
        + base_padding
        + Subway.stroke_safe_padding
      , l_y )
    ; w, l_y
    ];
  rounded_path
    context
    ~radius:20
    ~stroke:
      (Subway.stroke
         ~casing_fill:subway_casing_fill
         (Subway.Line.fill J_and_Z_but_call_it_J))
    [ w, j_y
    ; j_x, j_y
    ; ( j_x
      , manhattan_bottom
        - 8
        - geo_stroke_safe_padding
        - base_padding
        - Subway.stroke_safe_padding )
    ];
  let m_houston_y = parking_grid_top - base_padding - Subway.stroke_safe_padding in
  rounded_path
    context
    ~radius:20
    ~stroke:
      (Subway.stroke ~casing_fill:subway_casing_fill (north_fade (Subway.Line.fill M)))
    [ w, m_y
    ; m_middle_vertical_x, m_y
    ; m_middle_vertical_x, m_houston_y
    ; m_vert_x, m_houston_y
    ; m_vert_x, map_faded_top
    ];
  Element.draw
    bedford_status
    context
    ((Right, subway_status_right), (Top, bedford_status_top));
  Element.draw marcy_status context ((Right, subway_status_right), (Top, marcy_status_top));
  let bike_status_rx = subway_status_left - base_padding in
  Element.draw
    bridge_status
    context
    ((Right, bike_status_rx), (Bottom, bridge_status_bottom));
  Element.draw
    roebling_status
    context
    ((Right, bike_status_rx), (Top, j_y + Subway.stroke_safe_padding + base_padding));
  Element.draw
    vesey_status
    context
    ((Left, parking_grid_left), (Top, parking_grid_left_column_top));
  Element.draw
    west_status
    context
    ( (Left, parking_grid_left)
    , (Top, parking_grid_left_column_top + parking_status_height + base_padding) );
  Element.draw
    barclay_status
    context
    ((Left, parking_grid_right_column), (Top, parking_grid_top));
  Element.draw
    fulton_status
    context
    ( (Left, parking_grid_right_column)
    , (Top, parking_grid_top + parking_status_height + base_padding) );
  let random = Random.State.make [| Time_ns.hash now |] in
  let choose_sky_spot ~radius ~offset:(offset_x, offset_y) ~index ~count =
    let padding = radius + 2 in
    let left = screen_edge_padding + padding in
    let right = w - screen_edge_padding - offset_x - padding in
    let top = status_text_baseline + screen_edge_padding + padding - Int.min 0 offset_y in
    let bottom = map_faded_top - screen_edge_padding - padding in
    let height = (bottom - top + 1) / count in
    let y = top + (index * height) + Random.State.int random height in
    let x = left + Random.State.int random (right - left + 1) in
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
   | false -> ());
  Sky.draw_sun_moon
    context
    ~light_fill:(light_fill ?offset:None)
    ~dark_fill:moon_dark_fill
    ~is_night
    ~weather
    ~font
    ~text_fill:(solid `b)
    ~status_text_size
    ~center:sun_moon_center
    ~radius:sun_moon_radius;
  Element.draw
    rendered_battery
    context
    ((Left, status_text_padding), (Baseline, status_text_baseline));
  Element.draw
    rendered_updated
    context
    ((Right, w - status_text_padding), (Baseline, status_text_baseline));
  Option.iter message ~f:(fun message ->
    Element.draw message context ((Center, w / 2), (Baseline, status_text_baseline)));
  let farther_wall_x, sun_moon_near_side_x =
    match sun_moon_center_x < w / 2 with
    | true -> w, sun_moon_center_x + sun_moon_radius
    | false -> 0, sun_moon_center_x - sun_moon_radius
  in
  let cloud_center_x = (sun_moon_near_side_x + farther_wall_x) / 2 in
  let cloud_base_y = h / 3 in
  (match weather.conditions with
   | Not_cloudy -> ()
   | Cloudy precipitation ->
     Sky.draw_cloud
       context
       ~fill:cloud_fill
       ~graph_style
       ~zone:display_zone
       ~padding:base_padding
       ~center_x:cloud_center_x
       ~base_y:cloud_base_y
       precipitation);
  (match is_night with
   | true -> ()
   | false ->
     let wing_width = 10 in
     let spacing = 28 in
     let x, y =
       choose_sky_spot ~radius:wing_width ~offset:(spacing, -4) ~index:0 ~count:1
     in
     draw_bird context ~wing_width ~center:(x, y);
     draw_bird context ~wing_width ~center:(x + spacing, y - 4));
  (match weather.us_aqi with
   | Some aqi when Float.(aqi > 100.) ->
     let aqi = Int.to_string (Float.iround_nearest_exn aqi) in
     let aqi_text =
       text
         ~font
         ~fill:(solid inverse_background_color)
         [%string "AQI %{aqi}"]
         ~size:label_size
     in
     let _, text_height = Element.size aqi_text in
     let padding = 4 in
     let width = 62
     and height = text_height + (2 * padding) in
     let badge =
       Element.create
         ~size:(width, height)
         ~draw:(fun context ~upper_left:(left, top) ->
           let right = left + width - 1
           and bottom = top + height in
           rounded_polygon
             context
             ~radius:6
             ~fill:(solid background_color)
             ~stroke:(Stroke.solid inverse_background_color 1)
             [ left, top; right, top; right, bottom; left, bottom ];
           Element.draw
             aqi_text
             context
             ((Center, left + (width / 2)), (Top, top + padding)))
         ()
     in
     Element.draw badge context ((Center, w / 2), (Bottom, map_faded_top - base_padding))
   | Some _ | None -> ());
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
    [ { Subway.Status.Selection.line = L
      ; minimum_minutes = 11
      ; westbound_mta_direction = North
      }
    ]
  and marcy_rows =
    [ { Subway.Status.Selection.line = J_and_Z_but_call_it_J
      ; minimum_minutes = 5
      ; westbound_mta_direction = South
      }
    ; { line = M; minimum_minutes = 5; westbound_mta_direction = North }
    ]
  in
  let%bind citibike_result = Feeds.Citibike.query cache
  and mta_subway_status_result =
    Feeds.Mta_subway.query cache ~which_feeds:[ Line_L; Lines_J_Z; Lines_B_D_F_M ]
  and weather_result = query_weather cache in
  return
    (let%bind.Or_error citibike_stations =
       Feeds.Latest_result.latest_success citibike_result
       |> Or_error.map ~f:(fun completed -> completed.value)
     and forecast, air_quality =
       Feeds.Latest_result.latest_success weather_result
       |> Or_error.map ~f:(fun completed -> completed.value)
     and mta_subway_status = mta_subway_status_result in
     let station_status station_id =
       match Map.find citibike_stations station_id with
       | Some station -> Citibike_status.create station
       | None ->
         { Citibike_status.availability = Not_renting
         ; parking = Not_accepting_returns
         ; bikes_available_frac = 0.
         }
     in
     let%map.Or_error weather =
       Weather_info.create
         ~look_forward_hours:8
         ~now
         ~forecast
         ~us_aqi:air_quality.current.us_aqi
     and bedford_status =
       Subway.Status.create mta_subway_status ~now ~station_id:"L08" ~rows:bedford_rows
     and marcy_status =
       Subway.Status.create mta_subway_status ~now ~station_id:"M16" ~rows:marcy_rows
     in
     { Draw_inputs.device_status
     ; message
     ; weather
     ; bridge_status = station_status "66dc8768-0aca-11e7-82f6-3863bb44ef7c"
     ; roebling_status = station_status "66dced76-0aca-11e7-82f6-3863bb44ef7c"
     ; vesey_status = station_status "66db8d89-0aca-11e7-82f6-3863bb44ef7c"
     ; west_status = station_status "2170352212111402482"
     ; barclay_status = station_status "66dbf73d-0aca-11e7-82f6-3863bb44ef7c"
     ; fulton_status = station_status "66db79a3-0aca-11e7-82f6-3863bb44ef7c"
     ; bedford_status
     ; marcy_status
     ; now
     })
;;

let preset_draw_inputs ~font ~message ~now ~weather =
  let _, widest_two_digit_number =
    Drawing.Font.max_width font [ `Number (12, 99) ] ~size:20.
  in
  let widest_two_digit_number = Int.of_string widest_two_digit_number in
  let citibike_status =
    Citibike_status.Testing_data.dense_text ~widest_two_digit_number
  in
  let bedford_status =
    Subway.Status.Testing_data.dense_text ~widest_two_digit_number ~lines:[ L ]
  and marcy_status =
    Subway.Status.Testing_data.dense_text
      ~widest_two_digit_number
      ~lines:[ J_and_Z_but_call_it_J; M ]
  in
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
      live_draw_inputs cache ~message ~device_status:{ battery_voltage = Some 4.1 } ~now
    | Preview (Some preset) ->
      let%bind.Deferred.Or_error preset = return (Preset.of_string preset) in
      (match preset with
       | Dense_text_night ->
         let now =
           Time_ns.occurrence
             `First_after_or_at
             now
             ~ofday:(Time_ns.Ofday.create ~hr:22 ())
             ~zone:display_zone
         in
         let weather = Weather_info.Testing_data.dense_text ~now ~zone:display_zone in
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
         let weather = Weather_info.Testing_data.stormy ~hour_start ~zone:display_zone in
         return
           (Or_error.map weather ~f:(fun weather ->
              preset_draw_inputs ~font ~message ~now ~weather))
       | Errors_alerts ->
         let weather = Weather_info.Testing_data.errors ~now ~zone:display_zone in
         let citibike_status = Citibike_status.Testing_data.errors in
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
              ; bedford_status = Subway.Status.Testing_data.errors ~lines:[ L ]
              ; marcy_status =
                  Subway.Status.Testing_data.errors ~lines:[ J_and_Z_but_call_it_J; M ]
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
