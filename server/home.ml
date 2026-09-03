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
      ~font
      ~title
      ~box_size
      ~(station : Citibike.Station.t)
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
  Drawing.status_box
    context
    upper_left
    lower_right
    ~font
    ~title
    ~fill:(Drawing.Fill.fractional ~frac ~frontier_angle_degrees:15.)
    ~f
;;

let available_bike_status
      context
      ~font
      ~title
      ~box_size
      ~(station : Citibike.Station.t)
      anchor
  =
  availability_status
    context
    ~font
    ~title
    ~box_size
    ~station
    ~f:(fun context ~fill ->
      let width, height = Drawing.Context.size context in
      let fill = Drawing.Fill.invert fill
      and count_size = 40.
      and baseline_y = height - 12 in
      match station.is_renting with
      | true ->
        let middle = width / 2 in
        let bikes_available = station.bikes_available - station.ebikes_available
        and ebikes_available = Int.to_string station.ebikes_available in
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left:0
          ~right:middle
          (Int.to_string bikes_available);
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left:middle
          ~right:width
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
          ~left:middle
          ~right:width
          "e"
      | false ->
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left:0
          ~right:width
          "off")
    anchor
;;

let parking_status context ~font ~title ~(station : Citibike.Station.t) anchor =
  availability_status
    context
    ~font
    ~title
    ~box_size:(75, 55)
    ~station
    ~f:(fun context ~fill ->
      let width, height = Drawing.Context.size context in
      draw_centered_text
        context
        ~font
        ~fill:(Drawing.Fill.invert fill)
        ~size:40.
        ~baseline_y:(height - 12)
        ~left:0
        ~right:width
        (match station.is_returning with
         | true -> Int.to_string station.docks_available
         | false -> "off"))
    anchor
;;

let draw
      ~font
      ~citibike_stations
      ~(mta_subway_status : Mta_subway.Status.t)
      ~device_status
      ~now
  =
  let find_station = Map.find_or_error citibike_stations in
  let%bind.Or_error bridge_station = find_station "66dc8768-0aca-11e7-82f6-3863bb44ef7c"
  and roebling_station = find_station "66dced76-0aca-11e7-82f6-3863bb44ef7c"
  and vesey_station = find_station "66db8d89-0aca-11e7-82f6-3863bb44ef7c"
  and barclay_station = find_station "66dbf73d-0aca-11e7-82f6-3863bb44ef7c"
  and fulton_station = find_station "66db79a3-0aca-11e7-82f6-3863bb44ef7c"
  and bedford_status = Map.find_or_error mta_subway_status.stop_status_by_stop_id "L08"
  and marcy_status = Map.find_or_error mta_subway_status.stop_status_by_stop_id "M16" in
  let open Drawing.O in
  (* Short geometry variable names keep the function legible. *)
  let w = 800
  and h = 480 in
  let image = Image.create_grey ~max_val:1 w h in
  let context = Context.create image in
  let black = Fill.solid `b
  and land_fill = Fill.bayer_exn ~size:16 ~white_frac:(254. /. 256.)
  and geo_stroke = Stroke.solid `b 8 in
  rect context ~fill:(Fill.solid `w) (0, 0) (w, h);
  text context ~font ~fill:black ~origin_x:200 ~baseline_y:100 ~size:80. "weather here";
  let map_top = h / 2
  and man_fade_height = 20 in
  let man_faded_top = map_top - man_fade_height in
  let north_fade fill =
    fade_to_white
      ~white_frac:(fun (_, y) ->
        1. -. (Float.of_int (y - man_faded_top) /. Float.of_int man_fade_height))
      fill
  in
  let man_w = 250
  and man_padding = 10
  and man_inset = 50 in
  let manhattan_path =
    [ man_padding, man_faded_top
    ; man_padding, h - man_padding - man_inset
    ; man_padding + man_inset, h - man_padding
    ; man_w - man_inset, h - man_padding
    ; man_w, h - man_padding - man_inset
    ; man_w, map_top + man_inset + 20
    ; man_w - man_inset, map_top + 20
    ; man_w - man_inset, map_top
    ; man_w - man_inset, man_faded_top
    ]
  in
  let status_padding = 8
  and available_bike_status_size = 120, 62 in
  let available_bike_status_width, _ = available_bike_status_size in
  let br_start =
    w
    - Subway_status_box.width
    - available_bike_status_width
    - (3 * status_padding)
    - Stroke.safe_padding geo_stroke
  in
  let br_foot = 50 in
  let brooklyn_path =
    [ w, map_top; br_start, map_top; br_start, h - br_foot; br_start - br_foot, h ]
  in
  let water_fill (x, y) =
    let wave_x = (x + (y / 12 % 2 * 12)) % 24 in
    let distance_from_center = Int.abs (wave_x - 12) in
    match y % 12 = 5 - (distance_from_center * distance_from_center / 48) with
    | true -> `b
    | false -> `w
  in
  rect context ~fill:water_fill (0, map_top) (w, h);
  polygon context ~fill:(north_fade land_fill) manhattan_path;
  polygon context ~fill:land_fill (brooklyn_path @ [ w, h ]);
  rounded_path
    context
    ~radius:20
    ~stroke:(Stroke.create (north_fade black) 8)
    manhattan_path;
  rounded_path context ~radius:20 ~stroke:geo_stroke brooklyn_path;
  let subway_stroke fill = Stroke.create ~casing:(Stroke.solid `w 12) fill 8 in
  let l_fill : Fill.t = Fill.bayer_exn ~white_frac:(9. /. 16.)
  and l_y = map_top + 30 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke l_fill)
    [ man_padding + 25, l_y; w, l_y ];
  let j_fill : Fill.t = Fill.bayer_exn ~white_frac:(1. /. 16.)
  and j_y = h - 100
  and j_x = man_w - man_inset - 15 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke j_fill)
    [ w, j_y; j_x, j_y; j_x, h - man_padding - 25 ];
  let m_fill : Fill.t = Fill.bayer_exn ~offset:(1, 1) ~white_frac:(10. /. 16.)
  and m_y = j_y - 12
  and m_diag = 25
  and m_vert_x = man_w - (man_inset * 2) in
  let m_houston_y = m_y - man_inset - 10 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke (north_fade m_fill))
    [ w, m_y
    ; man_w - m_diag, m_y
    ; man_w - m_diag, m_houston_y
    ; m_vert_x, m_houston_y
    ; m_vert_x, man_faded_top
    ];
  let subway_status_right = w - status_padding in
  let bedford_status_top = map_top + status_padding + Stroke.safe_padding geo_stroke in
  let (subway_status_left, _), (_, bedford_status_bottom) =
    Subway_status_box.draw
      context
      ~anchor:(Anchor.Ur (subway_status_right, bedford_status_top))
      ~font
      ~title:"bedford"
      ~now
      ~stop_status:bedford_status
      ~rows:
        [ { bullet = "L", l_fill
          ; route_ids = [ "L" ]
          ; minimum_minutes = 11
          ; westbound_mta_direction = "N"
          }
        ]
  in
  Subway_status_box.draw
    context
    ~anchor:(Anchor.Ur (subway_status_right, bedford_status_bottom + status_padding))
    ~font
    ~title:"marcy"
    ~now
    ~stop_status:marcy_status
    ~rows:
      [ { bullet = "J", j_fill
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
  |> ignore;
  let bike_status_rx = subway_status_left - status_padding in
  available_bike_status
    context
    ~font
    ~title:"bridge"
    ~box_size:available_bike_status_size
    ~station:bridge_station
    (Anchor.Lr
       (bike_status_rx, m_y - status_padding - Stroke.safe_padding (subway_stroke black)));
  available_bike_status
    context
    ~font
    ~title:"roebling"
    ~box_size:available_bike_status_size
    ~station:roebling_station
    (Anchor.Ur
       (bike_status_rx, j_y + Stroke.safe_padding (subway_stroke black) + status_padding));
  parking_status
    context
    ~font
    ~title:"vesey"
    ~station:vesey_station
    (Anchor.Ul
       ( man_padding + Stroke.safe_padding geo_stroke + status_padding
       , h - man_padding - Stroke.safe_padding geo_stroke - man_inset - 70 ));
  parking_status
    context
    ~font
    ~title:"barclay"
    ~station:barclay_station
    (Anchor.Lr
       ( j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2)
       , j_y - status_padding ));
  parking_status
    context
    ~font
    ~title:"fulton"
    ~station:fulton_station
    (Anchor.Ur
       (j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2), j_y));
  let status_text =
    let { Renderer.Device_status.percent_charged; usb_connected } = device_status in
    let percent_charged = Float.iround_nearest_exn percent_charged in
    [ Some [%string "%{percent_charged#Int}%"]
    ; (match usb_connected, Int.equal percent_charged 100 with
       | false, _ -> None
       | true, true -> Some "(charged)"
       | true, false -> Some "(charging)")
    ; Some "/"
    ; Some "updated"
    ; Some
        (Time_ns_unix.format
           now
           "%H:%M"
           ~zone:(Time_ns_unix.Zone.find_exn "America/New_York"))
    ]
    |> List.filter_opt
    |> String.concat ~sep:" "
  and status_text_size = 24.
  and status_text_padding = 8 in
  let rendered_status_text = Font.render_text font status_text ~size:status_text_size in
  text
    ~halo:(2, Fill.solid `w)
    context
    ~font
    ~fill:black
    ~origin_x:
      (w
       - status_text_padding
       - rendered_status_text.width
       + rendered_status_text.origin_x)
    ~baseline_y:(h - status_text_padding)
    ~size:status_text_size
    status_text;
  Ok image
;;

type debug_preset = Dense

let render input cache =
  let device_status =
    match input with
    | Renderer.Input.Device (Some device_status) -> device_status
    | Device None | Preview None ->
      { Renderer.Device_status.percent_charged = 5.; usb_connected = false }
    | Preview (Some Dense) ->
      { Renderer.Device_status.percent_charged = 100.; usb_connected = true }
  in
  let%bind citibike_result = Citibike.query cache
  and mta_subway_status_result =
    Mta_subway.query
      cache
      ~which_feeds:[ Mta_subway.Realtime_feed.Line_L; Lines_J_Z; Lines_B_D_F_M ]
  and _weather_result =
    Weather.query
      cache
      ~coordinates:{ Weather.Coordinates.latitude = 40.7084; longitude = -73.9578 }
  in
  let%bind.Deferred.Or_error font =
    Font.create ~ttf_file:"server/fonts/inter_medium.ttf" |> return
  and citibike_stations =
    Latest_result.latest_success citibike_result
    |> Or_error.map ~f:(fun completed -> completed.value)
    |> return
  and mta_subway_status = mta_subway_status_result |> return in
  let%map.Deferred.Or_error image =
    draw ~font ~citibike_stations ~mta_subway_status ~device_status ~now:(Time_ns.now ())
    |> return
  in
  { Renderer.Render.buffer = image
  ; time_until_refresh = Time_ns.Span.of_sec 30.
  ; debug_info =
      citibike_result
      |> Latest_result.map ~f:(fun stations ->
        Map.find stations "66dc8768-0aca-11e7-82f6-3863bb44ef7c")
      |> Latest_result.sexp_of_t (Option.sexp_of_t Citibike.Station.sexp_of_t)
      |> Sexp.to_string_hum
  }
;;

let renderer =
  { Renderer.debug_presets = [ Dense ]
  ; debug_preset_name = (fun Dense -> "dense")
  ; render
  }
;;
