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
      and baseline_y = height - 12
      and horizontal_padding = 2 in
      let left = horizontal_padding
      and right = width - horizontal_padding in
      match station.is_renting with
      | true ->
        let middle = (left + right) / 2 in
        let bikes_available = station.bikes_available - station.ebikes_available
        and ebikes_available = Int.to_string station.ebikes_available in
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left
          ~right:middle
          (Int.to_string bikes_available);
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left:middle
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
          ~left:middle
          ~right
          "e"
      | false ->
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left
          ~right
          "off")
    anchor
;;

let parking_status context ~font ~title ~(station : Citibike.Station.t) anchor =
  availability_status
    context
    ~font
    ~title
    ~box_size:(71, 55)
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

module Draw_inputs = struct
  type t =
    { device_status : Renderer.Device_status.t
    ; bridge_station : Citibike.Station.t
    ; roebling_station : Citibike.Station.t
    ; vesey_station : Citibike.Station.t
    ; barclay_station : Citibike.Station.t
    ; fulton_station : Citibike.Station.t
    ; bedford_status : Mta_subway.Stop_status.t
    ; marcy_status : Mta_subway.Stop_status.t
    ; now : Time_ns.t
    }
end

let draw
      ~font
      ~device_status
      ~bridge_station
      ~roebling_station
      ~vesey_station
      ~barclay_station
      ~fulton_station
      ~bedford_status
      ~marcy_status
      ~now
  =
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
  let map_height = h / 2
  and brooklyn_height = 238
  and man_fade_height = 20 in
  let map_top = h - map_height in
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
    Path_resolver_step.resolve
      [ Path_resolver_step.Point (man_padding, man_faded_top)
      ; Point (man_padding, h - man_padding - man_inset)
      ; Offset (man_inset, man_inset)
      ; Point (man_w - man_inset, h - man_padding)
      ; Offset (man_inset, -man_inset)
      ; Point (man_w, map_top + man_inset + 20)
      ; Offset (-man_inset, -man_inset)
      ; Offset (0, -20)
      ; Point (man_w - man_inset, man_faded_top)
      ]
  in
  let status_padding = 8
  and available_bike_status_size = 100, 62 in
  let available_bike_status_width, _ = available_bike_status_size in
  let br_start =
    w
    - Subway_status_box.width
    - available_bike_status_width
    - (3 * status_padding)
    - Stroke.safe_padding geo_stroke
  in
  let br_foot = 50
  and brooklyn_corner_radius = 20 in
  let brooklyn_top = h - brooklyn_height in
  let brooklyn_path =
    [ w, brooklyn_top
    ; br_start, brooklyn_top
    ; br_start, h - br_foot
    ; br_start - br_foot, h
    ]
  in
  let water_fill (x, y) =
    let wave_x = (x + (y / 12 % 2 * 12)) % 24 in
    let distance_from_center = Int.abs (wave_x - 12) in
    match y % 12 = 5 - (distance_from_center * distance_from_center / 48) with
    | true -> `b
    | false -> `w
  in
  let brooklyn_fill (x, y) =
    let x_distance = x - (br_start + brooklyn_corner_radius)
    and y_distance = y - (brooklyn_top + brooklyn_corner_radius) in
    match
      x < br_start + brooklyn_corner_radius
      && y < brooklyn_top + brooklyn_corner_radius
      && (x_distance * x_distance) + (y_distance * y_distance)
         > brooklyn_corner_radius * brooklyn_corner_radius
    with
    | true -> water_fill (x, y)
    | false -> land_fill (x, y)
  in
  rect context ~fill:water_fill (0, map_top) (br_start + 30, h);
  polygon context ~fill:(north_fade land_fill) manhattan_path;
  polygon context ~fill:brooklyn_fill (brooklyn_path @ [ w, h ]);
  rounded_path
    context
    ~radius:20
    ~stroke:(Stroke.create (north_fade black) 8)
    manhattan_path;
  rounded_path context ~radius:brooklyn_corner_radius ~stroke:geo_stroke brooklyn_path;
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
    ~title:"roeb"
    ~box_size:available_bike_status_size
    ~station:roebling_station
    (Anchor.Ur
       (bike_status_rx, j_y + Stroke.safe_padding (subway_stroke black) + status_padding));
  parking_status
    context
    ~font
    ~title:"ves"
    ~station:vesey_station
    (Anchor.Ul
       ( man_padding + Stroke.safe_padding geo_stroke + status_padding
       , h - man_padding - Stroke.safe_padding geo_stroke - man_inset - 70 ));
  parking_status
    context
    ~font
    ~title:"barc"
    ~station:barclay_station
    (Anchor.Lr
       ( j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2)
       , j_y - status_padding ));
  parking_status
    context
    ~font
    ~title:"ful"
    ~station:fulton_station
    (Anchor.Ur
       (j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2), j_y));
  let status_text =
    let voltage =
      match device_status.Renderer.Device_status.battery_voltage with
      | Some battery_voltage ->
        [%string "voltage %{Float.to_string_hum battery_voltage ~decimals:1}V"]
      | None -> "voltage unknown"
    in
    [ voltage
    ; "/"
    ; "updated"
    ; Time_ns_unix.format
        now
        "%H:%M"
        ~zone:(Time_ns_unix.Zone.find_exn "America/New_York")
    ]
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
  image
;;

type debug_preset = Dense

let live_draw_inputs cache ~device_status ~now =
  let%bind citibike_result = Citibike.query cache
  and mta_subway_status_result =
    Mta_subway.query
      cache
      ~which_feeds:[ Mta_subway.Realtime_feed.Line_L; Lines_J_Z; Lines_B_D_F_M ]
  in
  return
    (let%bind.Or_error citibike_stations =
       Latest_result.latest_success citibike_result
       |> Or_error.map ~f:(fun completed -> completed.value)
     and mta_subway_status = mta_subway_status_result in
     let find_station = Map.find_or_error citibike_stations in
     let%map.Or_error bridge_station = find_station "66dc8768-0aca-11e7-82f6-3863bb44ef7c"
     and roebling_station = find_station "66dced76-0aca-11e7-82f6-3863bb44ef7c"
     and vesey_station = find_station "66db8d89-0aca-11e7-82f6-3863bb44ef7c"
     and barclay_station = find_station "66dbf73d-0aca-11e7-82f6-3863bb44ef7c"
     and fulton_station = find_station "66db79a3-0aca-11e7-82f6-3863bb44ef7c"
     and bedford_status = Map.find_or_error mta_subway_status.stop_status_by_stop_id "L08"
     and marcy_status =
       Map.find_or_error mta_subway_status.stop_status_by_stop_id "M16"
     in
     { Draw_inputs.device_status
     ; bridge_station
     ; roebling_station
     ; vesey_station
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
    | Preview (Some Dense) ->
      let text_width number =
        (Font.render_text font (Int.to_string number) ~size:40.).Font.Rendered_text.width
      in
      let widest_two_digit_number =
        List.range 11 100
        |> List.fold ~init:10 ~f:(fun widest number ->
          match Int.compare (text_width number) (text_width widest) >= 0 with
          | true -> number
          | false -> widest)
      in
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
           ; bridge_station = station
           ; roebling_station = station
           ; vesey_station = station
           ; barclay_station = station
           ; fulton_station = station
           ; bedford_status
           ; marcy_status
           ; now
           })
  in
  let { Draw_inputs.device_status
      ; bridge_station
      ; roebling_station
      ; vesey_station
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
      ~bridge_station
      ~roebling_station
      ~vesey_station
      ~barclay_station
      ~fulton_station
      ~bedford_status
      ~marcy_status
      ~now
  in
  return (Ok { Renderer.Render.buffer; time_until_refresh = Time_ns.Span.of_sec 30. })
;;

let renderer =
  { Renderer.debug_presets = [ Dense ]
  ; debug_preset_name = (fun Dense -> "dense")
  ; render
  }
;;
