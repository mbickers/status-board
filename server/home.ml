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
    ~fill:(Drawing.Fill.fractional ~frac ~frontier_angle_degrees:15.)
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
    ~f:(fun context ~fill ->
      let width, height = Drawing.Context.size context in
      let fill = Drawing.Fill.invert fill
      and count_size = Status_box.Style.primary_font_size style
      and horizontal_padding = Status_box.Style.horizontal_padding style
      and horizontal_padding_between_text =
        Status_box.Style.horizontal_padding_between_text style
      in
      let baseline_y = height - Status_box.Style.baseline_padding style
      and left = horizontal_padding
      and right = width - horizontal_padding in
      match station.is_renting with
      | true ->
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

let parking_status context ~style ~title ~box_size ~(station : Citibike.Station.t) anchor =
  let font = Status_box.Style.font style in
  availability_status
    context
    ~style
    ~title
    ~box_size
    ~station
    ~f:(fun context ~fill ->
      let width, height = Drawing.Context.size context in
      let horizontal_padding = Status_box.Style.horizontal_padding style in
      draw_centered_text
        context
        ~font
        ~fill:(Drawing.Fill.invert fill)
        ~size:(Status_box.Style.primary_font_size style)
        ~baseline_y:(height - Status_box.Style.baseline_padding style)
        ~left:horizontal_padding
        ~right:(width - horizontal_padding)
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
      ~status_box_style
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
  let font = Status_box.Style.font status_box_style in
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
  let maximum_citibike_count_width, _ =
    Font.max_width
      font
      [ `Number (0, 99) ]
      ~size:(Status_box.Style.primary_font_size status_box_style)
  in
  let maximum_citibike_count_width = Float.iround_up_exn maximum_citibike_count_width
  and horizontal_padding = Status_box.Style.horizontal_padding status_box_style in
  let status_padding = 8
  and inter_subway_padding = 4
  and available_bike_status_size =
    ( (2 * horizontal_padding)
      + Status_box.Style.horizontal_padding_between_text status_box_style
      + (2 * maximum_citibike_count_width)
    , 62 )
  and parking_status_size = (2 * horizontal_padding) + maximum_citibike_count_width, 55 in
  let available_bike_status_width, available_bike_status_height =
    available_bike_status_size
  and subway_status_width = Subway_status_box.width status_box_style in
  let subway_stroke fill = Stroke.create ~casing:(Stroke.solid `w 12) fill 8 in
  let subway_stroke_safe_padding = Stroke.safe_padding (subway_stroke black)
  and l_fill : Fill.t = Fill.bayer_exn ~white_frac:(9. /. 16.)
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
    (2 * (status_padding + Stroke.safe_padding geo_stroke))
    + bedford_status_height
    + status_padding
    + marcy_status_height
  in
  let brooklyn_top = h - brooklyn_height in
  let subway_status_right = w - status_padding
  and bedford_status_top =
    brooklyn_top + status_padding + Stroke.safe_padding geo_stroke
  in
  let subway_status_left = subway_status_right - subway_status_width
  and bedford_status_bottom = bedford_status_top + bedford_status_height in
  let marcy_status_top = bedford_status_bottom + status_padding in
  let marcy_status_bottom = marcy_status_top + marcy_status_height in
  let j_y =
    marcy_status_bottom
    - available_bike_status_height
    - status_padding
    - subway_stroke_safe_padding
  and j_x = man_w - man_inset - 15 in
  let m_y =
    j_y - subway_stroke_safe_padding - inter_subway_padding - subway_stroke_safe_padding
  and m_diag = 25
  and m_vert_x = man_w - (man_inset * 2) in
  let bridge_status_bottom = m_y - status_padding - subway_stroke_safe_padding in
  let bridge_status_top = bridge_status_bottom - available_bike_status_height in
  let l_y = bridge_status_top - status_padding - subway_stroke_safe_padding in
  let br_start =
    w
    - subway_status_width
    - available_bike_status_width
    - (3 * status_padding)
    - Stroke.safe_padding geo_stroke
  in
  let br_foot = 50
  and brooklyn_corner_radius = 20 in
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
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke l_fill)
    [ man_padding + 25, l_y; w, l_y ];
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke j_fill)
    [ w, j_y; j_x, j_y; j_x, h - man_padding - 25 ];
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
  let bike_status_rx = subway_status_left - status_padding in
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
    (Anchor.Ur (bike_status_rx, j_y + subway_stroke_safe_padding + status_padding));
  parking_status
    context
    ~style:status_box_style
    ~title:"ves"
    ~box_size:parking_status_size
    ~station:vesey_station
    (Anchor.Ul
       ( man_padding + Stroke.safe_padding geo_stroke + status_padding
       , h - man_padding - Stroke.safe_padding geo_stroke - man_inset - 70 ));
  parking_status
    context
    ~style:status_box_style
    ~title:"barc"
    ~box_size:parking_status_size
    ~station:barclay_station
    (Anchor.Lr
       ( j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2)
       , j_y - status_padding ));
  parking_status
    context
    ~style:status_box_style
    ~title:"ful"
    ~box_size:parking_status_size
    ~station:fulton_station
    (Anchor.Ur
       (j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2), j_y));
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
  and status_text_size = 24.
  and status_text_padding = 8 in
  let draw_status_text ~origin_x string =
    let rendered_text = Font.render_text font string ~size:status_text_size in
    text
      ~halo:(2, Fill.solid `w)
      context
      ~font
      ~fill:black
      ~origin_x:(origin_x rendered_text)
      ~baseline_y:(status_text_padding + rendered_text.baseline_y)
      ~size:status_text_size
      string
  in
  draw_status_text voltage_text ~origin_x:(fun rendered_text ->
    status_text_padding + rendered_text.origin_x);
  draw_status_text updated_text ~origin_x:(fun rendered_text ->
    w - status_text_padding - rendered_text.width + rendered_text.origin_x);
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
  let status_box_style =
    Status_box.Style.create
      ~font
      ~horizontal_padding:8
      ~horizontal_padding_between_text:12
      ~baseline_padding:12
      ~primary_font_size:40.
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
      let _, widest_two_digit_number =
        Font.max_width
          font
          [ `Number (12, 99) ]
          ~size:(Status_box.Style.primary_font_size status_box_style)
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
      ~status_box_style
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
