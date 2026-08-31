open! Core
open! Async

module Anchor = struct
  type t =
    | Ul of int * int
    | Ur of int * int
    | Ll of int * int
    | Lr of int * int
end

let status_box context (left, top) (right, bottom) ~font ~title ~f =
  let radius = 10
  and stroke = Drawing.Stroke.solid `b 4 in
  let inside ~inset (x, y) =
    let left = left + inset
    and top = top + inset
    and right = right - inset
    and bottom = bottom - inset
    and radius = radius - inset in
    let nearest_x = Int.max (left + radius) (Int.min (right - radius - 1) x)
    and nearest_y = Int.max (top + radius) (Int.min (bottom - radius - 1) y) in
    let dx = x - nearest_x
    and dy = y - nearest_y in
    (dx * dx) + (dy * dy) <= radius * radius
  in
  for y = top to bottom - 1 do
    for x = left to right - 1 do
      if inside ~inset:0 (x, y)
      then
        Drawing.Context.write
          context
          (x, y)
          (if inside ~inset:stroke.width (x, y) then `w else stroke.fill (x, y))
    done
  done;
  let safe_padding = Drawing.Stroke.safe_padding stroke in
  f
    (Drawing.Context.crop
       context
       ~offset:(left + safe_padding, top + safe_padding)
       ~size:(right - left - (2 * safe_padding), bottom - top - (2 * safe_padding)));
  let title_size = 17. in
  let rendered_title = Font.render_text font title ~size:title_size in
  let title_left = left + radius + 5 in
  let title_top = top - 3 in
  Drawing.rect
    context
    ~fill:(Drawing.Fill.solid `w)
    (title_left - 4, title_top - 2)
    (title_left + rendered_title.width + 4, title_top + rendered_title.height + 2);
  Drawing.text
    context
    ~font
    ~fill:(Drawing.Fill.solid `b)
    ~origin_x:(title_left + rendered_title.origin_x)
    ~baseline_y:(title_top + rendered_title.baseline_y)
    ~size:title_size
    title
;;

let available_bike_status context ~font ~title anchor =
  let width = 120
  and height = 65 in
  let upper_left, lower_right =
    match anchor with
    | Anchor.Ul (left, top) -> (left, top), (left + width, top + height)
    | Ur (right, top) -> (right - width, top), (right, top + height)
    | Ll (left, bottom) -> (left, bottom - height), (left + width, bottom)
    | Lr (right, bottom) -> (right - width, bottom - height), (right, bottom)
  in
  status_box context upper_left lower_right ~font ~title ~f:(fun context ->
    Drawing.text
      context
      ~font
      ~fill:(Drawing.Fill.solid `b)
      ~origin_x:20
      ~baseline_y:30
      ~size:30.
      "bike")
;;

let parking_status context ~font ~title anchor =
  let width = 75
  and height = 55 in
  let upper_left, lower_right =
    match anchor with
    | Anchor.Ul (left, top) -> (left, top), (left + width, top + height)
    | Ur (right, top) -> (right - width, top), (right, top + height)
    | Ll (left, bottom) -> (left, bottom - height), (left + width, bottom)
    | Lr (right, bottom) -> (right - width, bottom - height), (right, bottom)
  in
  status_box context upper_left lower_right ~font ~title ~f:(fun context ->
    Drawing.text
      context
      ~font
      ~fill:(Drawing.Fill.solid `b)
      ~origin_x:20
      ~baseline_y:30
      ~size:30.
      "park")
;;

let draw ~font =
  let open Drawing.O in
  let w = 800
  and h = 480 in
  let image = Image.create_grey ~max_val:1 w h in
  let context = Context.create image in
  let black = Fill.solid `b
  and land_fill = Fill.bayer ~size:16 254
  and geo_stroke = Stroke.solid `b 8 in
  rect context ~fill:(Fill.solid `w) (0, 0) (w, h);
  text context ~font ~fill:black ~origin_x:222 ~baseline_y:100 ~size:80. "hello world";
  let map_top = h / 2 in
  let man_fade_height = 20 in
  let man_faded_top = map_top - man_fade_height in
  let north_fade fill =
    fade_to_white ~level:(fun (_, y) -> (y - man_faded_top) * 16 / man_fade_height) fill
  in
  let manhattan_stroke = { Stroke.fill = north_fade black; width = 8 } in
  let man_w = 250 in
  let man_padding = 10 in
  let man_inset = 50 in
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
  let river_w = 100 in
  let br_start = man_w + river_w in
  let br_foot = 50 in
  let brooklyn_path =
    [ w, map_top; br_start, map_top; br_start, h - br_foot; br_start - br_foot, h ]
  in
  let brooklyn_polygon = brooklyn_path @ [ w, h ] in
  let water_fill (x, y) =
    let wave_x = (x + (y / 12 % 2 * 12)) % 24 in
    let distance_from_center = Int.abs (wave_x - 12) in
    if y % 12 = 5 - (distance_from_center * distance_from_center / 48) then `b else `w
  in
  rect context ~fill:water_fill (0, map_top) (w, h);
  polygon context ~fill:(north_fade land_fill) manhattan_path;
  polygon context ~fill:land_fill brooklyn_polygon;
  rounded_path context ~radius:20 ~stroke:manhattan_stroke manhattan_path;
  rounded_path context ~radius:20 ~stroke:geo_stroke brooklyn_path;
  let subway_stroke fill = { Stroke.fill; width = 8 } in
  let l_fill = Fill.bayer 9 in
  let l_y = map_top + 30 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke l_fill)
    [ man_padding + 25, l_y; w, l_y ];
  let j_fill = Fill.bayer 1 in
  let j_y = h - 100 in
  let j_x = man_w - man_inset - 15 in
  let j_downward_bottom = h - man_padding - 25 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke j_fill)
    [ w, j_y; j_x, j_y; j_x, j_downward_bottom ];
  let m_fill = north_fade (Fill.bayer ~offset:(1, 1) 10) in
  let m_y = j_y - 12 in
  let m_diag = 25 in
  let m_vert_x = man_w - (man_inset * 2) in
  let m_houston_y = m_y - man_inset - 10 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke m_fill)
    [ w, m_y
    ; man_w - m_diag, m_y
    ; man_w - m_diag, m_houston_y
    ; m_vert_x, m_houston_y
    ; m_vert_x, man_faded_top
    ];
  let status_padding = 8 in
  let subway_status_left = w - 250 - status_padding in
  let status_right = w - status_padding in
  let jzm_status_y = m_y - 40 in
  status_box
    context
    (subway_status_left, map_top + status_padding + Stroke.safe_padding geo_stroke)
    (status_right, jzm_status_y - status_padding)
    ~font
    ~title:"bedford"
    ~f:(fun context ->
      text context ~font ~fill:black ~origin_x:20 ~baseline_y:45 ~size:30. "L");
  status_box
    context
    (subway_status_left, jzm_status_y)
    (status_right, h - status_padding)
    ~font
    ~title:"marcy"
    ~f:(fun context ->
      text context ~font ~fill:black ~origin_x:20 ~baseline_y:45 ~size:30. "J/Z/M");
  let bike_status_rx = subway_status_left - status_padding in
  let bridge_bike_status_ly =
    m_y - status_padding - Stroke.safe_padding (subway_stroke black)
  in
  available_bike_status
    context
    ~font
    ~title:"bridge"
    (Anchor.Lr (bike_status_rx, bridge_bike_status_ly));
  let roeb_bike_uy = j_y + Stroke.safe_padding (subway_stroke black) + status_padding in
  available_bike_status
    context
    ~font
    ~title:"roebling"
    (Anchor.Ur (bike_status_rx, roeb_bike_uy));
  parking_status
    context
    ~font
    ~title:"ves"
    (Anchor.Ul
       ( man_padding + Stroke.safe_padding geo_stroke + status_padding
       , h - man_padding - Stroke.safe_padding geo_stroke - man_inset - 70 ));
  parking_status
    context
    ~font
    ~title:"park"
    (Anchor.Lr
       ( j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2)
       , j_y - status_padding ));
  parking_status
    context
    ~font
    ~title:"chur"
    (Anchor.Ur
       (j_x - Stroke.safe_padding (subway_stroke black) - (status_padding / 2), j_y));
  image
;;

let render cache =
  let%bind citibike_stations = Citibike.query cache in
  let%map.Deferred.Or_error font =
    Font.create ~ttf_file:"server/fonts/inter_medium.ttf" |> return
  in
  let image = draw ~font in
  { Screen_render.buffer = image
  ; time_until_refresh = Time_ns.Span.of_sec 30.
  ; debug_info =
      citibike_stations
      |> Latest_result.map ~f:(fun stations ->
        Map.find stations "66dc8768-0aca-11e7-82f6-3863bb44ef7c")
      |> Latest_result.sexp_of_t (Option.sexp_of_t Citibike.Station.sexp_of_t)
      |> Sexp.to_string_hum
  }
;;
