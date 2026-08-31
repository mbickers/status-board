open! Core
open! Async

let status_box context (left, top) (right, bottom) ~f =
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
       ~size:(right - left - (2 * safe_padding), bottom - top - (2 * safe_padding)))
;;

let draw ~font =
  let open Drawing.O in
  let w = 800
  and h = 480 in
  let image = Image.create_grey ~max_val:1 w h in
  let context = Context.create image in
  let black = Fill.solid `b
  and land_fill = Fill.solid `w
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
  let l_fill = Fill.bayer 7 in
  let l_y = map_top + 30 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke l_fill)
    [ man_padding + 25, l_y; w, l_y ];
  let j_fill = Fill.bayer 15 in
  let j_y = h - 100 in
  let j_x = man_w - man_inset - 25 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke j_fill)
    [ w, j_y; j_x, j_y; j_x, h - man_padding - 25 ];
  let m_fill = north_fade (Fill.bayer ~offset:(1, 1) 6) in
  let m_y = j_y - 12 in
  let m_diag = 25 in
  let m_vert_x = man_w - (man_inset * 2) in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke m_fill)
    [ w, m_y
    ; man_w - m_diag, m_y
    ; man_w - m_diag, m_y - man_inset
    ; m_vert_x, m_y - man_inset
    ; m_vert_x, man_faded_top
    ];
  let status_padding = 8 in
  let status_left = w - 250 - status_padding in
  let status_right = w - status_padding in
  let jzm_status_y = m_y - 40 in
  status_box
    context
    (status_left, map_top + status_padding + Stroke.safe_padding geo_stroke)
    (status_right, jzm_status_y - status_padding)
    ~f:(fun context ->
      text context ~font ~fill:black ~origin_x:20 ~baseline_y:45 ~size:30. "L");
  status_box
    context
    (status_left, jzm_status_y)
    (status_right, h - status_padding)
    ~f:(fun context ->
      text context ~font ~fill:black ~origin_x:20 ~baseline_y:45 ~size:30. "J/Z/M");
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
