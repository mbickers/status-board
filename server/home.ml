open! Core
open! Async

let draw ~font =
  let open Drawing.O in
  let w = 800
  and h = 480 in
  let image = Image.create_grey ~max_val:1 w h in
  let context = Drawing.Context.Clipped { width = w; height = h; image } in
  let black = Drawing.Fill.solid `b
  and land_fill = Drawing.Fill.solid `w
  and geo_stroke = Drawing.Stroke.solid `b 8 in
  rect context ~fill:land_fill (0, 0) (w, h);
  text context ~font ~fill:black ~origin_x:222 ~baseline_y:100 ~size:80. "hello world";
  let map_top = h / 2 in
  let man_fade_height = 20 in
  let man_faded_top = map_top - man_fade_height in
  let north_fade fill =
    Drawing.Fill.fade_to_white
      ~level:(fun (_, y) -> (y - man_faded_top) * 16 / man_fade_height)
      fill
  in
  let manhattan_stroke = { Drawing.Stroke.fill = north_fade black; width = 8 } in
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
  polygon context ~fill:land_fill manhattan_path;
  polygon context ~fill:land_fill brooklyn_polygon;
  rounded_path context ~radius:20 ~stroke:manhattan_stroke manhattan_path;
  rounded_path context ~radius:20 ~stroke:geo_stroke brooklyn_path;
  let subway_stroke fill = { Drawing.Stroke.fill; width = 8 } in
  let l_fill = Drawing.Fill.bayer 7 in
  let l_y = map_top + 30 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke l_fill)
    [ man_padding + 25, l_y; w, l_y ];
  let j_fill = Drawing.Fill.bayer 15 in
  let j_y = h - 100 in
  let j_x = man_w - man_inset - 25 in
  rounded_path
    context
    ~radius:20
    ~stroke:(subway_stroke j_fill)
    [ w, j_y; j_x, j_y; j_x, h - man_padding - 25 ];
  let m_fill = north_fade (Drawing.Fill.bayer ~offset:(1, 1) 6) in
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
