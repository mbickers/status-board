open! Core
open! Async

module Fill = struct
  type t = int * int -> [ `b | `w ]

  let solid color _ = color

  let bayer level =
    let level = Int.max 0 (Int.min 16 level) in
    let tile =
      [| [| 0; 8; 2; 10 |]; [| 12; 4; 14; 6 |]; [| 3; 11; 1; 9 |]; [| 15; 7; 13; 5 |] |]
    in
    fun (x, y) -> if tile.(y % 4).(x % 4) < level then `b else `w
  ;;
end

module Stroke = struct
  type t =
    { fill : Fill.t
    ; width : int
    }

  let solid color width = { fill = Fill.solid color; width }
end

let write buffer ((x, y) as point) fill =
  if x >= 0 && y >= 0 && x < buffer.Image.width && y < buffer.height
  then
    Image.write_grey
      buffer
      x
      y
      (match fill point with
       | `b -> 0
       | `w -> 1)
;;

let rect buffer ~fill (x1, y1) (x2, y2) =
  for y = y1 to y2 - 1 do
    for x = x1 to x2 - 1 do
      write buffer (x, y) fill
    done
  done
;;

let distance (x1, y1) (x2, y2) =
  Float.sqrt (((x2 -. x1) *. (x2 -. x1)) +. ((y2 -. y1) *. (y2 -. y1)))
;;

let draw_stroke_point buffer ~stroke (center_x, center_y) =
  let stroke_radius = Float.of_int stroke.Stroke.width /. 2. in
  for
    y = Int.of_float (Float.round_down (center_y -. stroke_radius))
    to Int.of_float (Float.round_up (center_y +. stroke_radius))
  do
    for
      x = Int.of_float (Float.round_down (center_x -. stroke_radius))
      to Int.of_float (Float.round_up (center_x +. stroke_radius))
    do
      let x_distance = Float.of_int x -. center_x
      and y_distance = Float.of_int y -. center_y in
      if
        Float.O.(
          (x_distance * x_distance) + (y_distance * y_distance)
          <= stroke_radius * stroke_radius)
      then write buffer (x, y) stroke.fill
    done
  done
;;

let draw_line buffer ~stroke ((x1, y1) as start) ((x2, y2) as finish) =
  let steps = Int.max 1 (distance start finish *. 2. |> Float.round_up |> Int.of_float) in
  for step = 0 to steps do
    let progress = Float.of_int step /. Float.of_int steps in
    draw_stroke_point
      buffer
      ~stroke
      (x1 +. ((x2 -. x1) *. progress), y1 +. ((y2 -. y1) *. progress))
  done
;;

let draw_quadratic_curve buffer ~stroke (start, control, finish) =
  let steps =
    Int.max
      1
      ((distance start control +. distance control finish) *. 2.
       |> Float.round_up
       |> Int.of_float)
  in
  let x1, y1 = start
  and control_x, control_y = control
  and x2, y2 = finish in
  for step = 0 to steps do
    let progress = Float.of_int step /. Float.of_int steps in
    let remaining = 1. -. progress in
    draw_stroke_point
      buffer
      ~stroke
      ( (remaining *. remaining *. x1)
        +. (2. *. remaining *. progress *. control_x)
        +. (progress *. progress *. x2)
      , (remaining *. remaining *. y1)
        +. (2. *. remaining *. progress *. control_y)
        +. (progress *. progress *. y2) )
  done
;;

let rounded_corner_tangent_points ~radius ~previous ((vertex_x, vertex_y) as vertex) ~next
  =
  let previous_length = distance vertex previous
  and next_length = distance vertex next in
  let corner_length =
    Float.min
      (Float.of_int radius)
      (Float.min (previous_length /. 2.) (next_length /. 2.))
  in
  let tangent_point (x, y) length =
    if Float.equal length 0.
    then vertex
    else
      ( vertex_x +. ((x -. vertex_x) *. corner_length /. length)
      , vertex_y +. ((y -. vertex_y) *. corner_length /. length) )
  in
  tangent_point previous previous_length, tangent_point next next_length
;;

let rounded_path buffer ~radius ~stroke points =
  let point (x, y) = Float.of_int x, Float.of_int y in
  match List.map points ~f:point with
  | [] -> ()
  | [ point ] -> draw_stroke_point buffer ~stroke point
  | first :: points ->
    let rec draw current previous = function
      | [] -> ()
      | [ last ] -> draw_line buffer ~stroke current last
      | vertex :: (next :: _ as remaining) ->
        let curve_start, curve_end =
          rounded_corner_tangent_points ~radius ~previous vertex ~next
        in
        draw_line buffer ~stroke current curve_start;
        draw_quadratic_curve buffer ~stroke (curve_start, vertex, curve_end);
        draw curve_end vertex remaining
    in
    draw first first points
;;

let text buffer ~font ~fill ~origin_x ~baseline_y ~size string =
  let rendered_text = Font.render_text font string ~size in
  for y = 0 to rendered_text.height - 1 do
    for x = 0 to rendered_text.width - 1 do
      if Bigarray.Array1.get rendered_text.buffer ((y * rendered_text.width) + x) >= 128
      then
        write
          buffer
          ( origin_x - rendered_text.origin_x + x
          , baseline_y - rendered_text.baseline_y + y )
          fill
    done
  done
;;

let draw ~font =
  let w = 800
  and h = 480 in
  let buffer = Image.create_grey ~max_val:1 w h in
  let black = Fill.solid `b in
  let geo_stroke = Stroke.solid `b 8 in
  rect buffer ~fill:(Fill.solid `w) (0, 0) (w, h);
  text buffer ~font ~fill:black ~origin_x:222 ~baseline_y:100 ~size:80. "hello world";
  let map_top = h / 2 in
  let man_w = 250 in
  let man_padding = 10 in
  let man_inset = 50 in
  rounded_path
    buffer
    ~radius:20
    ~stroke:geo_stroke
    [ man_padding, map_top
    ; man_padding, h - man_padding - man_inset
    ; man_padding + man_inset, h - man_padding
    ; man_w - man_inset, h - man_padding
    ; man_w, h - man_padding - man_inset
    ; man_w, map_top + man_inset + 20
    ; man_w - man_inset, map_top + 20
    ; man_w - man_inset, map_top
    ];
  let river_w = 100 in
  let br_start = man_w + river_w in
  let br_foot = 50 in
  rounded_path
    buffer
    ~radius:20
    ~stroke:geo_stroke
    [ w, map_top; br_start, map_top; br_start, h - br_foot; br_start - br_foot, h ];
  let subway_stroke fill = { Stroke.fill; width = 8 } in
  let l_fill = Fill.bayer 7 in
  let l_y = map_top + 30 in
  rounded_path
    buffer
    ~radius:20
    ~stroke:(subway_stroke l_fill)
    [ man_padding + 25, l_y; w, l_y ];
  let j_fill = Fill.bayer 15 in
  let j_y = h - 100 in
  let j_x = man_w - man_inset - 25 in
  rounded_path
    buffer
    ~radius:20
    ~stroke:(subway_stroke j_fill)
    [ w, j_y; j_x, j_y; j_x, h - man_padding - 25 ];
  let m_fill = Fill.bayer 4 in
  let m_y = j_y - 12 in
  let m_diag = 25 in
  let m_vert_x = man_w - (man_inset * 2) in
  rounded_path
    buffer
    ~radius:20
    ~stroke:(subway_stroke m_fill)
    [ w, m_y
    ; man_w - m_diag, m_y
    ; man_w - m_diag, m_y - man_inset
    ; m_vert_x, m_y - man_inset
    ; m_vert_x, map_top
    ];
  buffer
;;

let render cache =
  let%bind citibike_stations = Citibike.query cache in
  let%map.Deferred.Or_error font =
    Font.create ~ttf_file:"server/fonts/inter_medium.ttf" |> return
  in
  let buffer = draw ~font in
  { Screen_render.buffer
  ; time_until_refresh = Time_ns.Span.of_sec 30.
  ; debug_info =
      citibike_stations
      |> Latest_result.map ~f:(fun stations ->
        Map.find stations "66dc8768-0aca-11e7-82f6-3863bb44ef7c")
      |> Latest_result.sexp_of_t (Option.sexp_of_t Citibike.Station.sexp_of_t)
      |> Sexp.to_string_hum
  }
;;
