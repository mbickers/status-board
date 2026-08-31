open! Core

module Context = struct
  type t =
    | Image of Image.image
    | Clipped of
        { width : int
        ; height : int
        ; image : Image.image
        }

  let write t (x, y) color =
    let width, height, image =
      match t with
      | Image image -> image.Image.width, image.height, image
      | Clipped { width; height; image } ->
        Int.min width image.width, Int.min height image.height, image
    in
    if x >= 0 && y >= 0 && x < width && y < height
    then
      Image.write_grey
        image
        x
        y
        (match color with
         | `b -> 0
         | `w -> 1)
  ;;
end

module Fill = struct
  type t = int * int -> [ `b | `w ]

  let solid color _ = color

  let bayer_threshold (x, y) =
    let at x y =
      match y % 2, x % 2 with
      | 0, 0 -> 0
      | 0, 1 -> 2
      | 1, 0 -> 3
      | 1, 1 -> 1
      | _ -> 0
    in
    (4 * at x y) + at (x / 2) (y / 2)
  ;;

  let bayer ?(offset = 0, 0) level =
    let level = Int.max 0 (Int.min 16 level) in
    let offset_x, offset_y = offset in
    fun (x, y) -> if bayer_threshold (x + offset_x, y + offset_y) < level then `b else `w
  ;;

  let fade_to_white fill ~level point =
    if bayer_threshold point < Int.max 0 (Int.min 16 (level point))
    then fill point
    else `w
  ;;
end

module Stroke = struct
  type t =
    { fill : Fill.t
    ; width : int
    }

  let solid color width = { fill = Fill.solid color; width }
end

let rect context ~fill (x1, y1) (x2, y2) =
  for y = y1 to y2 - 1 do
    for x = x1 to x2 - 1 do
      Context.write context (x, y) (fill (x, y))
    done
  done
;;

let polygon context ~fill points =
  match points with
  | [] | [ _ ] | [ _; _ ] -> ()
  | first :: _ ->
    let rec edges = function
      | [] -> []
      | [ last ] -> [ last, first ]
      | start :: (finish :: _ as remaining) -> (start, finish) :: edges remaining
    in
    let min_y, max_y =
      List.fold
        points
        ~init:(Int.max_value, Int.min_value)
        ~f:(fun (min_y, max_y) (_, y) -> Int.min min_y y, Int.max max_y y)
    in
    for y = min_y to max_y - 1 do
      edges points
      |> List.filter_map ~f:(fun ((x1, y1), (x2, y2)) ->
        if (y1 <= y && y < y2) || (y2 <= y && y < y1)
        then
          Some
            (Float.of_int x1
             +. (Float.of_int (y - y1) *. Float.of_int (x2 - x1) /. Float.of_int (y2 - y1))
            )
        else None)
      |> List.sort ~compare:Float.compare
      |> fun intersections ->
      let rec fill_between_intersections = function
        | left :: right :: remaining ->
          for
            x = Int.of_float (Float.round_up left)
            to Int.of_float (Float.round_up right) - 1
          do
            Context.write context (x, y) (fill (x, y))
          done;
          fill_between_intersections remaining
        | [] | [ _ ] -> ()
      in
      fill_between_intersections intersections
    done
;;

let distance (x1, y1) (x2, y2) =
  Float.sqrt (((x2 -. x1) *. (x2 -. x1)) +. ((y2 -. y1) *. (y2 -. y1)))
;;

let draw_stroke_point context ~stroke (center_x, center_y) =
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
      then Context.write context (x, y) (stroke.fill (x, y))
    done
  done
;;

let draw_line context ~stroke ((x1, y1) as start) ((x2, y2) as finish) =
  let steps = Int.max 1 (distance start finish *. 2. |> Float.round_up |> Int.of_float) in
  for step = 0 to steps do
    let progress = Float.of_int step /. Float.of_int steps in
    draw_stroke_point
      context
      ~stroke
      (x1 +. ((x2 -. x1) *. progress), y1 +. ((y2 -. y1) *. progress))
  done
;;

let draw_quadratic_curve context ~stroke (start, control, finish) =
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
      context
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

let rounded_path context ~radius ~stroke points =
  let point (x, y) = Float.of_int x, Float.of_int y in
  match List.map points ~f:point with
  | [] -> ()
  | [ point ] -> draw_stroke_point context ~stroke point
  | first :: points ->
    let rec draw current previous = function
      | [] -> ()
      | [ last ] -> draw_line context ~stroke current last
      | vertex :: (next :: _ as remaining) ->
        let curve_start, curve_end =
          rounded_corner_tangent_points ~radius ~previous vertex ~next
        in
        draw_line context ~stroke current curve_start;
        draw_quadratic_curve context ~stroke (curve_start, vertex, curve_end);
        draw curve_end vertex remaining
    in
    draw first first points
;;

let text context ~font ~fill ~origin_x ~baseline_y ~size string =
  let rendered_text = Font.render_text font string ~size in
  for y = 0 to rendered_text.height - 1 do
    for x = 0 to rendered_text.width - 1 do
      if Bigarray.Array1.get rendered_text.buffer ((y * rendered_text.width) + x) >= 128
      then (
        let point =
          origin_x - rendered_text.origin_x + x, baseline_y - rendered_text.baseline_y + y
        in
        Context.write context point (fill point))
    done
  done
;;

module O = struct
  let rect = rect
  let polygon = polygon
  let draw_line = draw_line
  let draw_quadratic_curve = draw_quadratic_curve
  let rounded_path = rounded_path
  let text = text
end
