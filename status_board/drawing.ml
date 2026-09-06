open! Core

module Context = struct
  type t =
    { image : Image.image
    ; offset : int * int
    ; size : int * int
    }

  let create image =
    { image; offset = 0, 0; size = image.Image.width, image.Image.height }
  ;;

  let crop t ~size ~offset =
    let x, y = t.offset
    and offset_x, offset_y = offset in
    { t with offset = x + offset_x, y + offset_y; size }
  ;;

  let size t = t.size

  let write t (x, y) color =
    let width, height = t.size in
    match x >= 0 && y >= 0 && x < width && y < height with
    | true ->
      let offset_x, offset_y = t.offset
      and image = t.image in
      let x = x + offset_x
      and y = y + offset_y in
      (match x >= 0 && y >= 0 && x < image.width && y < image.height with
       | true ->
         Image.write_grey
           image
           x
           y
           (match color with
            | `b -> 0
            | `w -> 1)
       | false -> ())
    | false -> ()
  ;;
end

module Anchor = struct
  type t =
    | Ul of int * int
    | Ur of int * int
    | Ll of int * int
    | Lr of int * int

  let resolve t ~size:(width, height) =
    match t with
    | Ul (left, top) -> (left, top), (left + width, top + height)
    | Ur (right, top) -> (right - width, top), (right, top + height)
    | Ll (left, bottom) -> (left, bottom - height), (left + width, bottom)
    | Lr (right, bottom) -> (right - width, bottom - height), (right, bottom)
  ;;
end

module Fill = struct
  type t = int * int -> [ `b | `w ]

  let solid color _ = color

  let invert t point =
    match t point with
    | `b -> `w
    | `w -> `b
  ;;

  let rec bayer_matrix = function
    | 1 -> [| [| 0 |] |]
    | size ->
      let half_size = size / 2 in
      let smaller = bayer_matrix half_size in
      Array.init size ~f:(fun y ->
        Array.init size ~f:(fun x ->
          let quadrant_offset =
            match y / half_size, x / half_size with
            | 0, 0 -> 0
            | 0, 1 -> 2
            | 1, 0 -> 3
            | 1, 1 -> 1
            | _ -> 0
          in
          (4 * smaller.(y % half_size).(x % half_size)) + quadrant_offset))
  ;;

  let tile_index ~size coordinate =
    let index = coordinate % size in
    match index < 0 with
    | true -> index + size
    | false -> index
  ;;

  let bayer_threshold ~size =
    let matrix = bayer_matrix size in
    fun (x, y) ->
      Float.of_int matrix.(tile_index ~size y).(tile_index ~size x)
      /. Float.of_int (size * size)
  ;;

  let bayer_exn ?(size = 4) ?(offset = 0, 0) ~white_frac =
    (match size <= 0 || size land (size - 1) <> 0 with
     | true -> invalid_arg "Bayer matrix size must be a positive power of two"
     | false -> ());
    let threshold = bayer_threshold ~size in
    let offset_x, offset_y = offset in
    fun (x, y) ->
      match Float.compare (threshold (x + offset_x, y + offset_y)) white_frac >= 0 with
      | true -> `b
      | false -> `w
  ;;

  let fade_to fill ~color ~color_frac =
    let threshold = bayer_threshold ~size:16 in
    fun ((x, y) as point) ->
      match Float.compare (threshold (x, y)) (color_frac point) >= 0 with
      | true -> fill point
      | false -> color
  ;;

  let fractional ~frac ~frontier_angle_degrees context =
    let width, height = Context.size context in
    match Float.compare frac 0. <= 0, Float.compare frac 1. >= 0 with
    | true, _ -> solid `w
    | false, true -> solid `b
    | false, false ->
      let frontier_slope = Float.tan (frontier_angle_degrees *. Float.pi /. 180.) in
      fun (x, y) ->
        let frontier_at_center = Float.of_int width *. frac in
        let frontier =
          frontier_at_center
          +. ((Float.of_int y -. (Float.of_int height /. 2.)) *. frontier_slope)
        in
        (match Float.compare (Float.of_int x) frontier < 0 with
         | true -> `b
         | false -> `w)
  ;;
end

module Stroke = struct
  type t =
    { fill : Fill.t
    ; width : int
    ; casing : t option
    }

  let create ?casing fill width = { fill; width; casing }
  let solid ?casing color width = create ?casing (Fill.solid color) width

  let rec safe_padding t =
    Int.max (t.width / 2) (Option.value_map t.casing ~default:0 ~f:safe_padding)
  ;;
end

let rect context ~fill (x1, y1) (x2, y2) =
  for y = y1 to y2 - 1 do
    for x = x1 to x2 - 1 do
      Context.write context (x, y) (fill (x, y))
    done
  done
;;

module Path_resolver_step = struct
  type t =
    | Point of int * int
    | Offset of int * int

  let resolve steps =
    List.folding_map steps ~init:(0, 0) ~f:(fun (x, y) step ->
      let point =
        match step with
        | Point (point_x, point_y) -> point_x, point_y
        | Offset (x_offset, y_offset) -> x + x_offset, y + y_offset
      in
      point, point)
  ;;
end

let fill_polygon context ~fill points =
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
        ~init:(Float.infinity, Float.neg_infinity)
        ~f:(fun (min_y, max_y) (_, y) -> Float.min min_y y, Float.max max_y y)
    in
    for
      y = Int.of_float (Float.round_down min_y) to Int.of_float (Float.round_up max_y) - 1
    do
      edges points
      |> List.filter_map ~f:(fun ((x1, y1), (x2, y2)) ->
        let y = Float.of_int y in
        match Float.(y1 <= y && y < y2) || Float.(y2 <= y && y < y1) with
        | true -> Some (x1 +. ((y -. y1) *. (x2 -. x1) /. (y2 -. y1)))
        | false -> None)
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

let polygon context ~fill points =
  points
  |> List.map ~f:(fun (x, y) -> Float.of_int x, Float.of_int y)
  |> fill_polygon context ~fill
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
      match
        Float.O.(
          (x_distance * x_distance) + (y_distance * y_distance)
          <= stroke_radius * stroke_radius)
      with
      | true -> Context.write context (x, y) (stroke.fill (x, y))
      | false -> ()
    done
  done
;;

let circle context ~fill ~center:(center_x, center_y) ~radius =
  for y = center_y - radius to center_y + radius do
    for x = center_x - radius to center_x + radius do
      let x_distance = x - center_x
      and y_distance = y - center_y in
      match (x_distance * x_distance) + (y_distance * y_distance) <= radius * radius with
      | true -> Context.write context (x, y) (fill (x, y))
      | false -> ()
    done
  done
;;

let draw_line_without_casing context ~stroke ((x1, y1) as start) ((x2, y2) as finish) =
  let steps = Int.max 1 (distance start finish *. 2. |> Float.round_up |> Int.of_float) in
  for step = 0 to steps do
    let progress = Float.of_int step /. Float.of_int steps in
    draw_stroke_point
      context
      ~stroke
      (x1 +. ((x2 -. x1) *. progress), y1 +. ((y2 -. y1) *. progress))
  done
;;

let rec draw_line context ~stroke start finish =
  Option.iter stroke.Stroke.casing ~f:(fun casing ->
    draw_line context ~stroke:casing start finish);
  draw_line_without_casing context ~stroke start finish
;;

let quadratic_curve_points (start, control, finish) =
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
  List.init (steps + 1) ~f:(fun step ->
    let progress = Float.of_int step /. Float.of_int steps in
    let remaining = 1. -. progress in
    ( (remaining *. remaining *. x1)
      +. (2. *. remaining *. progress *. control_x)
      +. (progress *. progress *. x2)
    , (remaining *. remaining *. y1)
      +. (2. *. remaining *. progress *. control_y)
      +. (progress *. progress *. y2) ))
;;

let star context ~stroke ~radius ~center:(center_x, center_y) =
  let diagonal_x =
    Float.of_int radius *. Float.cos (Float.pi /. 6.) |> Float.iround_nearest_exn
  and diagonal_y =
    Float.of_int radius *. Float.sin (Float.pi /. 6.) |> Float.iround_up_exn
  in
  let float_point (x, y) = Float.of_int x, Float.of_int y in
  List.iter
    [ (center_x, center_y - radius), (center_x, center_y + radius)
    ; ( (center_x - diagonal_x, center_y - diagonal_y)
      , (center_x + diagonal_x, center_y + diagonal_y) )
    ; ( (center_x - diagonal_x, center_y + diagonal_y)
      , (center_x + diagonal_x, center_y - diagonal_y) )
    ]
    ~f:(fun (start, finish) ->
      draw_line context ~stroke (float_point start) (float_point finish))
;;

let draw_quadratic_curve_without_casing context ~stroke points =
  List.iter (quadratic_curve_points points) ~f:(draw_stroke_point context ~stroke)
;;

let rec draw_quadratic_curve context ~stroke points =
  Option.iter stroke.Stroke.casing ~f:(fun casing ->
    draw_quadratic_curve context ~stroke:casing points);
  draw_quadratic_curve_without_casing context ~stroke points
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
    match Float.equal length 0. with
    | true -> vertex
    | false ->
      ( vertex_x +. ((x -. vertex_x) *. corner_length /. length)
      , vertex_y +. ((y -. vertex_y) *. corner_length /. length) )
  in
  tangent_point previous previous_length, tangent_point next next_length
;;

let rec rounded_path context ~radius ~stroke points =
  Option.iter stroke.Stroke.casing ~f:(fun casing ->
    rounded_path context ~radius ~stroke:casing points);
  let stroke = { stroke with casing = None } in
  match List.map points ~f:(fun (x, y) -> Float.of_int x, Float.of_int y) with
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

let rec stroke_closed_path context ~stroke points =
  Option.iter stroke.Stroke.casing ~f:(fun casing ->
    stroke_closed_path context ~stroke:casing points);
  let stroke = { stroke with casing = None } in
  match points with
  | [] | [ _ ] -> ()
  | first :: _ ->
    let rec draw = function
      | [] -> ()
      | [ last ] -> draw_line_without_casing context ~stroke last first
      | start :: (finish :: _ as remaining) ->
        draw_line_without_casing context ~stroke start finish;
        draw remaining
    in
    draw points
;;

let rounded_polygon context ~radius ~fill ?stroke ?(round_corner = fun _ -> true) points =
  match List.map points ~f:(fun (x, y) -> Float.of_int x, Float.of_int y) with
  | [] | [ _ ] | [ _; _ ] -> ()
  | points ->
    let points = Array.of_list points in
    let point_count = Array.length points in
    let rounded_points =
      Array.mapi points ~f:(fun index vertex ->
        match round_corner index with
        | false -> [ vertex ]
        | true ->
          let previous = points.((index + point_count - 1) % point_count)
          and next = points.((index + 1) % point_count) in
          let curve_start, curve_end =
            rounded_corner_tangent_points ~radius ~previous vertex ~next
          in
          quadratic_curve_points (curve_start, vertex, curve_end))
      |> Array.to_list
      |> List.concat
    in
    fill_polygon context ~fill rounded_points;
    Option.iter stroke ~f:(fun stroke ->
      stroke_closed_path context ~stroke rounded_points)
;;

let blit_rendered_text
      ?halo
      context
      ~fill
      ~origin_x
      ~baseline_y
      (rendered_text : Font.Rendered_text.t)
  =
  let iter_black_pixels ~f =
    for y = 0 to rendered_text.height - 1 do
      for x = 0 to rendered_text.width - 1 do
        match
          Bigarray.Array1.get rendered_text.buffer ((y * rendered_text.width) + x) >= 128
        with
        | true ->
          f
            ( origin_x - rendered_text.origin_x + x
            , baseline_y - rendered_text.baseline_y + y )
        | false -> ()
      done
    done
  in
  Option.iter halo ~f:(fun (distance, halo_fill) ->
    iter_black_pixels ~f:(fun (x, y) ->
      for dy = -distance to distance do
        for dx = -distance to distance do
          match (dx * dx) + (dy * dy) <= distance * distance with
          | true ->
            let point = x + dx, y + dy in
            Context.write context point (halo_fill point)
          | false -> ()
        done
      done));
  iter_black_pixels ~f:(fun point -> Context.write context point (fill point))
;;

let text ?halo context ~font ~fill ~origin_x ~baseline_y ~size string =
  let rendered_text = Font.render_text font string ~size in
  blit_rendered_text ?halo context ~fill ~origin_x ~baseline_y rendered_text
;;

module Graph = struct
  module Tick = struct
    type t =
      { position_frac : float
      ; label : string option
      }
  end

  module Point = struct
    type t =
      { x_frac : float
      ; y_frac : float option
      }
  end

  module Style = struct
    type t =
      { font : Font.t
      ; label_size : float
      ; label_fill : Fill.t
      ; label_halo : (int * Fill.t) option
      ; stroke : Stroke.t
      ; tick_length : int
      ; labeled_tick_length : int
      }
  end

  let draw
        context
        ~bottom_center:(center_x, bottom_y)
        ~size:(width, height)
        ~style
        ~x_ticks
        ~y_ticks
        ~points
    =
    let { Style.font
        ; label_size
        ; label_fill
        ; label_halo
        ; stroke
        ; tick_length
        ; labeled_tick_length
        }
      =
      style
    in
    let render_ticks ticks =
      List.map ticks ~f:(fun tick ->
        ( tick.Tick.position_frac
        , Option.map tick.label ~f:(fun label ->
            Font.render_text font label ~size:label_size) ))
    in
    let x_ticks = render_ticks x_ticks
    and y_ticks = render_ticks y_ticks in
    let offset_x = center_x - (width / 2)
    and offset_y = bottom_y - height in
    let padding =
      Int.max (Option.value_map label_halo ~default:0 ~f:fst) (Stroke.safe_padding stroke)
    in
    let label_gap = 2 in
    let max_label_size ticks =
      List.fold ticks ~init:(0, 0) ~f:(fun (width, height) (_, rendered) ->
        match rendered with
        | None -> width, height
        | Some text ->
          Int.max width text.Font.Rendered_text.width, Int.max height text.height)
    in
    let y_label_width, y_label_height = max_label_size y_ticks
    and x_label_width, x_label_height = max_label_size x_ticks in
    let left =
      offset_x + padding + Int.max (y_label_width + label_gap + tick_length) x_label_width
    and right = offset_x + width - padding - 1
    and top = offset_y + padding + (y_label_height / 2)
    and bottom =
      offset_y + height - padding - x_label_height - label_gap - tick_length - 1
    in
    let x position_frac =
      Float.of_int left +. (position_frac *. Float.of_int (right - left))
    and y position_frac =
      Float.of_int bottom -. (position_frac *. Float.of_int (bottom - top))
    in
    draw_line context ~stroke (x 0., y 1.) (x 0., y 0.);
    draw_line context ~stroke (x 0., y 0.) (x 1., y 0.);
    List.iter y_ticks ~f:(fun (position_frac, rendered) ->
      let tick_length =
        match rendered with
        | None -> tick_length
        | Some _ -> labeled_tick_length
      in
      let tick_y = y position_frac in
      draw_line context ~stroke (Float.of_int (left - tick_length), tick_y) (x 0., tick_y);
      Option.iter rendered ~f:(fun rendered ->
        blit_rendered_text
          ?halo:label_halo
          context
          ~fill:label_fill
          ~origin_x:(left - tick_length - label_gap - rendered.width + rendered.origin_x)
          ~baseline_y:(Float.iround_nearest_exn tick_y + rendered.baseline_y)
          rendered));
    let label_shift = 5 in
    List.iter x_ticks ~f:(fun (position_frac, rendered) ->
      let tick_length =
        match rendered with
        | None -> tick_length
        | Some _ -> labeled_tick_length
      in
      let tick_x = x position_frac in
      draw_line
        context
        ~stroke
        (tick_x, y 0.)
        (tick_x, Float.of_int (bottom + tick_length));
      Option.iter rendered ~f:(fun rendered ->
        let label_left = Float.iround_nearest_exn tick_x - rendered.width + label_shift in
        blit_rendered_text
          ?halo:label_halo
          context
          ~fill:label_fill
          ~origin_x:(label_left + rendered.origin_x)
          ~baseline_y:(bottom + tick_length + label_gap + rendered.baseline_y + 2)
          rendered));
    ignore
      (List.fold points ~init:None ~f:(fun previous point ->
         match point.Point.y_frac with
         | None -> None
         | Some y_frac ->
           let point = x point.x_frac, y y_frac in
           draw_line context ~stroke (Option.value previous ~default:point) point;
           Some point)
       : (float * float) option);
    let missing_label = lazy (Font.render_text font "?" ~size:label_size) in
    ignore
      (List.fold points ~init:0.5 ~f:(fun last_y_frac point ->
         match point.Point.y_frac with
         | Some y_frac -> y_frac
         | None ->
           let rendered = Lazy.force missing_label in
           blit_rendered_text
             ?halo:label_halo
             context
             ~fill:label_fill
             ~origin_x:
               (Float.iround_nearest_exn (x point.x_frac)
                - (rendered.width / 2)
                + rendered.origin_x)
             ~baseline_y:
               (Float.iround_nearest_exn (y last_y_frac)
                - (rendered.height / 2)
                + rendered.baseline_y)
             rendered;
           last_y_frac)
       : float)
  ;;
end

module O = struct
  module Graph = Graph
  module Context = Context
  module Anchor = Anchor
  module Fill = Fill
  module Path_resolver_step = Path_resolver_step
  module Stroke = Stroke

  let solid = Fill.solid
  let invert = Fill.invert
  let bayer_exn = Fill.bayer_exn
  let fade_to = Fill.fade_to
  let rect = rect
  let polygon = polygon
  let circle = circle
  let star = star
  let draw_line = draw_line
  let draw_quadratic_curve = draw_quadratic_curve
  let rounded_path = rounded_path
  let rounded_polygon = rounded_polygon
  let blit_rendered_text = blit_rendered_text
  let text = text
end
