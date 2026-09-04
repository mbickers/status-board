open! Core

module Style = struct
  type t =
    { font : Font.t
    ; horizontal_padding : int
    ; horizontal_padding_between_text : int
    ; baseline_padding : int
    ; primary_font_size : float
    }

  let create
        ~font
        ~horizontal_padding
        ~horizontal_padding_between_text
        ~baseline_padding
        ~primary_font_size
    =
    { font
    ; horizontal_padding
    ; horizontal_padding_between_text
    ; baseline_padding
    ; primary_font_size
    }
  ;;

  let font t = t.font
  let horizontal_padding t = t.horizontal_padding
  let horizontal_padding_between_text t = t.horizontal_padding_between_text
  let baseline_padding t = t.baseline_padding
  let primary_font_size t = t.primary_font_size
end

let draw
      ?(fill = fun _ -> Drawing.Fill.solid `w)
      context
      (left, top)
      (right, bottom)
      ~style
      ~title
      ~f
  =
  let radius = 10
  and stroke_width = 4
  and stroke_fill = Drawing.Fill.solid `b in
  let box_context =
    Drawing.Context.crop context ~offset:(left, top) ~size:(right - left, bottom - top)
  in
  let fill = fill box_context in
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
  let iter_pixels ~f =
    for y = top to bottom - 1 do
      for x = left to right - 1 do
        match inside ~inset:0 (x, y) with
        | true -> f (x, y) ~is_interior:(inside ~inset:stroke_width (x, y))
        | false -> ()
      done
    done
  in
  iter_pixels ~f:(fun ((x, y) as point) ~is_interior ->
    Drawing.Context.write
      context
      point
      (match is_interior with
       | true -> fill (x - left, y - top)
       | false -> stroke_fill point));
  f box_context ~fill;
  iter_pixels ~f:(fun point ~is_interior ->
    match is_interior with
    | true -> ()
    | false -> Drawing.Context.write context point (stroke_fill point));
  let font = Style.font style
  and title_font_size = 17. in
  let rendered_title = Font.render_text font title ~size:title_font_size in
  Drawing.text
    ~halo:(3, Drawing.Fill.solid `w)
    context
    ~font
    ~fill:(Drawing.Fill.solid `b)
    ~origin_x:(left + radius + 2 + rendered_title.origin_x)
    ~baseline_y:(top - 2 + rendered_title.baseline_y)
    ~size:title_font_size
    title
;;
