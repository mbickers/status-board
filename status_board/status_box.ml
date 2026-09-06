open! Core

module Style = struct
  type t =
    { font : Font.t
    ; base_padding : int
    ; primary_font_size : float
    ; label_size : float
    ; error_fill : Drawing.Fill.t
    }

  let create ~font ~base_padding ~primary_font_size ~label_size ~error_fill =
    { font; base_padding; primary_font_size; label_size; error_fill }
  ;;

  let font t = t.font
  let base_padding t = t.base_padding
  let horizontal_padding_between_text t = t.base_padding + 4
  let baseline_padding t = t.base_padding + 4
  let primary_font_size t = t.primary_font_size
  let label_size t = t.label_size
  let error_fill t = t.error_fill
end

let draw
      ?(fill = fun _ -> Drawing.Fill.solid `w)
      context
      (left, top)
      (right, bottom)
      ~style
      ~label
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
  and label_size = Style.label_size style in
  let rendered_label = Font.render_text font label ~size:label_size in
  Drawing.blit_rendered_text
    ~halo:(3, Drawing.Fill.solid `w)
    context
    ~fill:(Drawing.Fill.solid `b)
    ~origin_x:(left + radius + 2 + rendered_label.origin_x)
    ~baseline_y:(top - 2 + rendered_label.baseline_y)
    rendered_label
;;
