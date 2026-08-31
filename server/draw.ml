open! Core

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
