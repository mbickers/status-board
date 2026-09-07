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

let create ?(fill = Drawing.Fill.solid `w) ~style ~label ~content () =
  let size = Drawing.Element.size content in
  let label =
    Drawing.Text.create
      ~font:(Style.font style)
      ~size:(Style.label_size style)
      ~fill:(Drawing.Fill.solid `b)
      ~halo:(3, Drawing.Fill.solid `w)
      label
  in
  Drawing.Element.create
    ~size
    ~draw:(fun context ~upper_left:(left, top) ->
      let width, height = size in
      let right = left + width
      and bottom = top + height in
      let radius = 10
      and stroke_width = 4
      and stroke_fill = Drawing.Fill.solid `b in
      let box_context =
        Drawing.Context.crop context ~offset:(left, top) ~size:(right - left, bottom - top)
      in
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
      Drawing.Element.draw content box_context ((Left, 0), (Top, 0));
      iter_pixels ~f:(fun point ~is_interior ->
        match is_interior with
        | true -> ()
        | false -> Drawing.Context.write context point (stroke_fill point));
      Drawing.Element.draw label context ((Left, left + radius + 2), (Top, top - 2)))
    ()
;;
