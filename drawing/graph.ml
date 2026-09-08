open! Core

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
    ; label_fill : Primitives.Fill.t
    ; label_halo : (int * Primitives.Fill.t) option
    ; stroke : Primitives.Stroke.t
    ; tick_length : int
    ; labeled_tick_length : int
    }
end

let create ~size:(width, height) ~style ~x_ticks ~y_ticks ~points =
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
          Primitives.text ?halo:label_halo ~font ~fill:label_fill label ~size:label_size)
      ))
  in
  let x_ticks = render_ticks x_ticks
  and y_ticks = render_ticks y_ticks in
  let padding =
    Int.max
      (Option.value_map label_halo ~default:0 ~f:fst)
      (Primitives.Stroke.safe_padding stroke)
  in
  let label_gap = 2 in
  let max_label_size ticks =
    List.fold ticks ~init:(0, 0) ~f:(fun (width, height) (_, rendered) ->
      match rendered with
      | None -> width, height
      | Some text ->
        let text_width, text_height = Element.size text in
        Int.max width text_width, Int.max height text_height)
  in
  let y_label_width, y_label_height = max_label_size y_ticks
  and x_label_width, x_label_height = max_label_size x_ticks in
  let missing_label =
    lazy (Primitives.text ?halo:label_halo ~font ~fill:label_fill "?" ~size:label_size)
  in
  Element.create
    ~size:(width, height)
    ~draw:(fun context ~upper_left:(offset_x, offset_y) ->
      let left =
        offset_x
        + padding
        + Int.max (y_label_width + label_gap + tick_length) x_label_width
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
      Primitives.draw_line context ~stroke (x 0., y 1.) (x 0., y 0.);
      Primitives.draw_line context ~stroke (x 0., y 0.) (x 1., y 0.);
      List.iter y_ticks ~f:(fun (position_frac, rendered) ->
        let tick_length =
          match rendered with
          | None -> tick_length
          | Some _ -> labeled_tick_length
        in
        let tick_y = y position_frac in
        Primitives.draw_line
          context
          ~stroke
          (Float.of_int (left - tick_length), tick_y)
          (x 0., tick_y);
        Option.iter rendered ~f:(fun rendered ->
          Element.draw
            rendered
            context
            ( (Right, left - tick_length - label_gap)
            , (Top, Float.iround_nearest_exn tick_y) )));
      let label_shift = 5 in
      List.iter x_ticks ~f:(fun (position_frac, rendered) ->
        let tick_length =
          match rendered with
          | None -> tick_length
          | Some _ -> labeled_tick_length
        in
        let tick_x = x position_frac in
        Primitives.draw_line
          context
          ~stroke
          (tick_x, y 0.)
          (tick_x, Float.of_int (bottom + tick_length));
        Option.iter rendered ~f:(fun rendered ->
          Element.draw
            rendered
            context
            ( (Right, Float.iround_nearest_exn tick_x + label_shift)
            , (Top, bottom + tick_length + label_gap + 2) )));
      ignore
        (List.fold points ~init:None ~f:(fun previous point ->
           match point.Point.y_frac with
           | None -> None
           | Some y_frac ->
             let point = x point.x_frac, y y_frac in
             Primitives.draw_line
               context
               ~stroke
               (Option.value previous ~default:point)
               point;
             Some point)
         : (float * float) option);
      ignore
        (List.fold points ~init:0.5 ~f:(fun last_y_frac point ->
           match point.y_frac with
           | Some y_frac -> y_frac
           | None ->
             let rendered = Lazy.force missing_label in
             Element.draw
               rendered
               context
               ( (Center, Float.iround_nearest_exn (x point.x_frac))
               , (Center, Float.iround_nearest_exn (y last_y_frac)) );
             last_y_frac)
         : float))
    ()
;;
