open! Core

module Row = struct
  type t =
    { bullet : string * Drawing.Fill.t
    ; route_ids : string list
    ; minimum_minutes : int
    ; westbound_mta_direction : string
    }
end

let width = 340

let draw_bullet
      context
      ~font
      ~font_size
      ~fill
      ~text_fill
      ~label
      ~radius
      ~center:(center_x, center_y)
  =
  for y = center_y - radius to center_y + radius do
    for x = center_x - radius to center_x + radius do
      if
        ((x - center_x) * (x - center_x)) + ((y - center_y) * (y - center_y))
        <= radius * radius
      then Drawing.Context.write context (x, y) (fill (x, y))
    done
  done;
  let rendered_text = Font.render_text font label ~size:font_size in
  Drawing.text
    context
    ~font
    ~fill:text_fill
    ~origin_x:(center_x - (rendered_text.width / 2) + rendered_text.origin_x)
    ~baseline_y:(center_y - (rendered_text.height / 2) + rendered_text.baseline_y)
    ~size:font_size
    label
;;

let columns context ~bullet_center_x ~bullet_radius ~padding =
  let width, _ = Drawing.Context.size context in
  let left = bullet_center_x + bullet_radius + padding
  and right = width - padding in
  left, (left + right) / 2, right
;;

let draw_directions
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~center_y
      ~half_height
  =
  let left, middle, right = columns context ~bullet_center_x ~bullet_radius ~padding
  and stroke = Drawing.Stroke.solid `b 2 in
  let draw_arrow direction ~center_x =
    let tip_x, tail_x, arrowhead_x =
      match direction with
      | `Left -> center_x - 9, center_x + 9, center_x - 3
      | `Right -> center_x + 9, center_x - 9, center_x + 3
    in
    let tip = Float.of_int tip_x, center_y in
    Drawing.draw_line context ~stroke (Float.of_int tail_x, center_y) tip;
    Drawing.draw_line
      context
      ~stroke
      tip
      (Float.of_int arrowhead_x, center_y -. half_height);
    Drawing.draw_line
      context
      ~stroke
      tip
      (Float.of_int arrowhead_x, center_y +. half_height)
  in
  draw_arrow `Left ~center_x:((left + middle) / 2);
  draw_arrow `Right ~center_x:((middle + right) / 2)
;;

let draw_row
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~font
      ~bullet_font_size
      ~departure_font_size
      ~now
      ~(stop_status : Mta_subway.Stop_status.t)
      ~center_y
      (row : Row.t)
  =
  let minimum_time_until_arrival = Time_ns.Span.of_int_min row.minimum_minutes in
  let departure_text direction =
    let minutes =
      List.filter_map stop_status.upcoming_arrivals ~f:(fun arrival ->
        let time_until_arrival = Time_ns.diff arrival.arrives_at now in
        if
          List.mem row.route_ids arrival.route_id ~equal:String.equal
          && String.is_suffix arrival.stop_id ~suffix:direction
          && Time_ns.Span.compare time_until_arrival minimum_time_until_arrival > 0
        then Some (Time_ns.Span.to_min time_until_arrival |> Float.iround_up_exn)
        else None)
    in
    List.take minutes 3
    |> List.map ~f:Int.to_string
    |> function
    | [] -> "-"
    | minutes -> String.concat minutes ~sep:","
  in
  let label, fill = row.bullet in
  let eastbound_direction =
    if String.equal row.westbound_mta_direction "N" then "S" else "N"
  and bullet_text_fill = Drawing.Fill.solid `w
  and text_left, text_middle, text_right =
    columns context ~bullet_center_x ~bullet_radius ~padding
  in
  let draw_centered_text text ~left ~right =
    let rendered_text = Font.render_text font text ~size:departure_font_size in
    Drawing.text
      context
      ~font
      ~fill:(Drawing.Fill.solid `b)
      ~origin_x:
        (left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
      ~baseline_y:(center_y - (rendered_text.height / 2) + rendered_text.baseline_y)
      ~size:departure_font_size
      text
  in
  draw_bullet
    context
    ~font
    ~font_size:bullet_font_size
    ~fill
    ~text_fill:bullet_text_fill
    ~label
    ~radius:bullet_radius
    ~center:(bullet_center_x, center_y - 6);
  draw_centered_text
    (departure_text row.westbound_mta_direction)
    ~left:text_left
    ~right:text_middle;
  draw_centered_text
    (departure_text eastbound_direction)
    ~left:text_middle
    ~right:text_right
;;

let draw context ~anchor ~font ~title ~now ~stop_status ~rows =
  let padding = 8
  and bullet_radius = 20
  and arrow_half_height = 6.
  and bullet_font_size = 35.
  and departure_font_size = 40. in
  let bullet_center_x = padding + bullet_radius
  and arrow_center_y = Float.of_int padding +. arrow_half_height
  and departure_line_height =
    (Font.render_text font "0" ~size:departure_font_size).height
  in
  let first_row_center_y =
    Int.of_float (arrow_center_y +. arrow_half_height)
    + padding
    + (departure_line_height / 2)
    + 3
  and row_height = Int.max (2 * bullet_radius) departure_line_height + padding in
  let box_height =
    first_row_center_y
    + ((List.length rows - 1) * row_height)
    + bullet_radius
    + padding
    - 2
  in
  let upper_left, lower_right = Drawing.Anchor.resolve anchor ~size:(width, box_height) in
  Drawing.status_box
    context
    upper_left
    lower_right
    ~font
    ~title
    ~f:(fun context ~fill:_ ->
      draw_directions
        context
        ~bullet_center_x
        ~bullet_radius
        ~padding
        ~center_y:arrow_center_y
        ~half_height:arrow_half_height;
      List.iteri rows ~f:(fun row_index row ->
        draw_row
          context
          ~bullet_center_x
          ~bullet_radius
          ~padding
          ~font
          ~bullet_font_size
          ~departure_font_size
          ~now
          ~stop_status
          ~center_y:(first_row_center_y + (row_index * row_height))
          row));
  upper_left, lower_right
;;
