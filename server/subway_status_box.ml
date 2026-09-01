open! Core

module Row = struct
  type t =
    { bullet : string * Drawing.Fill.t
    ; route_ids : string list
    ; minimum_minutes : int
    ; westbound_mta_direction : string
    }
end

let row_baseline_y ~first_row_baseline_y ~bullet_radius ~padding ~row_index =
  first_row_baseline_y + (row_index * ((2 * bullet_radius) + padding))
;;

let height
      ~first_row_baseline_y
      ~bullet_radius
      ~bullet_baseline_offset
      ~padding
      ~row_count
  =
  row_baseline_y ~first_row_baseline_y ~bullet_radius ~padding ~row_index:(row_count - 1)
  + bullet_radius
  - bullet_baseline_offset
  + padding
;;

let draw_bullet context ~font ~fill ~text_fill ~label ~radius ~center:(center_x, center_y)
  =
  for y = center_y - radius to center_y + radius do
    for x = center_x - radius to center_x + radius do
      if
        ((x - center_x) * (x - center_x)) + ((y - center_y) * (y - center_y))
        <= radius * radius
      then Drawing.Context.write context (x, y) (fill (x, y))
    done
  done;
  let font_size = 30. in
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

let draw_directions context ~bullet_center_x ~bullet_radius ~padding =
  let left, middle, right = columns context ~bullet_center_x ~bullet_radius ~padding
  and stroke = Drawing.Stroke.solid `b 2 in
  let draw_arrow direction ~center_x =
    let arrow_y = 14. in
    let tip_x, tail_x, arrowhead_x =
      match direction with
      | `Left -> center_x - 9, center_x + 9, center_x - 3
      | `Right -> center_x + 9, center_x - 9, center_x + 3
    in
    let tip = Float.of_int tip_x, arrow_y in
    Drawing.draw_line context ~stroke (Float.of_int tail_x, arrow_y) tip;
    Drawing.draw_line context ~stroke tip (Float.of_int arrowhead_x, arrow_y -. 6.);
    Drawing.draw_line context ~stroke tip (Float.of_int arrowhead_x, arrow_y +. 6.)
  in
  draw_arrow `Left ~center_x:((left + middle) / 2);
  draw_arrow `Right ~center_x:((middle + right) / 2)
;;

let draw_row
      context
      ~first_row_baseline_y
      ~bullet_center_x
      ~bullet_radius
      ~bullet_baseline_offset
      ~padding
      ~font
      ~now
      ~(stop_status : Mta_subway.Stop_status.t)
      ~row_index
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
  and bullet_text_fill = Drawing.Fill.solid (if String.equal label "J" then `w else `b)
  and baseline_y = row_baseline_y ~first_row_baseline_y ~bullet_radius ~padding ~row_index
  and text_left, text_middle, text_right =
    columns context ~bullet_center_x ~bullet_radius ~padding
  in
  let font_size = 25. in
  let draw_centered_text text ~left ~right =
    let rendered_text = Font.render_text font text ~size:font_size in
    Drawing.text
      context
      ~font
      ~fill:(Drawing.Fill.solid `b)
      ~origin_x:
        (left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
      ~baseline_y
      ~size:font_size
      text
  in
  draw_bullet
    context
    ~font
    ~fill
    ~text_fill:bullet_text_fill
    ~label
    ~radius:bullet_radius
    ~center:(bullet_center_x, baseline_y - bullet_baseline_offset);
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
  let width = 250
  and padding = 8
  and bullet_center_x = 26
  and bullet_radius = 18
  and bullet_baseline_offset = 9
  and first_row_baseline_y = 46 in
  let box_height =
    height
      ~first_row_baseline_y
      ~bullet_radius
      ~bullet_baseline_offset
      ~padding
      ~row_count:(List.length rows)
  in
  let upper_left, lower_right = Drawing.Anchor.resolve anchor ~size:(width, box_height) in
  Drawing.status_box
    context
    upper_left
    lower_right
    ~font
    ~title
    ~f:(fun context ~fill:_ ->
      draw_directions context ~bullet_center_x ~bullet_radius ~padding;
      List.iteri rows ~f:(fun row_index row ->
        draw_row
          context
          ~first_row_baseline_y
          ~bullet_center_x
          ~bullet_radius
          ~bullet_baseline_offset
          ~padding
          ~font
          ~now
          ~stop_status
          ~row_index
          row))
;;
