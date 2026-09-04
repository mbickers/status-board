open! Core

module Row = struct
  type t =
    { bullet : string * Drawing.Fill.t
    ; route_ids : string list
    ; minimum_minutes : int
    ; westbound_mta_direction : string
    }
end

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
      match
        ((x - center_x) * (x - center_x)) + ((y - center_y) * (y - center_y))
        <= radius * radius
      with
      | true -> Drawing.Context.write context (x, y) (fill (x, y))
      | false -> ()
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

let columns
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~horizontal_padding_between_text
  =
  let width, _ = Drawing.Context.size context in
  let left = bullet_center_x + bullet_radius + padding
  and right = width - padding in
  let westbound_right = ((left + right) / 2) - (horizontal_padding_between_text / 2) in
  left, westbound_right, westbound_right + horizontal_padding_between_text, right
;;

let draw_directions
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~horizontal_padding_between_text
      ~center_y
      ~half_height
  =
  let left, westbound_right, eastbound_left, right =
    columns
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~horizontal_padding_between_text
  in
  let stroke = Drawing.Stroke.solid `b 2 in
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
  draw_arrow `Left ~center_x:((left + westbound_right) / 2);
  draw_arrow `Right ~center_x:((eastbound_left + right) / 2)
;;

let draw_row
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~horizontal_padding_between_text
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
        match
          List.mem row.route_ids arrival.route_id ~equal:String.equal
          && String.is_suffix arrival.stop_id ~suffix:direction
          && Time_ns.Span.compare time_until_arrival minimum_time_until_arrival > 0
        with
        | true -> Some (Time_ns.Span.to_min time_until_arrival |> Float.iround_up_exn)
        | false -> None)
    in
    List.take minutes 3
    |> List.map ~f:Int.to_string
    |> function
    | [] -> "-"
    | minutes -> String.concat minutes ~sep:","
  in
  let label, fill = row.bullet in
  let eastbound_direction =
    match String.equal row.westbound_mta_direction "N" with
    | true -> "S"
    | false -> "N"
  and bullet_text_fill = Drawing.Fill.solid `w
  and text_left, westbound_right, eastbound_left, text_right =
    columns
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~horizontal_padding_between_text
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
    ~right:westbound_right;
  draw_centered_text
    (departure_text eastbound_direction)
    ~left:eastbound_left
    ~right:text_right
;;

module Layout = struct
  type t =
    { width : int
    ; height : int
    ; padding : int
    ; horizontal_padding_between_text : int
    ; bullet_radius : int
    ; arrow_half_height : float
    ; bullet_font_size : float
    ; departure_font_size : float
    ; arrow_center_y : float
    ; first_row_center_y : int
    ; row_height : int
    }

  let bullet_radius = 20

  let width style =
    let font = Status_box.Style.font style
    and padding = Status_box.Style.base_padding style
    and horizontal_padding_between_text =
      Status_box.Style.horizontal_padding_between_text style
    and departure_font_size = Status_box.Style.primary_font_size style in
    let maximum_direction_text_width, _ =
      Font.max_width
        font
        [ `Number (0, 99); `String ","; `Number (0, 99); `String ","; `Number (0, 99) ]
        ~size:departure_font_size
    in
    Float.of_int ((3 * padding) + horizontal_padding_between_text + (2 * bullet_radius))
    +. (2. *. maximum_direction_text_width)
    |> Float.iround_up_exn
  ;;

  let create style ~row_count =
    let font = Status_box.Style.font style in
    let padding = Status_box.Style.base_padding style
    and horizontal_padding_between_text =
      Status_box.Style.horizontal_padding_between_text style
    and arrow_half_height = 6.
    and bullet_font_size = 35.
    and departure_font_size = Status_box.Style.primary_font_size style in
    let arrow_center_y = Float.of_int padding +. arrow_half_height
    and departure_line_height =
      (Font.render_text font "0" ~size:departure_font_size).height
    in
    let first_row_center_y =
      Int.of_float (arrow_center_y +. arrow_half_height)
      + padding
      + (departure_line_height / 2)
      + 3
    and row_height = Int.max (2 * bullet_radius) departure_line_height + padding in
    let height =
      first_row_center_y + ((row_count - 1) * row_height) + bullet_radius + padding - 2
    in
    { width = width style
    ; height
    ; padding
    ; horizontal_padding_between_text
    ; bullet_radius
    ; arrow_half_height
    ; bullet_font_size
    ; departure_font_size
    ; arrow_center_y
    ; first_row_center_y
    ; row_height
    }
  ;;
end

let width = Layout.width
let height style ~row_count = (Layout.create style ~row_count).height

let draw context ~anchor ~style ~title ~now ~stop_status ~rows =
  let font = Status_box.Style.font style in
  let { Layout.width
      ; height
      ; padding
      ; horizontal_padding_between_text
      ; bullet_radius
      ; arrow_half_height
      ; bullet_font_size
      ; departure_font_size
      ; arrow_center_y
      ; first_row_center_y
      ; row_height
      }
    =
    Layout.create style ~row_count:(List.length rows)
  in
  let bullet_center_x = padding + bullet_radius in
  let upper_left, lower_right = Drawing.Anchor.resolve anchor ~size:(width, height) in
  Status_box.draw context upper_left lower_right ~style ~title ~f:(fun context ~fill:_ ->
    draw_directions
      context
      ~bullet_center_x
      ~bullet_radius
      ~padding
      ~horizontal_padding_between_text
      ~center_y:arrow_center_y
      ~half_height:arrow_half_height;
    List.iteri rows ~f:(fun row_index row ->
      draw_row
        context
        ~bullet_center_x
        ~bullet_radius
        ~padding
        ~horizontal_padding_between_text
        ~font
        ~bullet_font_size
        ~departure_font_size
        ~now
        ~stop_status
        ~center_y:(first_row_center_y + (row_index * row_height))
        row))
;;
