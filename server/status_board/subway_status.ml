open! Core

module Row = struct
  type 'display_route t =
    { display_route : 'display_route
    ; westbound_minutes : int list
    ; eastbound_minutes : int list
    }
end

type 'display_route t =
  { rows : 'display_route Row.t list
  ; has_alert : bool
  }

module Selection = struct
  type 'display_route t =
    { display_route : 'display_route
    ; route_ids : string list
    ; minimum_minutes : int
    ; westbound_mta_direction : Feeds.Mta_subway.Direction.t
    }
end

let create (status : Feeds.Mta_subway.Status.t) ~now ~station_id ~rows =
  let%map.Or_error stop_status =
    Map.find_or_error status.stop_status_by_station_id station_id
  in
  let rows =
    List.map rows ~f:(fun (selection : _ Selection.t) ->
      let minimum_time_until_arrival =
        Time_ns.Span.of_int_min selection.minimum_minutes
      in
      let minutes direction =
        List.filter_map stop_status.upcoming_arrivals ~f:(fun arrival ->
          let time_until_arrival = Time_ns.diff arrival.arrives_at now in
          match
            List.mem selection.route_ids arrival.route_id ~equal:String.equal
            && Feeds.Mta_subway.Direction.equal arrival.stop_id.direction direction
            && Time_ns.Span.compare time_until_arrival minimum_time_until_arrival > 0
          with
          | true -> Some (Time_ns.Span.to_min time_until_arrival |> Float.iround_up_exn)
          | false -> None)
        |> fun minutes -> List.take minutes 3
      in
      let eastbound_direction =
        match selection.westbound_mta_direction with
        | Feeds.Mta_subway.Direction.North -> Feeds.Mta_subway.Direction.South
        | South -> North
      in
      { Row.display_route = selection.display_route
      ; westbound_minutes = minutes selection.westbound_mta_direction
      ; eastbound_minutes = minutes eastbound_direction
      })
  in
  { rows; has_alert = not (List.is_empty stop_status.alerts) }
;;

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
      | true -> Graphics.Drawing.Context.write context (x, y) (fill (x, y))
      | false -> ()
    done
  done;
  let rendered_text = Graphics.Font.render_text font label ~size:font_size in
  Graphics.Drawing.text
    context
    ~font
    ~fill:text_fill
    ~origin_x:(center_x - (rendered_text.width / 2) + rendered_text.origin_x)
    ~baseline_y:(center_y - (rendered_text.height / 2) + rendered_text.baseline_y)
    ~size:font_size
    label
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
      Graphics.Font.max_width
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
      (Graphics.Font.render_text font "0" ~size:departure_font_size).height
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

let columns (layout : Layout.t) =
  let left = (2 * layout.padding) + (2 * layout.bullet_radius)
  and right = layout.width - layout.padding in
  let westbound_right =
    ((left + right) / 2) - (layout.horizontal_padding_between_text / 2)
  in
  left, westbound_right, westbound_right + layout.horizontal_padding_between_text, right
;;

let draw_directions context ~(layout : Layout.t) =
  let left, westbound_right, eastbound_left, right = columns layout in
  let stroke = Graphics.Drawing.Stroke.solid `b 2 in
  let draw_arrow direction ~center_x =
    let tip_x, tail_x, arrowhead_x =
      match direction with
      | `Left -> center_x - 9, center_x + 9, center_x - 3
      | `Right -> center_x + 9, center_x - 9, center_x + 3
    in
    let tip = Float.of_int tip_x, layout.arrow_center_y in
    Graphics.Drawing.draw_line
      context
      ~stroke
      (Float.of_int tail_x, layout.arrow_center_y)
      tip;
    Graphics.Drawing.draw_line
      context
      ~stroke
      tip
      (Float.of_int arrowhead_x, layout.arrow_center_y -. layout.arrow_half_height);
    Graphics.Drawing.draw_line
      context
      ~stroke
      tip
      (Float.of_int arrowhead_x, layout.arrow_center_y +. layout.arrow_half_height)
  in
  draw_arrow `Left ~center_x:((left + westbound_right) / 2);
  draw_arrow `Right ~center_x:((eastbound_left + right) / 2)
;;

let draw_row
      context
      ~(layout : Layout.t)
      ~font
      ~center_y
      ~display_route_text
      ~route_fill
      (row : _ Row.t)
  =
  let departure_text minutes =
    match List.map minutes ~f:Int.to_string with
    | [] -> "-"
    | minutes -> String.concat minutes ~sep:","
  in
  let fill = route_fill row.display_route in
  let bullet_text_fill = Graphics.Drawing.Fill.solid `w
  and text_left, westbound_right, eastbound_left, text_right = columns layout in
  let draw_centered_text text ~left ~right =
    let rendered_text =
      Graphics.Font.render_text font text ~size:layout.departure_font_size
    in
    Graphics.Drawing.text
      context
      ~font
      ~fill:(Graphics.Drawing.Fill.solid `b)
      ~origin_x:
        (left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
      ~baseline_y:(center_y - (rendered_text.height / 2) + rendered_text.baseline_y)
      ~size:layout.departure_font_size
      text
  in
  draw_bullet
    context
    ~font
    ~font_size:layout.bullet_font_size
    ~fill
    ~text_fill:bullet_text_fill
    ~label:(display_route_text row.display_route)
    ~radius:layout.bullet_radius
    ~center:(layout.padding + layout.bullet_radius, center_y - 6);
  draw_centered_text
    (departure_text row.westbound_minutes)
    ~left:text_left
    ~right:westbound_right;
  draw_centered_text
    (departure_text row.eastbound_minutes)
    ~left:eastbound_left
    ~right:text_right
;;

let width = Layout.width
let height style t = (Layout.create style ~row_count:(List.length t.rows)).height

let draw context ~anchor ~style ~title ~display_route_text ~route_fill { rows; has_alert }
  =
  let font = Status_box.Style.font style in
  let layout = Layout.create style ~row_count:(List.length rows) in
  let upper_left, lower_right =
    Graphics.Drawing.Anchor.resolve anchor ~size:(layout.width, layout.height)
  in
  Status_box.draw context upper_left lower_right ~style ~title ~f:(fun context ~fill:_ ->
    draw_directions context ~layout;
    List.iteri rows ~f:(fun row_index row ->
      draw_row
        context
        ~layout
        ~font
        ~display_route_text
        ~route_fill
        ~center_y:(layout.first_row_center_y + (row_index * layout.row_height))
        row));
  match has_alert with
  | false -> ()
  | true ->
    let _, top = upper_left
    and right, _ = lower_right in
    let size = 30. in
    let alert_text = "!!" in
    let rendered = Graphics.Font.render_text font alert_text ~size in
    Graphics.Drawing.text
      ~halo:(3, Graphics.Drawing.Fill.solid `w)
      context
      ~font
      ~fill:(Graphics.Drawing.Fill.solid `b)
      ~origin_x:(right - rendered.width - 8 + rendered.origin_x)
      ~baseline_y:(top - (rendered.height / 2) + rendered.baseline_y + 3)
      ~size
      alert_text
;;
