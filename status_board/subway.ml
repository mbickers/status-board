open! Core

module Line = struct
  type t =
    | L
    | M
    | J_and_Z_but_call_it_J

  let to_string = function
    | L -> "L"
    | M -> "M"
    | J_and_Z_but_call_it_J -> "J"
  ;;

  let mta_route_ids = function
    | L -> [ "L" ]
    | M -> [ "M" ]
    | J_and_Z_but_call_it_J -> [ "J"; "Z" ]
  ;;

  let fill = function
    | L -> Drawing.Fill.bayer_exn ?size:None ?offset:None ~white_frac:(9. /. 16.)
    | M -> Drawing.Fill.bayer_exn ?size:None ~offset:(1, 1) ~white_frac:(10. /. 16.)
    | J_and_Z_but_call_it_J ->
      Drawing.Fill.bayer_exn ?size:None ?offset:None ~white_frac:(1. /. 16.)
  ;;
end

let stroke ~casing_fill fill =
  let casing = Drawing.Stroke.create casing_fill 12 in
  Drawing.Stroke.create ~casing fill 8
;;

let stroke_safe_padding =
  Drawing.Stroke.safe_padding
    (stroke ~casing_fill:(Drawing.Fill.solid `b) (Drawing.Fill.solid `b))
;;

module Status = struct
  module Row = struct
    type t =
      { line : Line.t
      ; westbound_minutes : int list
      ; eastbound_minutes : int list
      }
  end

  type t =
    { rows : Row.t list
    ; has_alert : bool
    }

  module Testing_data = struct
    let dense_text ~widest_two_digit_number ~lines =
      let minutes = List.init 3 ~f:(fun _ -> widest_two_digit_number) in
      { rows =
          List.map lines ~f:(fun line ->
            { Row.line; westbound_minutes = minutes; eastbound_minutes = minutes })
      ; has_alert = false
      }
    ;;

    let errors ~lines =
      { rows =
          List.map lines ~f:(fun line ->
            { Row.line; westbound_minutes = []; eastbound_minutes = [] })
      ; has_alert = true
      }
    ;;
  end

  module Selection = struct
    type t =
      { line : Line.t
      ; minimum_minutes : int
      ; westbound_mta_direction : Feeds.Mta_subway.Direction.t
      }
  end

  let create (status : Feeds.Mta_subway.Status.t) ~now ~station_id ~rows =
    let%map.Or_error stop_status =
      Map.find_or_error status.stop_status_by_station_id station_id
    in
    let rows =
      List.map rows ~f:(fun (selection : Selection.t) ->
        let minimum_time_until_arrival =
          Time_ns.Span.of_int_min selection.minimum_minutes
        in
        let minutes direction =
          List.filter_map stop_status.upcoming_arrivals ~f:(fun arrival ->
            let time_until_arrival = Time_ns.diff arrival.arrives_at now in
            match
              List.mem
                (Line.mta_route_ids selection.line)
                arrival.route_id
                ~equal:String.equal
              && Feeds.Mta_subway.Direction.equal arrival.stop_id.direction direction
              && Time_ns.Span.compare time_until_arrival minimum_time_until_arrival > 0
            with
            | true -> Some (Time_ns.Span.to_min time_until_arrival |> Float.iround_up_exn)
            | false -> None)
          |> fun minutes -> List.take minutes 3
        in
        let eastbound_direction =
          match selection.westbound_mta_direction with
          | North -> Feeds.Mta_subway.Direction.South
          | South -> North
        in
        { Row.line = selection.line
        ; westbound_minutes = minutes selection.westbound_mta_direction
        ; eastbound_minutes = minutes eastbound_direction
        })
    in
    { rows; has_alert = not (List.is_empty stop_status.alerts) }
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
    let stroke = Drawing.Stroke.solid `b 2 in
    let draw_arrow direction ~center_x =
      let tip_x, tail_x, arrowhead_x =
        match direction with
        | `Left -> center_x - 9, center_x + 9, center_x - 3
        | `Right -> center_x + 9, center_x - 9, center_x + 3
      in
      let tip = Float.of_int tip_x, layout.arrow_center_y in
      Drawing.Shapes.draw_line
        context
        ~stroke
        (Float.of_int tail_x, layout.arrow_center_y)
        tip;
      Drawing.Shapes.draw_line
        context
        ~stroke
        tip
        (Float.of_int arrowhead_x, layout.arrow_center_y -. layout.arrow_half_height);
      Drawing.Shapes.draw_line
        context
        ~stroke
        tip
        (Float.of_int arrowhead_x, layout.arrow_center_y +. layout.arrow_half_height)
    in
    draw_arrow `Left ~center_x:((left + westbound_right) / 2);
    draw_arrow `Right ~center_x:((eastbound_left + right) / 2)
  ;;

  let element ~style ~label { rows; has_alert } =
    let font = Status_box.Style.font style in
    let layout = Layout.create style ~row_count:(List.length rows) in
    let rows =
      List.map rows ~f:(fun (row : Row.t) ->
        let departure_text minutes =
          match List.map minutes ~f:Int.to_string with
          | [] -> "-"
          | minutes -> String.concat minutes ~sep:","
        in
        let bullet =
          Drawing.Text.create
            ~font
            ~size:layout.bullet_font_size
            ~fill:(Drawing.Fill.solid `w)
            (Line.to_string row.line)
        and west =
          Drawing.Text.create
            ~font
            ~size:layout.departure_font_size
            ~fill:(Drawing.Fill.solid `b)
            (departure_text row.westbound_minutes)
        and east =
          Drawing.Text.create
            ~font
            ~size:layout.departure_font_size
            ~fill:(Drawing.Fill.solid `b)
            (departure_text row.eastbound_minutes)
        in
        fun context ~center_y ->
          let text_left, westbound_right, eastbound_left, text_right = columns layout in
          let center_x = layout.padding + layout.bullet_radius in
          Drawing.Shapes.circle
            context
            ~fill:(Line.fill row.line)
            ~center:(center_x, center_y - 6)
            ~radius:layout.bullet_radius;
          Drawing.Element.draw bullet context ((Center, center_x), (Center, center_y - 6));
          Drawing.Element.draw
            west
            context
            ((Center, (text_left + westbound_right) / 2), (Center, center_y));
          Drawing.Element.draw
            east
            context
            ((Center, (eastbound_left + text_right) / 2), (Center, center_y)))
    in
    let size = layout.width, layout.height in
    let content =
      Drawing.Element.create
        ~size
        ~draw:(fun context ~upper_left:(x, y) ->
          let context = Drawing.Context.crop context ~offset:(x, y) ~size in
          draw_directions context ~layout;
          List.iteri rows ~f:(fun index draw ->
            draw
              context
              ~center_y:(layout.first_row_center_y + (index * layout.row_height))))
        ()
    in
    let box = Status_box.create ~style ~label ~content () in
    let alert =
      match has_alert with
      | false -> None
      | true ->
        Some
          (Drawing.Text.create
             ~halo:(3, Drawing.Fill.solid `w)
             ~font
             ~size:30.
             ~fill:(Drawing.Fill.solid `b)
             "!!")
    in
    Drawing.Element.create
      ~size
      ~draw:(fun context ~upper_left:(left, top) ->
        Drawing.Element.draw box context ((Left, left), (Top, top));
        Option.iter alert ~f:(fun alert ->
          Drawing.Element.draw
            alert
            context
            ((Right, left + layout.width - 8), (Center, top + 3))))
      ()
  ;;
end
