open! Core

module Availability = struct
  type t =
    | Renting of
        { classic_bikes_available : int
        ; electric_bikes_available : int
        }
    | Not_renting
end

module Parking = struct
  type t =
    | Accepting_returns of { docks_available : int }
    | Not_accepting_returns
end

type t =
  { availability : Availability.t
  ; parking : Parking.t
  ; bikes_available_frac : float
  }

let create (station : Feeds.Citibike.Station.t) =
  let usable_capacity =
    station.capacity - station.bikes_disabled - station.docks_disabled
  in
  let bikes_available_frac =
    match usable_capacity <= 0 with
    | true -> 0.
    | false -> Float.of_int station.bikes_available /. Float.of_int usable_capacity
  in
  { availability =
      (match station.is_renting with
       | false -> Availability.Not_renting
       | true ->
         Renting
           { classic_bikes_available = station.bikes_available - station.ebikes_available
           ; electric_bikes_available = station.ebikes_available
           })
  ; parking =
      (match station.is_returning with
       | false -> Parking.Not_accepting_returns
       | true -> Accepting_returns { docks_available = station.docks_available })
  ; bikes_available_frac
  }
;;

let draw_centered_text context ~font ~fill ~size ~baseline_y ~left ~right text =
  let rendered_text = Graphics.Font.render_text font text ~size in
  Graphics.Drawing.text
    context
    ~font
    ~fill
    ~origin_x:(left + ((right - left - rendered_text.width) / 2) + rendered_text.origin_x)
    ~baseline_y
    ~size
    text
;;

let draw_box context ~anchor ~style ~title ~box_size t ~is_enabled ~f =
  let upper_left, lower_right = Graphics.Drawing.Anchor.resolve anchor ~size:box_size in
  Status_box.draw
    context
    upper_left
    lower_right
    ~style
    ~title
    ~fill:
      (match is_enabled with
       | true ->
         Graphics.Drawing.Fill.fractional
           ~frac:t.bikes_available_frac
           ~frontier_angle_degrees:15.
       | false -> fun _ -> Status_box.Style.error_fill style)
    ~f
;;

let draw_availability context ~anchor ~style ~title ~box_size t =
  let font = Status_box.Style.font style in
  draw_box
    context
    ~anchor
    ~style
    ~title
    ~box_size
    t
    ~is_enabled:
      (match t.availability with
       | Availability.Renting _ -> true
       | Not_renting -> false)
    ~f:(fun context ~fill ->
      match t.availability with
      | Availability.Not_renting -> ()
      | Renting { classic_bikes_available; electric_bikes_available } ->
        let width, height = Graphics.Drawing.Context.size context in
        let count_size = Status_box.Style.primary_font_size style
        and base_padding = Status_box.Style.base_padding style
        and horizontal_padding_between_text =
          Status_box.Style.horizontal_padding_between_text style
        in
        let baseline_y = height - Status_box.Style.baseline_padding style
        and left = base_padding
        and right = width - base_padding in
        let fill = Graphics.Drawing.Fill.invert fill in
        let middle = (left + right) / 2 in
        let bikes_right = middle - (horizontal_padding_between_text / 2)
        and ebikes_left = middle + (horizontal_padding_between_text / 2) in
        let ebikes_available = Int.to_string electric_bikes_available in
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left
          ~right:bikes_right
          (Int.to_string classic_bikes_available);
        draw_centered_text
          context
          ~font
          ~fill
          ~size:count_size
          ~baseline_y
          ~left:ebikes_left
          ~right
          ebikes_available;
        let ebike_label_size = 22. in
        let rendered_ebikes =
          Graphics.Font.render_text font ebikes_available ~size:count_size
        and rendered_ebike_label =
          Graphics.Font.render_text font "e" ~size:ebike_label_size
        in
        draw_centered_text
          context
          ~font
          ~fill
          ~size:ebike_label_size
          ~baseline_y:
            (baseline_y
             - rendered_ebikes.baseline_y
             - 5
             - rendered_ebike_label.height
             + rendered_ebike_label.baseline_y)
          ~left:ebikes_left
          ~right
          "e")
;;

let draw_parking context ~anchor ~style ~title ~box_size t =
  let font = Status_box.Style.font style in
  draw_box
    context
    ~anchor
    ~style
    ~title
    ~box_size
    t
    ~is_enabled:
      (match t.parking with
       | Parking.Accepting_returns _ -> true
       | Not_accepting_returns -> false)
    ~f:(fun context ~fill ->
      match t.parking with
      | Parking.Not_accepting_returns -> ()
      | Accepting_returns { docks_available } ->
        let width, height = Graphics.Drawing.Context.size context in
        let base_padding = Status_box.Style.base_padding style in
        draw_centered_text
          context
          ~font
          ~fill:(Graphics.Drawing.Fill.invert fill)
          ~size:(Status_box.Style.primary_font_size style)
          ~baseline_y:(height - Status_box.Style.baseline_padding style)
          ~left:base_padding
          ~right:(width - base_padding)
          (Int.to_string docks_available))
;;
