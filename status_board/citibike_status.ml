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
       | false -> Not_renting
       | true ->
         Renting
           { classic_bikes_available = station.bikes_available - station.ebikes_available
           ; electric_bikes_available = station.ebikes_available
           })
  ; parking =
      (match station.is_returning with
       | false -> Not_accepting_returns
       | true -> Accepting_returns { docks_available = station.docks_available })
  ; bikes_available_frac
  }
;;

let parking_size style =
  let count_width, _ =
    Font.max_width
      (Status_box.Style.font style)
      [ `Number (0, 99) ]
      ~size:(Status_box.Style.primary_font_size style)
  in
  ( Float.iround_up_exn count_width + (2 * Status_box.Style.base_padding style)
  , Float.iround_up_exn (Status_box.Style.primary_font_size style)
    + Status_box.Style.baseline_padding style )
;;

let availability_size style =
  let parking_width, parking_height = parking_size style in
  ( (2 * parking_width)
    - (2 * Status_box.Style.base_padding style)
    + Status_box.Style.horizontal_padding_between_text style
  , parking_height + 10 )
;;

let availability ~style ~label t =
  let font = Status_box.Style.font style in
  let width, height = availability_size style in
  let fill, counts =
    match t.availability with
    | Not_renting -> Status_box.Style.error_fill style, None
    | Renting { classic_bikes_available; electric_bikes_available } ->
      let fill =
        Drawing.Fill.fractional
          ~frac:t.bikes_available_frac
          ~frontier_angle_degrees:15.
          ~size:(width, height)
      in
      let size = Status_box.Style.primary_font_size style in
      let text_fill = Drawing.Fill.invert fill in
      let classic =
        Drawing.Text.create
          ~font
          ~size
          ~fill:text_fill
          (Int.to_string classic_bikes_available)
      and electric =
        Drawing.Text.create
          ~font
          ~size
          ~fill:text_fill
          (Int.to_string electric_bikes_available)
      and label = Drawing.Text.create ~font ~size:22. ~fill:text_fill "e" in
      fill, Some (classic, electric, label)
  in
  let content =
    Drawing.Element.create
      ~size:(width, height)
      ~draw:(fun context ~upper_left:(x, y) ->
        Option.iter counts ~f:(fun (classic, electric, label) ->
          let padding = Status_box.Style.base_padding style in
          let gap = Status_box.Style.horizontal_padding_between_text style in
          let middle = width / 2 in
          let classic_x = x + ((padding + middle - (gap / 2)) / 2)
          and electric_x = x + ((middle + (gap / 2) + width - padding) / 2)
          and bottom = y + height - Status_box.Style.baseline_padding style in
          let _, electric_height = Drawing.Element.size electric in
          Drawing.Element.draw classic context ((Center, classic_x), (Baseline, bottom));
          Drawing.Element.draw electric context ((Center, electric_x), (Baseline, bottom));
          Drawing.Element.draw
            label
            context
            ((Center, electric_x), (Bottom, bottom - electric_height - 4))))
      ()
  in
  Status_box.create ~style ~label ~fill ~content ()
;;

let parking ~style ~label t =
  let width, height = parking_size style in
  let fill, count =
    match t.parking with
    | Not_accepting_returns -> Status_box.Style.error_fill style, None
    | Accepting_returns { docks_available } ->
      let fill =
        Drawing.Fill.fractional
          ~frac:t.bikes_available_frac
          ~frontier_angle_degrees:15.
          ~size:(width, height)
      in
      ( fill
      , Some
          (Drawing.Text.create
             ~font:(Status_box.Style.font style)
             ~size:(Status_box.Style.primary_font_size style)
             ~fill:(Drawing.Fill.invert fill)
             (Int.to_string docks_available)) )
  in
  let content =
    Drawing.Element.create
      ~size:(width, height)
      ~draw:(fun context ~upper_left:(x, y) ->
        Option.iter count ~f:(fun count ->
          Drawing.Element.draw
            count
            context
            ( (Center, x + (width / 2))
            , (Baseline, y + height - Status_box.Style.baseline_padding style) )))
      ()
  in
  Status_box.create ~style ~label ~fill ~content ()
;;

module Testing_data = struct
  let dense_text ~widest_two_digit_number =
    { availability =
        Renting
          { classic_bikes_available = widest_two_digit_number
          ; electric_bikes_available = widest_two_digit_number
          }
    ; parking = Accepting_returns { docks_available = widest_two_digit_number }
    ; bikes_available_frac = 2. /. 3.
    }
  ;;

  let errors =
    { availability = Not_renting
    ; parking = Not_accepting_returns
    ; bikes_available_frac = 0.
    }
  ;;
end
