open! Core

module Step = struct
  type t =
    | Point of int * int
    | Offset of int * int
end

let resolve steps =
  List.folding_map steps ~init:(0, 0) ~f:(fun (x, y) step ->
    let point =
      match step with
      | Step.Point (point_x, point_y) -> point_x, point_y
      | Offset (x_offset, y_offset) -> x + x_offset, y + y_offset
    in
    point, point)
;;
