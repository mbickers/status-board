open! Core

module Step : sig
  type t =
    | Point of int * int
    | Offset of int * int
end

val resolve : Step.t list -> (int * int) list
