open! Core

module Horizontal_alignment : sig
  type t =
    | Left
    | Center
    | Right
end

module Vertical_alignment : sig
  type t =
    | Top
    | Center
    | Bottom
    | Baseline
end

module Anchor : sig
  type t = (Horizontal_alignment.t * int) * (Vertical_alignment.t * int)
end

type t

val create
  :  ?baseline:int
  -> size:int * int
  -> draw:(Context.t -> upper_left:int * int -> unit)
  -> unit
  -> t

(* Elements may draw beyond their reported size (e.g. text halos). Size represents visual bounds for layout, not strict bounds on where we draw. *)
val size : t -> int * int
val draw : t -> Context.t -> Anchor.t -> unit
val column : gap:int -> align:Horizontal_alignment.t -> t list -> t
