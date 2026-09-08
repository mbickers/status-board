open! Core

module Fill : sig
  type t = int * int -> [ `b | `w ]

  val solid : [ `b | `w ] -> t
  val invert : t -> t
  val bayer_exn : ?size:int -> ?offset:int * int -> white_frac:float -> t
  val fade_to : t -> color:[ `b | `w ] -> color_frac:(int * int -> float) -> t
  val fractional : frac:float -> frontier_angle_degrees:float -> size:int * int -> t
end

module Stroke : sig
  type t

  val create : ?casing:t -> Fill.t -> int -> t
  val solid : ?casing:t -> [ `b | `w ] -> int -> t
  val safe_padding : t -> int
end

val rect : Context.t -> fill:Fill.t -> int * int -> int * int -> unit
val polygon : Context.t -> fill:Fill.t -> (int * int) list -> unit
val circle : Context.t -> fill:Fill.t -> center:int * int -> radius:int -> unit
val star : Context.t -> stroke:Stroke.t -> radius:int -> center:int * int -> unit
val draw_line : Context.t -> stroke:Stroke.t -> float * float -> float * float -> unit

val draw_quadratic_curve
  :  Context.t
  -> stroke:Stroke.t
  -> (float * float) * (float * float) * (float * float)
  -> unit

val rounded_path : Context.t -> radius:int -> stroke:Stroke.t -> (int * int) list -> unit

val rounded_polygon
  :  Context.t
  -> radius:int
  -> fill:Fill.t
  -> ?stroke:Stroke.t
  -> ?round_corner:(int -> bool)
  -> (int * int) list
  -> unit

val text
  :  ?halo:int * Fill.t
  -> font:Font.t
  -> fill:Fill.t
  -> size:float
  -> string
  -> Element.t
