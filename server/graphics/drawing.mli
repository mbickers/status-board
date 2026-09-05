open! Core

module Context : sig
  type t

  val create : Image.image -> t
  val crop : t -> size:int * int -> offset:int * int -> t
  val size : t -> int * int
  val write : t -> int * int -> [ `b | `w ] -> unit
end

module Anchor : sig
  type t =
    | Ul of int * int
    | Ur of int * int
    | Ll of int * int
    | Lr of int * int

  val resolve : t -> size:int * int -> (int * int) * (int * int)
end

module Fill : sig
  type t = int * int -> [ `b | `w ]

  val solid : [ `b | `w ] -> t
  val invert : t -> t
  val bayer_exn : ?size:int -> ?offset:int * int -> white_frac:float -> t
  val fade_to : t -> color:[ `b | `w ] -> color_frac:(int * int -> float) -> t
  val fractional : frac:float -> frontier_angle_degrees:float -> Context.t -> t
end

module Stroke : sig
  type t

  val create : ?casing:t -> Fill.t -> int -> t
  val solid : ?casing:t -> [ `b | `w ] -> int -> t
  val safe_padding : t -> int
end

val rect : Context.t -> fill:Fill.t -> int * int -> int * int -> unit

module Path_resolver_step : sig
  type t =
    | Point of int * int
    | Offset of int * int

  val resolve : t list -> (int * int) list
end

val polygon : Context.t -> fill:Fill.t -> (int * int) list -> unit
val circle : Context.t -> fill:Fill.t -> center:int * int -> radius:int -> unit
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
  -> (int * int) list
  -> unit

val text
  :  ?halo:int * Fill.t
  -> Context.t
  -> font:Font.t
  -> fill:Fill.t
  -> origin_x:int
  -> baseline_y:int
  -> size:float
  -> string
  -> unit

module O : sig
  module Context = Context
  module Anchor = Anchor
  module Fill = Fill
  module Path_resolver_step = Path_resolver_step
  module Stroke = Stroke

  val solid : [ `b | `w ] -> Fill.t
  val invert : Fill.t -> Fill.t
  val bayer_exn : ?size:int -> ?offset:int * int -> white_frac:float -> Fill.t
  val fade_to : Fill.t -> color:[ `b | `w ] -> color_frac:(int * int -> float) -> Fill.t
  val rect : Context.t -> fill:Fill.t -> int * int -> int * int -> unit
  val polygon : Context.t -> fill:Fill.t -> (int * int) list -> unit
  val circle : Context.t -> fill:Fill.t -> center:int * int -> radius:int -> unit
  val draw_line : Context.t -> stroke:Stroke.t -> float * float -> float * float -> unit

  val draw_quadratic_curve
    :  Context.t
    -> stroke:Stroke.t
    -> (float * float) * (float * float) * (float * float)
    -> unit

  val rounded_path
    :  Context.t
    -> radius:int
    -> stroke:Stroke.t
    -> (int * int) list
    -> unit

  val rounded_polygon
    :  Context.t
    -> radius:int
    -> fill:Fill.t
    -> ?stroke:Stroke.t
    -> (int * int) list
    -> unit

  val text
    :  ?halo:int * Fill.t
    -> Context.t
    -> font:Font.t
    -> fill:Fill.t
    -> origin_x:int
    -> baseline_y:int
    -> size:float
    -> string
    -> unit
end
