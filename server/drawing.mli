open! Core

module Context : sig
  type t =
    | Image of Image.image
    | Clipped of
        { width : int
        ; height : int
        ; image : Image.image
        }

  val write : t -> int * int -> [ `b | `w ] -> unit
end

module Fill : sig
  type t = int * int -> [ `b | `w ]

  val solid : [ `b | `w ] -> t
  val bayer : ?offset:int * int -> int -> t
  val fade_to_white : t -> level:(int * int -> int) -> t
end

module Stroke : sig
  type t =
    { fill : Fill.t
    ; width : int
    }

  val solid : [ `b | `w ] -> int -> t
end

module O : sig
  val rect : Context.t -> fill:Fill.t -> int * int -> int * int -> unit
  val polygon : Context.t -> fill:Fill.t -> (int * int) list -> unit
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

  val text
    :  Context.t
    -> font:Font.t
    -> fill:Fill.t
    -> origin_x:int
    -> baseline_y:int
    -> size:float
    -> string
    -> unit
end
