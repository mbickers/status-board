open! Core

module Rendered_text : sig
  type t =
    { buffer : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    ; width : int
    ; height : int
    ; origin_x : int
    ; baseline_y : int
    }
end

type t

val create : ttf_file:string -> t Or_error.t
val render_text : t -> string -> size:float -> Rendered_text.t

val max_width
  :  t
  -> [ `Number of int * int | `String of string ] list
  -> size:float
  -> float * string
