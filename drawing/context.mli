open! Core

type t

val create : Bitmap.t -> t
val crop : t -> size:int * int -> offset:int * int -> t
val size : t -> int * int
val write : t -> int * int -> [ `b | `w ] -> unit
