open! Core

type t

val create : width:int -> height:int -> t
val size : t -> int * int
val write_exn : t -> x:int -> y:int -> [ `b | `w ] -> unit
val encode_bmp : t -> string
