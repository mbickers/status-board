open! Core
open! Async

type t

val create : monitor_path:string -> t
val respond : t -> path:[ `Exact of string ] -> Http.Handler.t
val script : t -> string
