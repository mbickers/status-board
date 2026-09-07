open! Core
open! Async

type t

val create : filename:string -> t
val latest : t -> string option Deferred.Or_error.t

val respond
  :  t
  -> path:[ `Exact of string ]
  -> validate:(string -> unit Or_error.t)
  -> Http.Handler.t
