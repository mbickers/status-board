open! Core
open! Async

val get_body : string -> string Deferred.Or_error.t
