open! Core
open! Async

type t

val create : monitor_path:string list -> t
val respond : t -> Cohttp.Request.t -> Cohttp_async.Server.response_action Deferred.t
val script : t -> string
