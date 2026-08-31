open! Core
open! Async

type t

val create : monitor_path:string list -> t
val monitor_path : t -> string list
val respond : t -> Cohttp.Request.t -> Cohttp_async.Server.response_action Deferred.t
val script : t -> string
