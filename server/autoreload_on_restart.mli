open! Core
open! Async

type t

val create : monitor_path_segment:string -> t
val monitor_path_segment : t -> string
val respond : t -> Cohttp.Request.t -> Cohttp_async.Server.response_action Deferred.t
val script : t -> string
