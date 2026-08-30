open! Core
open! Async

type renderer = Cache.t -> Screen_render.t Deferred.t
type t

val create
  :  autoreload_script:string
  -> cache:Cache.t
  -> renderers:renderer String.Map.t
  -> t Or_error.t

val respond : t -> name:string -> Cohttp_async.Server.response_action Deferred.t
val renderer_names : t -> string list
