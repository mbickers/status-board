open! Core
open! Async

type t

val create
  :  autoreload_script:string
  -> cache:Cache.t
  -> image_publisher:Image_publisher.t
  -> renderers:Renderer.packed String.Map.t
  -> t Or_error.t

val respond
  :  t
  -> request:Cohttp.Request.t
  -> name:string
  -> Cohttp_async.Server.response_action Deferred.t

val renderer_names : t -> string list
