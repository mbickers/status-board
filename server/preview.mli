open! Core
open! Async

val respond
  :  autoreload_script:string
  -> image_path:(string option -> string)
  -> renderer:Renderer.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t
