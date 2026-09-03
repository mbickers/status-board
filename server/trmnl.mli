open! Core
open! Async

type t

val create
  :  cache:Cache.t
  -> image_publisher:Image_publisher.t
  -> name:string
  -> renderer:Renderer.packed
  -> t

val respond_setup
  :  t
  -> request:Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t

val respond_display
  :  t
  -> request:Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t
