open! Core
open! Async

val latest : filename:string -> string option Deferred.Or_error.t

val respond
  :  filename:string
  -> validate:(string -> unit Or_error.t)
  -> body:Cohttp_async.Body.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t
