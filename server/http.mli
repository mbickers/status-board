open! Core
open! Async

val request_origin : Cohttp.Request.t -> string Or_error.t
val get_body : string -> string Deferred.Or_error.t

val respond_string
  :  ?headers:Cohttp.Header.t
  -> ?status:Cohttp.Code.status_code
  -> string
  -> Cohttp_async.Server.response_action Deferred.t
