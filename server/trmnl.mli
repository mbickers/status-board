open! Core
open! Async

val respond
  :  base_url:string
  -> image_path:(Status_board.Device_status.t -> string)
  -> refresh_interval:Time_ns.Span.t
  -> body:Cohttp_async.Body.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t
