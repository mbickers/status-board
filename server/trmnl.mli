open! Core
open! Async

val respond
  :  path:[ `Prefix of string ]
  -> image_path:(Status_board.Device_status.t -> string)
  -> refresh_interval:Time_ns.Span.t
  -> Http.Handler.t
