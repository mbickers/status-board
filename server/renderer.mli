open! Core
open! Async

module Device_status : sig
  type t = { battery_voltage : float option } [@@deriving sexp]
end

module Input : sig
  type t =
    | Device of Device_status.t
    | Preview of string option
end

type t =
  { refresh_interval : Time_ns.Span.t
  ; debug_presets : string list
  ; render : Input.t -> Cache.t -> Image.image Deferred.Or_error.t
  }

val url_query_string : Input.t -> string

val respond
  :  cache:Cache.t
  -> renderer:t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t
