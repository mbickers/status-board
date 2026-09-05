open! Core
open! Async

module Device_status = struct
  type t = { battery_voltage : float option } [@@deriving sexp]
end

module Input = struct
  type t =
    | Device of Device_status.t
    | Preview of string option
end

type t =
  { refresh_interval : Time_ns.Span.t
  ; debug_presets : string list
  ; render : Input.t -> Feeds.Cache.t -> Image.image Deferred.Or_error.t
  }
