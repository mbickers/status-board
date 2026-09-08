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
  ; render :
      Input.t
      -> Feeds.Cache.t
      -> message:string option
      -> Drawing.Bitmap.t Deferred.Or_error.t
  }
