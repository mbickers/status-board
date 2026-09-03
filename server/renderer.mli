open! Core
open! Async

module Device_status : sig
  type t = { battery_voltage : float option } [@@deriving sexp]
end

module Input : sig
  type 'debug_preset t =
    | Device of Device_status.t
    | Preview of 'debug_preset option
end

module Render : sig
  type t =
    { buffer : Image.image
    ; time_until_refresh : Time_ns.Span.t
    }
end

type 'debug_preset t =
  { debug_presets : 'debug_preset list
  ; debug_preset_name : 'debug_preset -> string
  ; render : 'debug_preset Input.t -> Cache.t -> Render.t Deferred.Or_error.t
  }

type packed = Pack : 'debug_preset t -> packed

val debug_preset_names : packed -> string list
val render_device : packed -> Device_status.t -> Cache.t -> Render.t Deferred.Or_error.t

val render_preview
  :  packed
  -> debug_preset:string option
  -> Cache.t
  -> Render.t Deferred.Or_error.t
