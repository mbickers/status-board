open! Core

module Cloudy_conditions : sig
  type t =
    { rain : bool
    ; snow : bool
    ; thunderstorm : bool
    }
end

module Conditions : sig
  type t =
    | Not_cloudy
    | Cloudy of Cloudy_conditions.t
end

type t =
  { current_temperature_celsius : float option
  ; low_temperature_celsius : float option
  ; high_temperature_celsius : float option
  ; maximum_uv_index : float option
  ; conditions : Conditions.t
  ; moon_phase : float option
  ; sunrise : Time_ns.t
  ; sunset : Time_ns.t
  }

val create
  :  look_forward_hours:int
  -> now:Time_ns.t
  -> forecast:Feeds.Weather.Forecast.t
  -> t Or_error.t
