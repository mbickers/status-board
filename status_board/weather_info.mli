open! Core

module Precipitation : sig
  module Sample : sig
    type t =
      { time : Time_ns.t
      ; probability_frac : float option
      }
  end

  type t =
    { precipitation : Feeds.Weather.Precipitation.t
    ; samples : Sample.t list
    }
end

module Conditions : sig
  type t =
    | Not_cloudy
    | Cloudy of Precipitation.t option
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
