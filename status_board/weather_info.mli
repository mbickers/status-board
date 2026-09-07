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
  ; us_aqi : float option
  ; conditions : Conditions.t
  ; moon_phase : float option
  ; sunrise : Time_ns.t
  ; sunset : Time_ns.t
  }

val create
  :  look_forward_hours:int
  -> now:Time_ns.t
  -> forecast:Feeds.Weather.Forecast.t
  -> us_aqi:float option
  -> t Or_error.t

module Testing_data : sig
  val dense_text : now:Time_ns.t -> zone:Time_ns_unix.Zone.t -> t
  val stormy : hour_start:Time_ns.t -> zone:Time_ns_unix.Zone.t -> t Or_error.t
  val errors : now:Time_ns.t -> zone:Time_ns_unix.Zone.t -> t
end
