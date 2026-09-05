open! Core

type t =
  { current_temperature_celsius : float option
  ; low_temperature_celsius : float option
  ; high_temperature_celsius : float option
  ; maximum_uv_index : float option
  ; sunrise : Time_ns.t
  ; sunset : Time_ns.t
  }

val create
  :  look_forward_hours:int
  -> now:Time_ns.t
  -> forecast:Feeds.Weather.Forecast.t
  -> t Or_error.t
