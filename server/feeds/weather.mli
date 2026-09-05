open! Core
open! Async

module Coordinates : sig
  type t =
    { latitude : float
    ; longitude : float
    }
end

module Summary : sig
  type t =
    { current_temperature_celsius : float option
    ; low_temperature_celsius : float option
    ; high_temperature_celsius : float option
    ; current_uv_index : float option
    ; uv_indices : (Time_ns.Alternate_sexp.t * float) list
    ; sunrise : Time_ns.Alternate_sexp.t
    ; sunset : Time_ns.Alternate_sexp.t
    ; precipitation_probabilities : (Time_ns.Alternate_sexp.t * int) list
    ; thunderstorm : bool
    ; cloudy : bool
    ; snow : bool
    ; us_aqi : float option
    }
  [@@deriving sexp]
end

val query : Cache.t -> coordinates:Coordinates.t -> Summary.t Latest_result.t Deferred.t
