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
    { max_uv : float option (** UV Index starts at 0. Values of 11 or more are extreme. *)
    ; sunset : Time_ns.Alternate_sexp.t option
    ; precipitation_probabilities : (Time_ns.Alternate_sexp.t * int) list
    ; thunderstorm : bool
    ; cloudy : bool
    ; snow : bool
    ; us_aqi : float option
    }
  [@@deriving sexp]
end

val query : Cache.t -> coordinates:Coordinates.t -> Summary.t Latest_result.t Deferred.t
