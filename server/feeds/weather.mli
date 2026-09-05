open! Core
open! Async

module Coordinates : sig
  type t =
    { latitude : float
    ; longitude : float
    }
end

module Conditions : sig
  type t =
    { thunderstorm : bool
    ; cloudy : bool
    ; snow : bool
    }
  [@@deriving sexp]
end

module Forecast_current : sig
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; interval_seconds : int
    ; temperature_2m : float option
    ; uv_index : float option
    }
  [@@deriving sexp]
end

module Hourly : sig
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; temperature_2m : float option
    ; precipitation_probability : int option
    ; conditions : Conditions.t option
    ; uv_index : float option
    }
  [@@deriving sexp]
end

module Daily : sig
  type t =
    { date : Date.t
    ; sunrise : Time_ns.Alternate_sexp.t option
    ; sunset : Time_ns.Alternate_sexp.t option
    }
  [@@deriving sexp]
end

module Forecast : sig
  type t =
    { timezone : string
    ; current : Forecast_current.t
    ; hourly : Hourly.t list
    ; daily : Daily.t list
    }
  [@@deriving sexp]
end

module Air_quality_current : sig
  type t =
    { time : Time_ns.Alternate_sexp.t
    ; interval_seconds : int
    ; us_aqi : float option
    }
  [@@deriving sexp]
end

module Air_quality : sig
  type t = { current : Air_quality_current.t } [@@deriving sexp]
end

val query
  :  Cache.t
  -> coordinates:Coordinates.t
  -> (Forecast.t * Air_quality.t) Latest_result.t Deferred.t
