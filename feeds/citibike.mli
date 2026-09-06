open! Core
open! Async

module Station : sig
  type t =
    { station_id : string
    ; name : string
    ; latitude : float
    ; longitude : float
    ; capacity : int
    ; bikes_available : int
    ; ebikes_available : int
    ; bikes_disabled : int
    ; docks_available : int
    ; docks_disabled : int
    ; is_installed : bool
    ; is_renting : bool
    ; is_returning : bool
    ; last_reported : Time_ns.Alternate_sexp.t
    }
end

val query : Cache.t -> Station.t String.Map.t Latest_result.t Deferred.t
