open! Core
open! Async

type station_information =
  { station_id : string
  ; name : string
  ; lon : float
  ; lat : float
  ; capacity : int
  }

type station_information_data = { stations : station_information list }

type station_information_feed =
  { data : station_information_data
  ; last_updated : int
  ; ttl : int
  ; version : string
  }

type station_status =
  { station_id : string
  ; num_bikes_available : int
  ; num_ebikes_available : int
  ; num_bikes_disabled : int
  ; num_docks_available : int
  ; num_docks_disabled : int
  ; is_installed : int
  ; is_renting : int
  ; is_returning : int
  ; last_reported : int
  }

type station_status_data = { stations : station_status list }

type station_status_feed =
  { data : station_status_data
  ; last_updated : int
  ; ttl : int
  ; version : string
  }

module Feed : sig
  type 'a t

  val station_information : station_information_feed t
  val station_status : station_status_feed t
end

type t

val discover : string -> t Deferred.Or_error.t
val fetch : t -> 'a Feed.t -> 'a Deferred.Or_error.t
