open! Core
open! Async

module Station_information : sig
  type t =
    { station_id : string
    ; name : string
    ; lon : float
    ; lat : float
    ; capacity : int
    }
end

module Station_information_data : sig
  type t = { stations : Station_information.t list }
end

module Station_information_feed : sig
  type t =
    { data : Station_information_data.t
    ; last_updated : int
    ; ttl : int
    ; version : string
    }
end

module Station_status : sig
  type t =
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
end

module Station_status_data : sig
  type t = { stations : Station_status.t list }
end

module Station_status_feed : sig
  type t =
    { data : Station_status_data.t
    ; last_updated : int
    ; ttl : int
    ; version : string
    }
end

module Feed : sig
  type 'a t

  val station_information : Station_information_feed.t t
  val station_status : Station_status_feed.t t
end

module Discovered_endpoints : sig
  type t
end

val discover : string -> Discovered_endpoints.t Deferred.Or_error.t
val fetch : Discovered_endpoints.t -> 'a Feed.t -> 'a Deferred.Or_error.t
