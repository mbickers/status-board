open! Core
open! Async

module Realtime_feed : sig
  type t =
    | Lines_1_2_3_4_5_6_7
    | Lines_A_C_E
    | Lines_B_D_F_M
    | Line_G
    | Lines_J_Z
    | Line_L
    | Lines_N_Q_R_W
    | Staten_island_railway
end

module Arrival : sig
  type t =
    { route_id : string
    ; trip_id : string option
    ; stop_id : string
    ; arrives_at : Time_ns.Alternate_sexp.t
    }
end

module Alert : sig
  type t =
    { id : string
    ; header : string option
    ; description : string option
    ; url : string option
    ; affected_route_ids : string list
    ; affected_stop_ids : string list
    }
end

module Stop_status : sig
  type t =
    { upcoming_arrivals : Arrival.t list
    ; alerts : Alert.t list
    }
end

val query
  :  Cache.t
  -> which_feeds:Realtime_feed.t list
  -> Stop_status.t String.Map.t Deferred.Or_error.t
