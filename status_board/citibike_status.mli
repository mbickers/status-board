open! Core

module Availability : sig
  type t =
    | Renting of
        { classic_bikes_available : int
        ; electric_bikes_available : int
        }
    | Not_renting
end

module Parking : sig
  type t =
    | Accepting_returns of { docks_available : int }
    | Not_accepting_returns
end

type t =
  { availability : Availability.t
  ; parking : Parking.t
  ; bikes_available_frac : float
  }

val create : Feeds.Citibike.Station.t -> t
val availability_size : Status_box.Style.t -> int * int
val parking_size : Status_box.Style.t -> int * int

val draw_availability
  :  Drawing.Context.t
  -> anchor:Drawing.Anchor.t
  -> style:Status_box.Style.t
  -> label:string
  -> t
  -> unit

val draw_parking
  :  Drawing.Context.t
  -> anchor:Drawing.Anchor.t
  -> style:Status_box.Style.t
  -> label:string
  -> t
  -> unit
