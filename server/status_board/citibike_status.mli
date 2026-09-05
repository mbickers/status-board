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

val draw_availability
  :  Graphics.Drawing.Context.t
  -> anchor:Graphics.Drawing.Anchor.t
  -> style:Status_box.Style.t
  -> title:string
  -> box_size:int * int
  -> t
  -> unit

val draw_parking
  :  Graphics.Drawing.Context.t
  -> anchor:Graphics.Drawing.Anchor.t
  -> style:Status_box.Style.t
  -> title:string
  -> box_size:int * int
  -> t
  -> unit
