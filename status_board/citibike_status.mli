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
val availability : style:Status_box.Style.t -> label:string -> t -> Drawing.Element.t
val parking : style:Status_box.Style.t -> label:string -> t -> Drawing.Element.t

module Testing_data : sig
  val dense_text : widest_two_digit_number:int -> t
  val errors : t
end
