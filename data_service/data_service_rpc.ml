open! Core
open! Async

module Station = struct
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
    ; last_reported : int
    }
  [@@deriving bin_io, sexp]
end

module Get_station = struct
  module Query = struct
    type t = string [@@deriving bin_io, sexp]
  end

  module Response = struct
    type t = Station.t option [@@deriving bin_io, sexp]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-citi-bike-station"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Rpc.How_to_recognise_errors.Only_on_exn
  ;;
end
