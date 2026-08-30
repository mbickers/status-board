open! Core
open! Async
module Latest_result = Latest_result

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
    ; last_reported : Time_ns.Alternate_sexp.t
    }
  [@@deriving bin_io, sexp]
end

module Get_data = struct
  module Query = struct
    type t = { citibike_station_ids : string list } [@@deriving bin_io, sexp]
  end

  module Response = struct
    type t = { stations : Station.t String.Map.t Latest_result.t }
    [@@deriving bin_io, sexp]
  end

  let rpc =
    Rpc.Rpc.create
      ~name:"get-data"
      ~version:1
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~include_in_error_count:Rpc.How_to_recognise_errors.Only_on_exn
  ;;
end
