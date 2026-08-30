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

module Mta = struct
  module Subway_realtime_feed = struct
    type t =
      | Lines_1_2_3_4_5_6_7
      | Lines_A_C_E
      | Lines_B_D_F_M
      | Line_G
      | Lines_J_Z
      | Line_L
      | Lines_N_Q_R_W
      | Staten_island_railway
    [@@deriving bin_io, compare, enumerate, sexp]
  end

  module Arrival = struct
    type t =
      { route_id : string
      ; trip_id : string option
      ; stop_id : string
      ; arrives_at : Time_ns.Alternate_sexp.t
      }
    [@@deriving bin_io, sexp]
  end

  module Subway_upcoming_arrivals = struct
    type t =
      { realtime_feed : Subway_realtime_feed.t
      ; upcoming_arrivals_by_station_id : Arrival.t list String.Map.t Latest_result.t
      }
    [@@deriving bin_io, sexp]
  end

  module Alert = struct
    type t =
      { id : string
      ; header : string option
      ; description : string option
      ; url : string option
      ; affected_route_ids : string list
      ; affected_stop_ids : string list
      }
    [@@deriving bin_io, sexp]
  end
end

module Get_data = struct
  module Query = struct
    type t =
      { citibike_station_ids : string list
      ; mta_subway_realtime_feeds : Mta.Subway_realtime_feed.t list
      }
    [@@deriving bin_io, sexp]
  end

  module Response = struct
    type t =
      { stations : Station.t String.Map.t Latest_result.t
      ; mta_subway_upcoming_arrivals : Mta.Subway_upcoming_arrivals.t list
      ; mta_all_alerts : Mta.Alert.t list Latest_result.t
      }
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
