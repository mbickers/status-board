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
    ; last_reported : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp]
end

let join station_information station_statuses =
  let%bind.Or_error status_by_station_id =
    station_statuses
    |> List.map ~f:(fun (status : Gbfs.Station_status.t) -> status.station_id, status)
    |> String.Map.of_alist_or_error
  in
  station_information
  |> List.filter_map ~f:(fun (information : Gbfs.Station_information.t) ->
    Map.find status_by_station_id information.station_id
    |> Option.map ~f:(fun status ->
      let station =
        { Station.station_id = information.station_id
        ; name = information.name
        ; latitude = information.lat
        ; longitude = information.lon
        ; capacity = information.capacity
        ; bikes_available = status.num_bikes_available
        ; ebikes_available = status.num_ebikes_available
        ; bikes_disabled = status.num_bikes_disabled
        ; docks_available = status.num_docks_available
        ; docks_disabled = status.num_docks_disabled
        ; is_installed = Int.equal status.is_installed 1
        ; is_renting = Int.equal status.is_renting 1
        ; is_returning = Int.equal status.is_returning 1
        ; last_reported =
            Time_ns.of_span_since_epoch (Time_ns.Span.of_int_sec status.last_reported)
        }
      in
      station.station_id, station))
  |> String.Map.of_alist_or_error
;;

let fetch () =
  let%bind.Deferred.Or_error gbfs =
    Gbfs.discover "https://gbfs.citibikenyc.com/gbfs/2.3/gbfs.json"
  in
  let%bind.Deferred.Or_error information_feed =
    Gbfs.fetch gbfs Gbfs.Feed.station_information
  and status_feed = Gbfs.fetch gbfs Gbfs.Feed.station_status in
  join information_feed.data.stations status_feed.data.stations |> return
;;

let cache_key =
  lazy
    (Cache.Key.create
       (module struct
         type t = Station.t String.Map.t [@@deriving sexp]
       end)
       ~filename:"citibike")
;;

let query cache =
  Cache.get cache (Lazy.force cache_key) ~max_age:(Time_ns.Span.of_sec 30.) ~fetch
;;
