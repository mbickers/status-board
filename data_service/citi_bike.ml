open! Core
open! Async

let join
  (station_information : Gbfs.station_information list)
  (station_statuses : Gbfs.station_status list)
  =
  let status_by_station_id =
    station_statuses
    |> List.map ~f:(fun (status : Gbfs.station_status) -> status.station_id, status)
    |> String.Map.of_alist_exn
  in
  station_information
  |> List.filter_map ~f:(fun (information : Gbfs.station_information) ->
    Map.find status_by_station_id information.station_id
    |> Option.map ~f:(fun status ->
      let station : Data_service_rpc.Station.t =
        { station_id = information.station_id
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
        ; last_reported = status.last_reported
        }
      in
      station.station_id, station))
  |> String.Map.of_alist_exn
;;

let fetch_snapshot () =
  let open Deferred.Let_syntax in
  let%bind discovery =
    Gbfs.fetch
      "https://gbfs.citibikenyc.com/gbfs/2.3/gbfs.json"
      Gbfs.discovery_of_yojson
  in
  let information_url = Gbfs.find_feed_url discovery "station_information" in
  let status_url = Gbfs.find_feed_url discovery "station_status" in
  let%map information_feed, status_feed =
    Deferred.both
      (Gbfs.fetch information_url Gbfs.station_information_feed_of_yojson)
      (Gbfs.fetch status_url Gbfs.station_status_feed_of_yojson)
  in
  join information_feed.data.stations status_feed.data.stations
;;
