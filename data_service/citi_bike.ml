open! Core
open! Async

let join
      (station_information : Gbfs.station_information list)
      (station_statuses : Gbfs.station_status list)
  =
  let open Or_error.Let_syntax in
  let%bind status_by_station_id =
    station_statuses
    |> List.map ~f:(fun (status : Gbfs.station_status) -> status.station_id, status)
    |> String.Map.of_alist_or_error
  in
  let%bind stations =
    station_information
    |> List.filter_map ~f:(fun (information : Gbfs.station_information) ->
      Map.find status_by_station_id information.station_id
      |> Option.map ~f:(fun status ->
        let%map last_reported_ns =
          Or_error.try_with (fun () ->
            Int63.Overflow_exn.(
              Int63.of_int status.last_reported * Int63.of_int 1_000_000_000))
        in
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
          ; last_reported = Time_ns.of_int63_ns_since_epoch last_reported_ns
          }
        in
        station.station_id, station))
    |> Or_error.combine_errors
  in
  String.Map.of_alist_or_error stations
;;

let fetch_snapshot () =
  let open Deferred.Or_error.Let_syntax in
  let%bind discovery =
    Gbfs.fetch "https://gbfs.citibikenyc.com/gbfs/2.3/gbfs.json" Gbfs.discovery_of_yojson
  in
  let%bind information_url =
    Gbfs.find_feed_url discovery "station_information" |> Deferred.return
  in
  let%bind status_url =
    Gbfs.find_feed_url discovery "station_status" |> Deferred.return
  in
  let%bind information_feed =
    Gbfs.fetch information_url Gbfs.station_information_feed_of_yojson
  and status_feed = Gbfs.fetch status_url Gbfs.station_status_feed_of_yojson in
  join information_feed.data.stations status_feed.data.stations |> Deferred.return
;;
