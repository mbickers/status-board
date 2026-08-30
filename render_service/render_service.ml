open! Core
open! Async

let station_ids = [ "66dc8768-0aca-11e7-82f6-3863bb44ef7c" ]

let rec connect data_service_host_and_port =
  let open Deferred.Let_syntax in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port data_service_host_and_port
  in
  let%bind result = Rpc.Connection.client where_to_connect in
  match result with
  | Ok connection -> return connection
  | Error error ->
    eprintf "Waiting for data_service: %s\n%!" (Exn.to_string error);
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 1.) in
    connect data_service_host_and_port
;;

let print_station (station : Data_service_rpc.Station.t) =
  printf
    "%s\n\
     Station ID: %s\n\
     Location: %.6f, %.6f\n\
     Capacity: %d\n\
     Bikes available: %d\n\
     E-bikes available: %d\n\
     Bikes disabled: %d\n\
     Docks available: %d\n\
     Docks disabled: %d\n\
     Installed: %b\n\
     Renting: %b\n\
     Returning: %b\n\
     Last reported: %s\n\
     %!"
    station.name
    station.station_id
    station.latitude
    station.longitude
    station.capacity
    station.bikes_available
    station.ebikes_available
    station.bikes_disabled
    station.docks_available
    station.docks_disabled
    station.is_installed
    station.is_renting
    station.is_returning
    (Time_ns.to_string_utc station.last_reported)
;;

let print_stations stations ~station_ids =
  List.iter station_ids ~f:(fun station_id ->
    match Map.find stations station_id with
    | Some station -> print_station station
    | None -> eprintf "Citi Bike station was not found: %s\n%!" station_id)
;;

let run ~data_service_host_and_port =
  let open Deferred.Let_syntax in
  let%bind connection = connect data_service_host_and_port in
  let query = { Data_service_rpc.Get_data.Query.citibike_station_ids = station_ids } in
  match%bind Rpc.Rpc.dispatch Data_service_rpc.Get_data.rpc connection query with
  | Error error ->
    eprintf "Citi Bike RPC failed: %s\n%!" (Error.to_string_hum error);
    Shutdown.exit 1
  | Ok response ->
    (match response.stations.value with
     | Data_service_rpc.Latest_result.Success stations ->
       print_stations stations ~station_ids;
       Deferred.never ()
     | Data_service_rpc.Latest_result.Error { error; last_good = None } ->
       eprintf "Citi Bike fetch failed: %s\n%!" (Error.to_string_hum error);
       Shutdown.exit 1
     | Data_service_rpc.Latest_result.Error { error; last_good = Some last_good } ->
       eprintf
         "Citi Bike refresh failed; using data fetched at %s: %s\n%!"
         (Time_ns.to_string_utc last_good.at)
         (Error.to_string_hum error);
       print_stations last_good.value ~station_ids;
       Deferred.never ())
;;
