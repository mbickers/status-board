open! Core
open! Async

let station_id = "66dc8768-0aca-11e7-82f6-3863bb44ef7c"

let rec connect () =
  let open Deferred.Let_syntax in
  let where_to_connect =
    Host_and_port.create ~host:"127.0.0.1" ~port:8090
    |> Tcp.Where_to_connect.of_host_and_port
  in
  let%bind result = Rpc.Connection.client where_to_connect in
  match result with
  | Ok connection -> return connection
  | Error error ->
    eprintf "Waiting for data_service: %s\n%!" (Exn.to_string error);
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 1.) in
    connect ()
;;

let print_station (station : Data_service_rpc.Station.t) =
  printf
    "%s\nStation ID: %s\nLocation: %.6f, %.6f\nCapacity: %d\nBikes available: %d\nE-bikes available: %d\nBikes disabled: %d\nDocks available: %d\nDocks disabled: %d\nInstalled: %b\nRenting: %b\nReturning: %b\nLast reported: %d\n%!"
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
    station.last_reported
;;

let run () =
  let open Deferred.Let_syntax in
  let%bind connection = connect () in
  let%bind response =
    Rpc.Rpc.dispatch Data_service_rpc.Get_station.rpc connection station_id
  in
  (match response with
   | Error error -> raise (Error.to_exn error)
   | Ok None -> raise_s [%message "Citi Bike station was not found" station_id]
   | Ok (Some station) -> print_station station);
  Deferred.never ()
;;
