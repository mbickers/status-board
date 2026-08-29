open! Core
open! Async

let rpc_port = 8090
let refresh_interval = Time_ns.Span.of_sec 30.

let rec refresh stations =
  let open Deferred.Let_syntax in
  let%bind () = Clock_ns.after refresh_interval in
  let%bind result = Monitor.try_with Citi_bike.fetch_snapshot in
  (match result with
   | Ok snapshot -> stations := snapshot
   | Error error ->
     eprintf "Citi Bike refresh failed: %s\n%!" (Exn.to_string error));
  refresh stations
;;

let run () =
  let open Deferred.Let_syntax in
  let%bind initial_snapshot = Citi_bike.fetch_snapshot () in
  let stations = ref initial_snapshot in
  don't_wait_for (refresh stations);
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement Data_service_rpc.Get_station.rpc (fun () station_id ->
            return (Map.find !stations station_id))
        ]
      ~on_unknown_rpc:`Raise
  in
  let%bind _server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _client_identity _connection -> ())
      ~where_to_listen:(Tcp.Where_to_listen.of_port rpc_port)
      ()
  in
  printf "data_service RPC listening on port %d\n%!" rpc_port;
  Deferred.never ()
;;
