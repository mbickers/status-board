open! Core
open! Async

let run ~port =
  let open Deferred.Let_syntax in
  let citi_bike =
    Cached_data.create ~max_age:(Time_ns.Span.of_sec 30.) ~fetch:Citi_bike.fetch_snapshot
  in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement Data_service_rpc.Get_data.rpc (fun () query ->
            let%map result = Cached_data.get citi_bike in
            let station_ids = String.Set.of_list query.citibike_station_ids in
            let stations =
              Data_service_rpc.Latest_result.map result ~f:(fun snapshot ->
                Map.filter_keys snapshot ~f:(Set.mem station_ids))
            in
            { Data_service_rpc.Get_data.Response.stations })
        ]
      ~on_unknown_rpc:`Raise
  in
  let%bind _server =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _client_identity _connection -> ())
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  printf "data_service RPC listening on port %d\n%!" port;
  Deferred.never ()
;;
