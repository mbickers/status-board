open! Core
open! Async

let run ~port =
  let open Deferred.Let_syntax in
  let max_age = Time_ns.Span.of_sec 30. in
  let citibike = Cached_data.create ~max_age ~fetch:Citibike.fetch_snapshot in
  let mta_subway_realtime_feeds =
    Data_service_rpc.Mta.Subway_realtime_feed.all
    |> List.map ~f:(fun realtime_feed ->
      ( realtime_feed
      , Cached_data.create ~max_age ~fetch:(fun () ->
          Mta.fetch_upcoming_arrivals realtime_feed) ))
    |> Map.Poly.of_alist_exn
  in
  let mta_all_alerts = Cached_data.create ~max_age ~fetch:Mta.fetch_all_alerts in
  let implementations =
    Rpc.Implementations.create_exn
      ~implementations:
        [ Rpc.Rpc.implement Data_service_rpc.Get_data.rpc (fun () query ->
            let requested_mta_subway_realtime_feeds =
              List.dedup_and_sort
                query.mta_subway_realtime_feeds
                ~compare:Data_service_rpc.Mta.Subway_realtime_feed.compare
            in
            let%map result = Cached_data.get citibike
            and mta_all_alerts = Cached_data.get mta_all_alerts
            and mta_subway_upcoming_arrivals =
              Deferred.List.map
                requested_mta_subway_realtime_feeds
                ~how:`Parallel
                ~f:(fun realtime_feed ->
                  let%map upcoming_arrivals_by_station_id =
                    realtime_feed
                    |> Map.find_exn mta_subway_realtime_feeds
                    |> Cached_data.get
                  in
                  { Data_service_rpc.Mta.Subway_upcoming_arrivals.realtime_feed
                  ; upcoming_arrivals_by_station_id
                  })
            in
            let station_ids = String.Set.of_list query.citibike_station_ids in
            let stations =
              Data_service_rpc.Latest_result.map result ~f:(fun snapshot ->
                Map.filter_keys snapshot ~f:(Set.mem station_ids))
            in
            { Data_service_rpc.Get_data.Response.stations
            ; mta_subway_upcoming_arrivals
            ; mta_all_alerts
            })
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
