open! Core
open! Async
module Gtfs = Mta_protobuf.Gtfs_realtime.Transit_realtime
module Rpc = Data_service_rpc.Mta
module Subway_realtime_feed = Rpc.Subway_realtime_feed

let realtime_feed_url = function
  | Subway_realtime_feed.Lines_1_2_3_4_5_6_7 ->
    "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs"
  | Lines_A_C_E ->
    "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs-ace"
  | Lines_B_D_F_M ->
    "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs-bdfm"
  | Line_G -> "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs-g"
  | Lines_J_Z -> "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs-jz"
  | Line_L -> "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs-l"
  | Lines_N_Q_R_W ->
    "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs-nqrw"
  | Staten_island_railway ->
    "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/nyct%2Fgtfs-si"
;;

let fetch_message url =
  let open Deferred.Or_error.Let_syntax in
  let%bind uri = Or_error.try_with (fun () -> Uri.of_string url) |> Deferred.return in
  let%bind response, body =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
  in
  let%bind contents =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Body.to_string body)
  in
  let status_code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  if Cohttp.Code.is_success status_code
  then
    Gtfs.FeedMessage.from_proto (Ocaml_protoc_plugin.Reader.create contents)
    |> Result.map_error ~f:(fun error ->
      Error.of_string (Ocaml_protoc_plugin.Result.show_error error))
    |> Deferred.return
  else Deferred.Or_error.errorf "MTA request to %s failed with HTTP %d" url status_code
;;

let time_ns_of_seconds_since_epoch seconds =
  match Int63.of_int64 seconds with
  | None -> Or_error.errorf "Timestamp is outside the Time_ns range: %Ld" seconds
  | Some seconds ->
    let%map.Or_error nanoseconds =
      Or_error.try_with (fun () ->
        Int63.Overflow_exn.(seconds * Int63.of_int 1_000_000_000))
    in
    Time_ns.of_int63_ns_since_epoch nanoseconds
;;

let station_id_of_stop_id stop_id =
  match String.chop_suffix stop_id ~suffix:"N" with
  | Some station_id -> station_id
  | None ->
    (match String.chop_suffix stop_id ~suffix:"S" with
     | Some station_id -> station_id
     | None -> stop_id)
;;

let arrival
      ~now
      (trip : Gtfs.TripDescriptor.t)
      (stop_time_update : Gtfs.TripUpdate.StopTimeUpdate.t)
  =
  match trip.route_id, stop_time_update.stop_id, stop_time_update.arrival with
  | Some route_id, Some stop_id, Some { time = Some arrival_time; _ } ->
    let%map.Or_error arrives_at = time_ns_of_seconds_since_epoch arrival_time in
    if Time_ns.compare arrives_at now < 0
    then None
    else
      Some
        ( station_id_of_stop_id stop_id
        , { Rpc.Arrival.route_id; trip_id = trip.trip_id; stop_id; arrives_at } )
  | _ -> Ok None
;;

let upcoming_arrivals feed_message =
  let now = Time_ns.now () in
  let open Or_error.Let_syntax in
  let%map arrivals =
    feed_message.Gtfs.FeedMessage.entity
    |> List.concat_map ~f:(fun entity ->
      match entity.Gtfs.FeedEntity.trip_update with
      | None -> []
      | Some trip_update ->
        List.map trip_update.stop_time_update ~f:(arrival ~now trip_update.trip))
    |> Or_error.combine_errors
  in
  arrivals
  |> List.filter_opt
  |> String.Map.of_alist_multi
  |> Map.map
       ~f:
         (List.sort ~compare:(fun left right ->
            Time_ns.compare left.Rpc.Arrival.arrives_at right.arrives_at))
;;

let fetch_upcoming_arrivals realtime_feed =
  let open Deferred.Or_error.Let_syntax in
  let%bind feed_message = realtime_feed |> realtime_feed_url |> fetch_message in
  upcoming_arrivals feed_message |> Deferred.return
;;

let translated_text translated_string =
  let translations = translated_string.Gtfs.TranslatedString.translation in
  let preferred =
    List.find translations ~f:(fun translation ->
      Option.exists translation.language ~f:(String.is_prefix ~prefix:"en"))
    |> Option.first_some
         (List.find translations ~f:(fun translation ->
            Option.is_none translation.language))
    |> Option.first_some (List.hd translations)
  in
  Option.map preferred ~f:(fun translation -> translation.text)
;;

let alert (entity : Gtfs.FeedEntity.t) =
  Option.map entity.alert ~f:(fun alert ->
    let affected_route_ids =
      List.filter_map alert.informed_entity ~f:(fun entity -> entity.route_id)
      |> List.dedup_and_sort ~compare:String.compare
    in
    let affected_stop_ids =
      List.filter_map alert.informed_entity ~f:(fun entity -> entity.stop_id)
      |> List.dedup_and_sort ~compare:String.compare
    in
    { Rpc.Alert.id = entity.id
    ; header = Option.bind alert.header_text ~f:translated_text
    ; description = Option.bind alert.description_text ~f:translated_text
    ; url = Option.bind alert.url ~f:translated_text
    ; affected_route_ids
    ; affected_stop_ids
    })
;;

let fetch_all_alerts () =
  let open Deferred.Or_error.Let_syntax in
  let%map feed_message =
    fetch_message
      "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds/camsys%2Fall-alerts"
  in
  List.filter_map feed_message.entity ~f:alert
;;
