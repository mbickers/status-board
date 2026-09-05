open! Core
open! Async
module Gtfs = Mta_protobuf.Gtfs_realtime.Transit_realtime

module Realtime_feed = struct
  type t =
    | Lines_1_2_3_4_5_6_7
    | Lines_A_C_E
    | Lines_B_D_F_M
    | Line_G
    | Lines_J_Z
    | Line_L
    | Lines_N_Q_R_W
    | Staten_island_railway
  [@@deriving compare, sexp]
end

module Arrival = struct
  type t =
    { route_id : string
    ; trip_id : string option
    ; stop_id : string
    ; arrives_at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp]
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
  [@@deriving sexp]
end

module Feed = struct
  type _ t =
    | Realtime : Realtime_feed.t -> Arrival.t list String.Map.t t
    | All_alerts : Alert.t list t
end

module Stop_status = struct
  type t =
    { upcoming_arrivals : Arrival.t list
    ; alerts : Alert.t list
    }
  [@@deriving sexp]
end

module Status = struct
  type t =
    { stop_status_by_stop_id : Stop_status.t String.Map.t
    ; systemwide_alerts : Alert.t list
    }
  [@@deriving sexp]
end

let time_ns_of_seconds_since_epoch seconds =
  Or_error.try_with (fun () ->
    Time_ns.of_span_since_epoch (Time_ns.Span.of_int_sec (Int64.to_int_exn seconds)))
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
    Or_error.map (time_ns_of_seconds_since_epoch arrival_time) ~f:(fun arrives_at ->
      match Time_ns.compare arrives_at now < 0 with
      | true -> None
      | false ->
        Some
          ( station_id_of_stop_id stop_id
          , { Arrival.route_id; trip_id = trip.trip_id; stop_id; arrives_at } ))
  | _ -> Ok None
;;

let upcoming_arrivals (feed_message : Gtfs.FeedMessage.t) =
  let now = Time_ns.now () in
  let arrivals =
    feed_message.entity
    |> List.concat_map ~f:(fun entity ->
      match entity.trip_update with
      | None -> []
      | Some trip_update ->
        List.map trip_update.stop_time_update ~f:(arrival ~now trip_update.trip))
    |> Or_error.combine_errors
  in
  Or_error.map arrivals ~f:(fun arrivals ->
    arrivals
    |> List.filter_opt
    |> String.Map.of_alist_multi
    |> Map.map ~f:(fun arrivals ->
      arrivals
      |> List.sort ~compare:(fun left right ->
        Time_ns.compare left.Arrival.arrives_at right.arrives_at)))
;;

let translated_text (translated_string : Gtfs.TranslatedString.t) =
  let translations = translated_string.translation in
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
    { Alert.id = entity.id
    ; header = Option.bind alert.header_text ~f:translated_text
    ; description = Option.bind alert.description_text ~f:translated_text
    ; url = Option.bind alert.url ~f:translated_text
    ; affected_route_ids
    ; affected_stop_ids
    })
;;

let fetch_message (type result) (feed : result Feed.t) =
  let base_url = "https://api-endpoint.mta.info/Dataservice/mtagtfsfeeds" in
  let url =
    match feed with
    | Feed.Realtime realtime_feed ->
      base_url
      ^
        (match realtime_feed with
        | Realtime_feed.Lines_1_2_3_4_5_6_7 -> "/nyct%2Fgtfs"
        | Lines_A_C_E -> "/nyct%2Fgtfs-ace"
        | Lines_B_D_F_M -> "/nyct%2Fgtfs-bdfm"
        | Line_G -> "/nyct%2Fgtfs-g"
        | Lines_J_Z -> "/nyct%2Fgtfs-jz"
        | Line_L -> "/nyct%2Fgtfs-l"
        | Lines_N_Q_R_W -> "/nyct%2Fgtfs-nqrw"
        | Staten_island_railway -> "/nyct%2Fgtfs-si")
    | All_alerts -> base_url ^ "/camsys%2Fall-alerts"
  in
  let decode : Gtfs.FeedMessage.t -> result Or_error.t =
    match feed with
    | Feed.Realtime _ -> upcoming_arrivals
    | All_alerts -> fun feed_message -> Ok (List.filter_map feed_message.entity ~f:alert)
  in
  let%bind.Deferred.Or_error contents = Http.get_body url in
  let%bind.Deferred.Or_error feed_message =
    Gtfs.FeedMessage.from_proto (Ocaml_protoc_plugin.Reader.create contents)
    |> Core.Result.map_error ~f:(fun error ->
      Error.of_string (Ocaml_protoc_plugin.Result.show_error error))
    |> return
  in
  return (decode feed_message)
;;

let query cache ~which_feeds =
  let max_age = Time_ns.Span.of_sec 30. in
  let realtime_feeds = List.dedup_and_sort which_feeds ~compare:Realtime_feed.compare in
  let%bind upcoming_arrival_results =
    Deferred.List.map realtime_feeds ~how:`Parallel ~f:(fun realtime_feed ->
      let key =
        let feed_name =
          realtime_feed
          |> Realtime_feed.sexp_of_t
          |> Sexp.to_string_mach
          |> String.lowercase
        in
        [%string "mta-subway-%{feed_name}"]
      in
      Cache.get
        cache
        (module struct
          type t = Arrival.t list String.Map.t [@@deriving sexp]
        end)
        ~max_age
        ~fetch:(fun () -> fetch_message (Feed.Realtime realtime_feed))
        ~key)
  and all_alerts_result =
    Cache.get
      cache
      (module struct
        type t = Alert.t list [@@deriving sexp]
      end)
      ~max_age
      ~fetch:(fun () -> fetch_message Feed.All_alerts)
      ~key:"mta-all-alerts"
  in
  return
    (let%bind.Or_error completed_arrivals =
       upcoming_arrival_results
       |> List.map ~f:Latest_result.latest_success
       |> Or_error.combine_errors
     and all_alerts =
       all_alerts_result
       |> Latest_result.latest_success
       |> Or_error.map ~f:(fun completed -> completed.value)
     in
     (* comment *)
     let upcoming_arrivals_by_stop_id =
       completed_arrivals
       |> List.concat_map ~f:(fun completed -> Map.to_alist completed.value)
       |> String.Map.of_alist_multi
       |> Map.map ~f:(fun arrivals ->
         arrivals
         |> List.concat
         |> List.sort ~compare:(fun left right ->
           Time_ns.compare left.Arrival.arrives_at right.arrives_at))
     in
     let systemwide_alerts, targeted_alerts =
       List.partition_tf all_alerts ~f:(fun alert ->
         List.is_empty alert.affected_stop_ids && List.is_empty alert.affected_route_ids)
     in
     let stop_ids =
       Map.keys upcoming_arrivals_by_stop_id
       @ List.concat_map targeted_alerts ~f:(fun alert ->
         List.map alert.affected_stop_ids ~f:station_id_of_stop_id)
       |> String.Set.of_list
     in
     stop_ids
     |> Set.to_list
     |> List.map ~f:(fun stop_id ->
       let upcoming_arrivals =
         Map.find upcoming_arrivals_by_stop_id stop_id |> Option.value ~default:[]
       in
       let route_ids =
         upcoming_arrivals
         |> List.map ~f:(fun arrival -> arrival.route_id)
         |> String.Set.of_list
       in
       let alerts =
         List.filter targeted_alerts ~f:(fun alert ->
           List.exists alert.affected_stop_ids ~f:(fun affected_stop_id ->
             String.equal (station_id_of_stop_id affected_stop_id) stop_id)
           || List.exists alert.affected_route_ids ~f:(Set.mem route_ids))
       in
       stop_id, { Stop_status.upcoming_arrivals; alerts })
     |> String.Map.of_alist_or_error
     |> Or_error.map ~f:(fun stop_status_by_stop_id ->
       { Status.stop_status_by_stop_id; systemwide_alerts }))
;;
