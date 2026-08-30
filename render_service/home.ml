open! Core
open! Async

let render ~get_data =
  let open Deferred.Let_syntax in
  let query =
    { Data_service_rpc.Get_data.Query.citibike_station_ids =
        [ "66dc8768-0aca-11e7-82f6-3863bb44ef7c" ]
    ; mta_subway_realtime_feeds = []
    }
  in
  let%map response = get_data query in
  let debug_info =
    match response with
    | Ok response ->
      Data_service_rpc.Get_data.Response.sexp_of_t response |> Sexp.to_string_hum
    | Error error -> Error.to_string_hum error
  in
  { Render.text = "hello world"
  ; time_until_refresh = Time_ns.Span.of_sec 30.
  ; display_size = { Render.Display_size.width_cm = 16.3; height_cm = 9.8 }
  ; debug_info
  }
;;
