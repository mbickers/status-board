open! Core
open! Async

let parse_device_status request =
  let headers = Cohttp.Request.headers request in
  let battery_voltage =
    match Cohttp.Header.get headers "battery-voltage" with
    | None -> None
    | Some battery_voltage ->
      (match Or_error.try_with (fun () -> Float.of_string battery_voltage) with
       | Ok battery_voltage -> Some battery_voltage
       | Error error ->
         [%log.global.error "Invalid TRMNL device status" (error : Error.t)];
         None)
  in
  { Status_board.Device_status.battery_voltage }
;;

(* The TRMNL API needs path with server. *)
let image_url_with_origin ~image_path ~request =
  let device_status = parse_device_status request in
  let%map.Or_error origin = Http.request_origin request in
  [%string "%{origin}%{image_path device_status}"]
;;

let unique_filename () =
  Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string
;;

let respond_with_json response =
  match response with
  | Ok json ->
    Http.string_response
      ~headers:
        (Cohttp.Header.of_list
           [ "content-type", "application/json"; "cache-control", "no-store" ])
      (Yojson.Safe.to_string json)
  | Error error ->
    Http.string_response ~status:`Internal_server_error (Error.to_string_hum error)
;;

let respond ~path:(`Prefix path) ~image_path ~refresh_interval ~body request =
  let request_method = Cohttp.Request.meth request |> Cohttp.Code.string_of_method
  and request_url = Cohttp.Request.uri request |> Uri.to_string
  and headers = Cohttp.Request.headers request |> Cohttp.Header.to_list in
  [%log.global.info
    "TRMNL request"
      (request_method : string)
      (request_url : string)
      (headers : (string * string) list)];
  let path = Http.request_path request |> String.chop_prefix ~prefix:path in
  match Cohttp.Request.meth request, path with
  | `GET, Some "/setup" ->
    respond_with_json
      (let%map.Or_error image_url = image_url_with_origin ~image_path ~request in
       `Assoc
         [ "status", `Int 200
         ; "api_key", `String "status-board"
         ; "friendly_id", `String "STATUS"
         ; "image_url", `String image_url
         ; "filename", `String (unique_filename ())
         ])
  | `GET, Some "/display" ->
    respond_with_json
      (let%map.Or_error image_url = image_url_with_origin ~image_path ~request in
       let refresh_seconds =
         Time_ns.Span.to_sec refresh_interval |> Float.iround_up_exn
       in
       `Assoc
         [ "status", `Int 0
         ; "filename", `String (unique_filename ())
         ; "firmware_url", `Null
         ; "firmware_version", `Null
         ; "image_url", `String image_url
         ; "image_url_timeout", `Int 0
         ; "maximum_compatibility", `Bool false
         ; "refresh_rate", `Int refresh_seconds
         ; "reset_firmware", `Bool false
         ; "special_function", `String "none"
         ; "temperature_profile", `String "default"
         ; "touchbar_mode", `String "tap"
         ; "update_firmware", `Bool false
         ])
  | `POST, Some "/log" ->
    let%bind body = Cohttp_async.Body.to_string body in
    [%log.global.error "TRMNL firmware log" (body : string)];
    Http.string_response ""
  | _ -> Http.string_response ~status:`Not_found "Not found"
;;
