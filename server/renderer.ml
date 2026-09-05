open! Core
open! Async

module Device_status = struct
  type t = { battery_voltage : float option } [@@deriving sexp]
end

module Input = struct
  type t =
    | Device of Device_status.t
    | Preview of string option
end

type t =
  { refresh_interval : Time_ns.Span.t
  ; debug_presets : string list
  ; render : Input.t -> Cache.t -> Image.image Deferred.Or_error.t
  }

let url_query_string input =
  let request_id = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string in
  let query =
    match input with
    | Input.Device device_status ->
      ("input", [ "device" ])
      :: Option.value_map
           device_status.Device_status.battery_voltage
           ~default:[]
           ~f:(fun battery_voltage ->
             [ "battery_voltage", [ Float.to_string battery_voltage ] ])
    | Preview debug_preset ->
      ("input", [ "preview" ])
      :: Option.value_map debug_preset ~default:[] ~f:(fun preset ->
        [ "preset", [ preset ] ])
  in
  Uri.encoded_of_query (("request", [ request_id ]) :: query)
;;

let input request =
  let uri = Cohttp.Request.uri request in
  match Uri.get_query_param uri "input" with
  | Some "device" ->
    let%map.Or_error battery_voltage =
      match Uri.get_query_param uri "battery_voltage" with
      | None -> Ok None
      | Some battery_voltage ->
        Or_error.try_with (fun () -> Float.of_string battery_voltage)
        |> Or_error.map ~f:Option.some
    in
    Input.Device { battery_voltage }
  | Some "preview" ->
    Uri.get_query_param uri "preset"
    |> Option.filter ~f:(Fn.non String.is_empty)
    |> Input.Preview
    |> Or_error.return
  | Some input -> Or_error.errorf "Unknown renderer input %S" input
  | None -> Or_error.error_string "Renderer image request has no input"
;;

let respond ~cache ~renderer request =
  let%bind rendered =
    match input request with
    | Ok input -> renderer.render input cache
    | Error error -> return (Error error)
  in
  match rendered with
  | Ok image ->
    Http.respond_string
      ~headers:
        (Cohttp.Header.of_list
           [ "content-type", "image/bmp"; "cache-control", "no-store" ])
      (Bmp.encode image)
  | Error error ->
    Http.respond_string ~status:`Internal_server_error (Error.to_string_hum error)
;;
