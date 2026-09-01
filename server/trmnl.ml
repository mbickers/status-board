open! Core
open! Async

type renderer = Cache.t -> Screen_render.t Deferred.Or_error.t

type t =
  { cache : Cache.t
  ; image_publisher : Image_publisher.t
  ; name : string
  ; renderer : renderer
  }

let create ~cache ~image_publisher ~name ~renderer =
  { cache; image_publisher; name; renderer }
;;

let request_origin request =
  let headers = Cohttp.Request.headers request in
  let scheme =
    Cohttp.Header.get headers "x-forwarded-proto" |> Option.value ~default:"http"
  in
  match
    Cohttp.Header.get headers "x-forwarded-host", Cohttp.Header.get headers "host"
  with
  | Some host, _ | None, Some host -> Ok [%string "%{scheme}://%{host}"]
  | None, None -> Or_error.error_string "Request has no Host header"
;;

let render_and_publish t ~request =
  let%bind screen_render = t.renderer t.cache in
  return
    (let%bind.Or_error screen_render = screen_render in
     let%bind.Or_error origin = request_origin request in
     let published =
       Image_publisher.publish t.image_publisher ~name:t.name ~buffer:screen_render.buffer
     in
     Ok (screen_render, published, [%string "%{origin}%{published.image_url}"]))
;;

let respond_with_json response =
  match response with
  | Ok json ->
    Http.respond_string
      ~headers:
        (Cohttp.Header.of_list
           [ "content-type", "application/json"; "cache-control", "no-store" ])
      (Yojson.Safe.to_string json)
  | Error error ->
    Http.respond_string ~status:`Internal_server_error (Error.to_string_hum error)
;;

let respond_setup t ~request =
  let%bind result = render_and_publish t ~request in
  respond_with_json
    (let%map.Or_error _, published, image_url = result in
     `Assoc
       [ "status", `Int 200
       ; "api_key", `String "status-board"
       ; "friendly_id", `String "STATUS"
       ; "image_url", `String image_url
       ; "filename", `String published.filename
       ])
;;

let respond_display t ~request =
  let%bind result = render_and_publish t ~request in
  respond_with_json
    (let%map.Or_error screen_render, published, image_url = result in
     let refresh_seconds =
       screen_render.time_until_refresh |> Time_ns.Span.to_sec |> Float.iround_up_exn
     in
     `Assoc
       [ "status", `Int 0
       ; "filename", `String published.filename
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
;;
