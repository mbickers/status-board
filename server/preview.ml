open! Core
open! Async

type renderer = Cache.t -> Screen_render.t Deferred.Or_error.t

type t =
  { autoreload_script : string
  ; cache : Cache.t
  ; image_publisher : Image_publisher.t
  ; renderers : renderer String.Map.t
  ; template : Mustache.t
  }

let create ~autoreload_script ~cache ~image_publisher ~renderers =
  let%bind.Or_error contents =
    Or_error.try_with (fun () -> In_channel.read_all "server/preview.html")
  in
  let%map.Or_error template = Or_error.try_with (fun () -> Mustache.of_string contents) in
  { autoreload_script; cache; image_publisher; renderers; template }
;;

let page_html t ~image_url screen_render =
  let refresh_seconds =
    Time_ns.Span.to_sec screen_render.Screen_render.time_until_refresh
  in
  let template_data =
    `O
      [ "image_url", `String image_url
      ; "refresh_seconds", `String (Float.to_string_hum ~decimals:3 refresh_seconds)
      ; ( "refresh_milliseconds"
        , `String
            (Float.to_string_hum ~decimals:0 (Float.max 0. refresh_seconds *. 1_000.)) )
      ; "display_width_px", `String (Int.to_string screen_render.display_resolution.width)
      ; ( "display_height_px"
        , `String (Int.to_string screen_render.display_resolution.height) )
      ; "debug_info", `String screen_render.debug_info
      ; "autoreload_script", `String t.autoreload_script
      ]
  in
  Or_error.try_with (fun () -> Mustache.render t.template template_data)
;;

let response_action response =
  let%bind response = response in
  return (`Response response)
;;

let respond t ~name =
  match Map.find t.renderers name with
  | Some renderer ->
    let%bind screen_render = renderer t.cache in
    (match screen_render with
     | Ok screen_render ->
       let { Image_publisher.Publish_record.image_url } =
         Image_publisher.publish t.image_publisher ~name ~buffer:screen_render.buffer
       in
       (match page_html t ~image_url screen_render with
        | Ok html ->
          Cohttp_async.Server.respond_string
            ~headers:
              (Cohttp.Header.of_list
                 [ "content-type", "text/html; charset=utf-8"
                 ; "cache-control", "no-store"
                 ])
            html
          |> response_action
        | Error error ->
          Cohttp_async.Server.respond_string
            ~status:`Internal_server_error
            (Error.to_string_hum error)
          |> response_action)
     | Error error ->
       Cohttp_async.Server.respond_string
         ~status:`Internal_server_error
         (Error.to_string_hum error)
       |> response_action)
  | None ->
    Cohttp_async.Server.respond_string
      ~status:`Not_found
      [%string "No renderer named %{name}"]
    |> response_action
;;

let renderer_names t = Map.keys t.renderers
