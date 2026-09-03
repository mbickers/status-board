open! Core
open! Async

type t =
  { autoreload_script : string
  ; cache : Cache.t
  ; image_publisher : Image_publisher.t
  ; renderers : Renderer.packed String.Map.t
  ; template : Mustache.t
  }

let create ~autoreload_script ~cache ~image_publisher ~renderers =
  let%bind.Or_error contents =
    Or_error.try_with (fun () -> In_channel.read_all "server/preview.html")
  in
  let%map.Or_error template = Or_error.try_with (fun () -> Mustache.of_string contents) in
  { autoreload_script; cache; image_publisher; renderers; template }
;;

let page_html t ~image_url ~debug_preset ~debug_preset_names screen_render =
  let refresh_seconds =
    Time_ns.Span.to_sec screen_render.Renderer.Render.time_until_refresh
  in
  let template_data =
    `O
      [ "image_url", `String image_url
      ; "refresh_seconds", `String (Float.to_string_hum ~decimals:3 refresh_seconds)
      ; ( "refresh_milliseconds"
        , `String
            (Float.to_string_hum ~decimals:0 (Float.max 0. refresh_seconds *. 1_000.)) )
      ; "display_width_px", `String (Int.to_string screen_render.buffer.width)
      ; "display_height_px", `String (Int.to_string screen_render.buffer.height)
      ; "autoreload_script", `String t.autoreload_script
      ; "no_debug_preset_selected", `Bool (Option.is_none debug_preset)
      ; ( "debug_presets"
        , `A
            (List.map debug_preset_names ~f:(fun name ->
               `O
                 [ "name", `String name
                 ; "selected", `Bool (Option.equal String.equal debug_preset (Some name))
                 ])) )
      ]
  in
  Or_error.try_with (fun () -> Mustache.render t.template template_data)
;;

let respond t ~request ~name =
  match Map.find t.renderers name with
  | Some renderer ->
    let debug_preset = Uri.get_query_param (Cohttp.Request.uri request) "preset" in
    let%bind screen_render = Renderer.render_preview renderer ~debug_preset t.cache in
    let html =
      let%bind.Or_error screen_render = screen_render in
      let { Image_publisher.Publish_record.image_url; filename = _ } =
        Image_publisher.publish t.image_publisher ~name ~buffer:screen_render.buffer
      in
      page_html
        t
        ~image_url
        ~debug_preset
        ~debug_preset_names:(Renderer.debug_preset_names renderer)
        screen_render
    in
    (match html with
     | Ok html ->
       Http.respond_string
         ~headers:
           (Cohttp.Header.of_list
              [ "content-type", "text/html; charset=utf-8"; "cache-control", "no-store" ])
         html
     | Error error ->
       Http.respond_string ~status:`Internal_server_error (Error.to_string_hum error))
  | None -> Http.respond_string ~status:`Not_found [%string "No renderer named %{name}"]
;;

let renderer_names t = Map.keys t.renderers
