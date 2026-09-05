open! Core
open! Async

let preview_url ~debug_preset =
  match debug_preset with
  | None -> "?"
  | Some preset -> Uri.make ~query:[ "preset", [ preset ] ] () |> Uri.to_string
;;

let page_html ~autoreload_script ~template ~image_path ~debug_preset ~debug_presets =
  let template_data =
    `O
      [ "image_url", `String image_path
      ; "autoreload_script", `String autoreload_script
      ; "default_url", `String (preview_url ~debug_preset:None)
      ; "default_selected", `Bool (Option.is_none debug_preset)
      ; ( "debug_presets"
        , `A
            (List.map debug_presets ~f:(fun preset ->
               `O
                 [ "name", `String preset
                 ; "url", `String (preview_url ~debug_preset:(Some preset))
                 ; ( "selected"
                   , `Bool (Option.equal String.equal debug_preset (Some preset)) )
                 ])) )
      ]
  in
  Or_error.try_with (fun () -> Mustache.render template template_data)
;;

let respond ~autoreload_script ~image_path ~(status_board : Status_board.t) request =
  let debug_preset =
    Uri.get_query_param (Cohttp.Request.uri request) "preset"
    |> Option.filter ~f:(Fn.non String.is_empty)
  in
  let image_path = image_path debug_preset in
  match
    let%bind.Or_error contents =
      Or_error.try_with (fun () -> In_channel.read_all "server/preview.html")
    in
    let%bind.Or_error template =
      Or_error.try_with (fun () -> Mustache.of_string contents)
    in
    page_html
      ~autoreload_script
      ~template
      ~image_path
      ~debug_preset
      ~debug_presets:status_board.debug_presets
  with
  | Ok html ->
    Http.respond_string
      ~headers:
        (Cohttp.Header.of_list
           [ "content-type", "text/html; charset=utf-8"; "cache-control", "no-store" ])
      html
  | Error error ->
    Http.respond_string ~status:`Internal_server_error (Error.to_string_hum error)
;;
