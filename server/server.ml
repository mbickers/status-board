open! Core
open! Async

let run ~cache_path ~port =
  let autoreload = Autoreload_on_restart.create ~monitor_path:[ "wait-for-restart" ] in
  let home_renderer = Home.renderer in
  let cache = Cache.create ~path:cache_path in
  let image_path input = [%string "/image/home?%{Renderer.url_query_string input}"] in
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun ~body _ request ->
         let path =
           request
           |> Cohttp.Request.uri
           |> Uri.path
           |> String.chop_prefix_if_exists ~prefix:"/"
           |> String.chop_suffix_if_exists ~suffix:"/"
           |> String.split ~on:'/'
         in
         (* I want to replace these repeated route and URL paths with a handler DSL. *)
         match Cohttp.Request.meth request, path with
         | `GET, [ "wait-for-restart" ] ->
           Autoreload_on_restart.respond autoreload request
         | `GET, [ "preview"; "home" ] ->
           Preview.respond
             ~autoreload_script:(Autoreload_on_restart.script autoreload)
             ~image_path:(fun debug_preset -> image_path (Preview debug_preset))
             ~renderer:home_renderer
             request
         | `GET, [ "image"; "home" ] ->
           Renderer.respond ~cache ~renderer:home_renderer request
         | _, "api" :: _ ->
           Trmnl.respond
             ~base_url:"/api"
             ~image_path:(fun device_status -> image_path (Device device_status))
             ~refresh_interval:home_renderer.refresh_interval
             ~body
             request
         | _ -> Http.respond_string ~status:`Not_found "Not found")
  in
  Deferred.never ()
;;
