open! Core
open! Async

let render ~cache_path ~preset ~filename =
  let cache = Feeds.Cache.create ~path:cache_path in
  let%bind.Deferred.Or_error message = Message.latest ~filename:"messages.sexp" in
  let%bind.Deferred.Or_error image =
    Home.status_board.render (Preview preset) cache ~message
  in
  Deferred.Or_error.try_with (fun () -> Writer.save filename ~contents:(Bmp.encode image))
;;

let run ~cache_path ~port ~autoreload =
  (* I want to replace repeated hardcoded route with a handler DSL. *)
  let autoreload =
    match autoreload with
    | false -> None
    | true -> Some (Autoreload_on_restart.create ~monitor_path:[ "wait-for-restart" ])
  in
  let home_status_board = Home.status_board in
  let cache = Feeds.Cache.create ~path:cache_path in
  let messages_filename = "messages.sexp" in
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
         match Cohttp.Request.meth request, path with
         | `GET, [ "" ] ->
           Http.respond_file ~content_type:"text/html; charset=utf-8" "server/index.html"
         | `GET, [ "style.css" ] ->
           Http.respond_file ~content_type:"text/css; charset=utf-8" "server/style.css"
         | `GET, [ "wait-for-restart" ] ->
           (match autoreload with
            | None -> Http.respond_string ~status:`Not_found "Not found"
            | Some autoreload -> Autoreload_on_restart.respond autoreload request)
         | `GET, [ "preview"; "home" ] ->
           Preview.respond
             ~autoreload_script:(Option.map autoreload ~f:Autoreload_on_restart.script)
             ~image_path:(fun debug_preset -> image_path (Preview debug_preset))
             ~status_board:home_status_board
             request
         | `GET, [ "image"; "home" ] ->
           let%bind message = Message.latest ~filename:messages_filename in
           (match message with
            | Ok message ->
              Renderer.respond ~cache ~message ~status_board:home_status_board request
            | Error error ->
              Http.respond_string
                ~status:`Internal_server_error
                (Error.to_string_hum error))
         | _, [ "messages" ] ->
           Message.respond
             ~filename:messages_filename
             ~validate:(fun message ->
               Home.render_message message |> Or_error.map ~f:ignore)
             ~body
             request
         | _, "api" :: _ ->
           Trmnl.respond
             ~base_url:"/api"
             ~image_path:(fun device_status -> image_path (Device device_status))
             ~refresh_interval:home_status_board.refresh_interval
             ~body
             request
         | _ -> Http.respond_string ~status:`Not_found "Not found")
  in
  Deferred.never ()
;;
