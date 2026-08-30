open! Core
open! Async

let run ~cache_path ~port =
  let autoreload = Autoreload_on_restart.create ~monitor_path:"/wait-for-restart" in
  let%bind.Deferred.Or_error preview_handler =
    Preview.create
      ~autoreload_script:(Autoreload_on_restart.script autoreload)
      ~cache:(Cache.create ~path:cache_path)
      ~renderers:(String.Map.of_alist_exn [ "home", Home.render ])
    |> Deferred.return
  in
  let%bind.Deferred _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun ~body:_ _ request ->
         match
           ( Cohttp.Request.meth request
           , request
             |> Cohttp.Request.uri
             |> Uri.path
             |> String.chop_prefix_if_exists ~prefix:"/"
             |> String.split ~on:'/' )
         with
         | `GET, [ path ]
           when String.equal
                  path
                  (Autoreload_on_restart.monitor_path autoreload
                   |> String.chop_prefix_if_exists ~prefix:"/") ->
           Autoreload_on_restart.respond autoreload request
         | `GET, [ "preview"; name ] -> Preview.respond preview_handler ~name
         | _ ->
           let%map.Deferred response =
             Cohttp_async.Server.respond_string ~status:`Not_found "Not found"
           in
           `Response response)
  in
  Preview.renderer_names preview_handler
  |> List.iter ~f:(fun name ->
    [%log.global.info
      "Preview available"
        ([%string "http://127.0.0.1:%{port#Int}/preview/%{name}"] : string)]);
  Deferred.never ()
;;
