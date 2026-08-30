open! Core
open! Async

let run ~cache_path ~port ~preview_template_path =
  let autoreload =
    Autoreload_on_restart.create ~monitor_path_segment:"wait-for-restart"
  in
  let image_publisher = Image_publisher.create () in
  let%bind.Deferred.Or_error preview_handler =
    Preview.create
      ~autoreload_script:(Autoreload_on_restart.script autoreload)
      ~cache:(Cache.create ~path:cache_path)
      ~image_publisher
      ~renderers:(String.Map.of_alist_exn [ "home", Home.render ])
      ~template_path:preview_template_path
  in
  let%bind _server =
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
         | `GET, [ segment ]
           when String.equal
                  segment
                  (Autoreload_on_restart.monitor_path_segment autoreload) ->
           Autoreload_on_restart.respond autoreload request
         | `GET, [ "preview"; name ] -> Preview.respond preview_handler ~name
         | `GET, [ "image"; name ] -> Image_publisher.respond image_publisher ~name
         | _ -> Http.respond_string ~status:`Not_found "Not found")
  in
  Preview.renderer_names preview_handler
  |> List.iter ~f:(fun name ->
    let url = [%string "http://127.0.0.1:%{port#Int}/preview/%{name}"] in
    [%log.global.info "Preview available" (url : string)]);
  Deferred.never ()
;;
