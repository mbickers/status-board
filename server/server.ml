open! Core
open! Async

let run ~cache_path ~port =
  let autoreload = Autoreload_on_restart.create ~monitor_path:[ "wait-for-restart" ] in
  let image_publisher = Image_publisher.create () in
  let cache = Cache.create ~path:cache_path in
  let trmnl = Trmnl.create ~cache ~image_publisher ~name:"home" ~renderer:Home.render in
  let%bind.Deferred.Or_error preview_handler =
    Preview.create
      ~autoreload_script:(Autoreload_on_restart.script autoreload)
      ~cache
      ~image_publisher
      ~renderers:(String.Map.of_alist_exn [ "home", Home.render ])
    |> return
  in
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun ~body _ request ->
         let method_ = Cohttp.Request.meth request
         and path =
           request
           |> Cohttp.Request.uri
           |> Uri.path
           |> String.chop_prefix_if_exists ~prefix:"/"
           |> String.chop_suffix_if_exists ~suffix:"/"
           |> String.split ~on:'/'
         in
         (match path with
          | "api" :: _ | "image" :: _ | "setup-image" :: _ ->
            let headers = Cohttp.Request.headers request in
            let request_method = Cohttp.Code.string_of_method method_
            and firmware_version = Cohttp.Header.get headers "fw-version"
            and model = Cohttp.Header.get headers "model"
            and width = Cohttp.Header.get headers "width"
            and height = Cohttp.Header.get headers "height" in
            [%log.global.info
              "TRMNL request"
                (request_method : string)
                (path : string list)
                (firmware_version : string option)
                (model : string option)
                (width : string option)
                (height : string option)]
          | _ -> ());
         match method_, path with
         | `GET, path
           when List.equal
                  String.equal
                  path
                  (Autoreload_on_restart.monitor_path autoreload) ->
           Autoreload_on_restart.respond autoreload request
         | `GET, [ "preview"; name ] -> Preview.respond preview_handler ~name
         | `GET, [ "image"; name ] -> Image_publisher.respond image_publisher ~name
         | `GET, [ "setup-image"; name ] ->
           Image_publisher.respond_setup_image image_publisher ~name
         | `GET, [ "api"; "setup" ] -> Trmnl.respond_setup trmnl ~request
         | `GET, [ "api"; "display" ] -> Trmnl.respond_display trmnl ~request
         | `POST, [ "api"; "log" ] ->
           let%bind body = Cohttp_async.Body.to_string body in
           [%log.global.error "TRMNL firmware log" (body : string)];
           Http.respond_string ""
         | _ -> Http.respond_string ~status:`Not_found "Not found")
  in
  Preview.renderer_names preview_handler
  |> List.iter ~f:(fun name ->
    let url = [%string "http://127.0.0.1:%{port#Int}/preview/%{name}"] in
    [%log.global.info "Preview available" (url : string)]);
  Deferred.never ()
;;
