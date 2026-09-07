open! Core
open! Async

let render ~cache_path ~preset ~filename =
  let cache = Feeds.Cache.create ~path:cache_path in
  let message_manager = Message_manager.create ~filename:"messages.sexp" in
  let%bind.Deferred.Or_error message = Message_manager.latest message_manager in
  let%bind.Deferred.Or_error bitmap =
    Home.status_board.render (Preview preset) cache ~message
  in
  Deferred.Or_error.try_with (fun () ->
    Writer.save filename ~contents:(Bitmap.encode_bmp bitmap))
;;

let run ~cache_path ~port ~autoreload =
  let home_status_board = Home.status_board in
  let cache = Feeds.Cache.create ~path:cache_path in
  let message_manager = Message_manager.create ~filename:"messages.sexp" in
  let routes = [] in
  let routes, autoreload =
    match autoreload with
    | false -> routes, None
    | true ->
      let path = "/wait-for-restart" in
      let autoreload = Autoreload_on_restart.create ~monitor_path:path in
      ( Http.Handler.Route.create
          (`Exact path)
          ~f:(Autoreload_on_restart.respond autoreload)
        :: routes
      , Some autoreload )
  in
  let routes, renderer =
    let path = "/image/home" in
    let renderer =
      Renderer.create ~path ~cache ~message_manager ~status_board:home_status_board
    in
    ( Http.Handler.Route.create (`Exact path) ~f:(Renderer.respond renderer) :: routes
    , renderer )
  in
  let routes =
    [ Http.Handler.Route.create
        (`Exact "/preview/home")
        ~f:
          (Preview.respond
             ~autoreload_script:(Option.map autoreload ~f:Autoreload_on_restart.script)
             ~image_path:(fun preset -> Renderer.image_path renderer (Preview preset))
             ~status_board:home_status_board)
    ; Http.Handler.Route.create
        (`Exact "/messages")
        ~f:
          (Message_manager.respond message_manager ~validate:(fun message ->
             Home.render_message ~fill:(Drawing.Fill.solid `b) message
             |> Or_error.map ~f:ignore))
    ; Http.Handler.Route.create
        (`Prefix "/api")
        ~f:
          (Trmnl.respond
             ~image_path:(fun device_status ->
               Renderer.image_path renderer (Device device_status))
             ~refresh_interval:home_status_board.refresh_interval)
    ; Http.Handler.Route.create
        (`Exact "/style.css")
        ~f:(Http.respond_file ~content_type:"text/css; charset=utf-8" "server/style.css")
    ; Http.Handler.Route.create
        (`Exact "/")
        ~f:
          (Http.respond_file ~content_type:"text/html; charset=utf-8" "server/index.html")
    ]
    @ routes
  in
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun ~body _ request -> Http.Handler.Route.respond routes ~body request)
  in
  Deferred.never ()
;;
