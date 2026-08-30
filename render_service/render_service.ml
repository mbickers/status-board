open! Core
open! Async

let rec connect data_service_host_and_port =
  let open Deferred.Let_syntax in
  let where_to_connect =
    Tcp.Where_to_connect.of_host_and_port data_service_host_and_port
  in
  let%bind result = Rpc.Connection.client where_to_connect in
  match result with
  | Ok connection -> return connection
  | Error error ->
    eprintf "Waiting for data_service: %s\n%!" (Exn.to_string error);
    let%bind () = Clock_ns.after (Time_ns.Span.of_sec 1.) in
    connect data_service_host_and_port
;;

let load_preview_template path =
  let open Or_error.Let_syntax in
  let%bind contents = Or_error.try_with (fun () -> In_channel.read_all path) in
  Or_error.try_with (fun () -> Mustache.of_string contents)
;;

let render_preview template render ~autoreload_script =
  let refresh_seconds = Time_ns.Span.to_sec render.Render.time_until_refresh in
  let template_data =
    `O
      [ "text", `String render.text
      ; "refresh_seconds", `String (Float.to_string_hum ~decimals:3 refresh_seconds)
      ; ( "refresh_milliseconds"
        , `String
            (Float.to_string_hum ~decimals:0 (Float.max 0. refresh_seconds *. 1_000.)) )
      ; "display_width_cm", `String (Float.to_string_hum render.display_size.width_cm)
      ; "display_height_cm", `String (Float.to_string_hum render.display_size.height_cm)
      ; "debug_info", `String render.debug_info
      ; "autoreload_script", `String autoreload_script
      ]
  in
  Or_error.try_with (fun () -> Mustache.render template template_data)
;;

let respond response =
  let open Deferred.Let_syntax in
  let%map response = response in
  `Response response
;;

let handle_preview_request ~get_data ~renderers ~template ~autoreload request =
  let path = request |> Cohttp.Request.uri |> Uri.path in
  match String.chop_prefix path ~prefix:"/preview/" with
  | Some name ->
    (match Map.find renderers name with
     | Some render ->
       let open Deferred.Let_syntax in
       let%bind rendered = render ~get_data in
       (match
          render_preview
            template
            rendered
            ~autoreload_script:(Autoreload_on_restart.script autoreload)
        with
        | Ok html ->
          Cohttp_async.Server.respond_string
            ~headers:
              (Cohttp.Header.of_list
                 [ "content-type", "text/html; charset=utf-8"
                 ; "cache-control", "no-store"
                 ])
            html
          |> respond
        | Error error ->
          Cohttp_async.Server.respond_string
            ~status:`Internal_server_error
            (Error.to_string_hum error)
          |> respond)
     | None ->
       Cohttp_async.Server.respond_string
         ~status:`Not_found
         [%string "No renderer named %{name}"]
       |> respond)
  | None -> Cohttp_async.Server.respond_string ~status:`Not_found "Not found" |> respond
;;

let run ~data_service_host_and_port ~preview_port ~preview_template =
  match load_preview_template preview_template with
  | Error error -> return (Error error)
  | Ok template ->
    let open Deferred.Let_syntax in
    let%bind connection = connect data_service_host_and_port in
    let get_data query =
      Rpc.Rpc.dispatch Data_service_rpc.Get_data.rpc connection query
    in
    let renderers = String.Map.singleton "home" Home.render in
    let autoreload = Autoreload_on_restart.create () in
    let%bind _server =
      Cohttp_async.Server.create_expert
        ~on_handler_error:`Raise
        (Tcp.Where_to_listen.of_port preview_port)
        (fun ~body:_ _ request ->
           match Autoreload_on_restart.handle_request autoreload request with
           | Some response -> response
           | None ->
             handle_preview_request ~get_data ~renderers ~template ~autoreload request)
    in
    Map.iter_keys renderers ~f:(fun name ->
      printf "http://127.0.0.1:%d/preview/%s\n%!" preview_port name);
    Deferred.never ()
;;
