open! Core
open! Async

let command =
  Command.async_or_error
    ~summary:"Run the render service"
    (let%map_open.Command data_service_host_and_port =
       flag
         "-data-service-host-and-port"
         (required host_and_port)
         ~doc:"HOST:PORT data service address"
     and preview_port = flag "-preview-port" (required int) ~doc:"PORT preview HTTP port"
     and preview_template =
       flag "-preview-template" (required string) ~doc:"PATH preview HTML template"
     in
     fun () ->
       Render_service.run ~data_service_host_and_port ~preview_port ~preview_template)
;;

let () = Command_unix.run command
