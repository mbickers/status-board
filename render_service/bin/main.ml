open! Core
open! Async

let command =
  Command.async
    ~summary:"Run the render service"
    (let%map_open.Command data_service_host_and_port =
       flag
         "-data-service-host-and-port"
         (required host_and_port)
         ~doc:"HOST:PORT data service address"
     in
     fun () -> Render_service.run ~data_service_host_and_port)
;;

let () = Command_unix.run command
