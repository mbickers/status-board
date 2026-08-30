open! Core
open! Async

let command =
  Command.async
    ~summary:"Run the render service"
    (let%map_open.Command host =
       flag "-host" (required string) ~doc:"HOST data service host"
     and port = flag "-port" (required int) ~doc:"PORT data service port" in
     fun () -> Render_service.run ~host ~port)
;;

let () = Command_unix.run command
