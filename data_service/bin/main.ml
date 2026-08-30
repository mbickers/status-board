open! Core
open! Async

let command =
  Command.async
    ~summary:"Run the data service"
    (let%map_open.Command port = flag "-port" (required int) ~doc:"PORT RPC port" in
     fun () -> Data_service.run ~port)
;;

let () = Command_unix.run command
