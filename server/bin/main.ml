open! Core
open! Async

let command =
  Command.async_or_error
    ~summary:"Run the status board server"
    (let%map_open.Command cache_path =
       flag "-cache-path" (required string) ~doc:"PATH cache directory"
     and port = flag "-port" (required int) ~doc:"PORT HTTP port" in
     fun () -> Server.run ~cache_path ~port)
;;

let () = Command_unix.run command
