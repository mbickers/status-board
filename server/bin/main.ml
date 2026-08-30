open! Core
open! Async

let command =
  Command.async_or_error
    ~summary:"Run the status board server"
    (let%map_open.Command cache_path =
       flag "-cache-path" (required string) ~doc:"PATH cache directory"
     and port = flag "-port" (required int) ~doc:"PORT HTTP port"
     and preview_template_path =
       flag
         "-preview-template-path"
         (required string)
         ~doc:"PATH mustache template for the preview page"
     in
     fun () -> Server.run ~cache_path ~port ~preview_template_path)
;;

let () = Command_unix.run command
