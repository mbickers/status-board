open! Core
open! Async

let start =
  Command.async_or_error
    ~summary:"Run the status board server"
    (let%map_open.Command cache_path =
       flag "-cache-path" (required string) ~doc:"PATH cache directory"
     and port = flag "-port" (required int) ~doc:"PORT HTTP port"
     and autoreload =
       flag "-autoreload" no_arg ~doc:" Reload previews when the server restarts"
     in
     fun () -> Server.run ~cache_path ~port ~autoreload)
;;

let render =
  Command.async_or_error
    ~summary:"Render a status board image to a BMP file"
    (let%map_open.Command cache_path =
       flag "-cache-path" (required string) ~doc:"PATH cache directory"
     and preset = flag "-preset" (optional string) ~doc:"NAME preview preset"
     and filename = anon ("FILENAME" %: string) in
     fun () -> Server.render ~cache_path ~preset ~filename)
;;

let command = Command.group ~summary:"Status board" [ "start", start; "render", render ]
let () = Command_unix.run command
