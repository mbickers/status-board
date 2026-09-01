open! Core
open! Async

module Publish_record = struct
  type t =
    { image_url : string
    ; filename : string
    }
end

type t =
  { images : Image.image String.Table.t
  ; mutable generation : int
  }

let create () = { images = String.Table.create (); generation = 0 }

let publish t ~name ~buffer =
  Hashtbl.set t.images ~key:name ~data:buffer;
  t.generation <- t.generation + 1;
  { Publish_record.image_url = [%string "/image/%{name}"]
  ; filename = [%string "%{name}-%{t.generation#Int}.png"]
  }
;;

let respond t ~name =
  match Hashtbl.find t.images name with
  | Some image ->
    let image_png = Buffer.create 0 in
    (match
       Or_error.try_with (fun () ->
         ImagePNG.write_png (ImageUtil.chunk_writer_of_buffer image_png) image)
     with
     | Ok () ->
       Http.respond_string
         ~headers:
           (Cohttp.Header.of_list
              [ "content-type", "image/png"; "cache-control", "no-store" ])
         (Buffer.contents image_png)
     | Error error ->
       Http.respond_string ~status:`Internal_server_error (Error.to_string_hum error))
  | None -> Http.respond_string ~status:`Not_found [%string "No image named %{name}"]
;;
