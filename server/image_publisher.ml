open! Core
open! Async

module Publish_record = struct
  type t = { image_url : string }
end

type t = Cairo.Surface.t String.Table.t

let create () = String.Table.create ()

let publish t ~name ~buffer =
  Hashtbl.set t ~key:name ~data:buffer;
  { Publish_record.image_url = [%string "/image/%{name}"] }
;;

let respond t ~name =
  match Hashtbl.find t name with
  | Some image ->
    let image_png = Buffer.create 48_000 in
    (match
       Or_error.try_with (fun () ->
         (* Cairo encodes RGB24 surfaces as 8-bit RGB PNGs. This may need to become
            a 1-bit encoder if the bit depth causes TRMNL to use full refreshes. *)
         Cairo.PNG.write_to_stream image (Buffer.add_string image_png))
     with
     | Ok () ->
       let%bind response =
         Cohttp_async.Server.respond_string
           ~headers:
             (Cohttp.Header.of_list
                [ "content-type", "image/png"; "cache-control", "no-store" ])
           (Buffer.contents image_png)
       in
       return (`Response response)
     | Error error ->
       let%bind response =
         Cohttp_async.Server.respond_string
           ~status:`Internal_server_error
           (Error.to_string_hum error)
       in
       return (`Response response))
  | None ->
    let%bind response =
      Cohttp_async.Server.respond_string
        ~status:`Not_found
        [%string "No image named %{name}"]
    in
    return (`Response response)
;;
