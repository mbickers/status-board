open! Core
open! Async

type t =
  { text : string
  ; written_at : Time_ns.Alternate_sexp.t
  }
[@@deriving sexp]

let read ~filename =
  let%bind contents =
    Monitor.try_with ~extract_exn:true (fun () -> Reader.file_contents filename)
  in
  match contents with
  | Error (Unix.Unix_error (Unix.Error.ENOENT, _, _)) -> return (Ok [])
  | Error exn -> return (Error (Error.of_exn exn))
  | Ok contents ->
    return
      (Or_error.try_with (fun () ->
         Sexp.of_string_many contents |> List.rev_map ~f:t_of_sexp))
;;

let latest ~filename =
  let%bind.Deferred.Or_error messages = read ~filename in
  return (Ok (List.hd messages |> Option.map ~f:(fun message -> message.text)))
;;

let page_html ~messages ~attempted ~error =
  let%bind.Or_error contents =
    Or_error.try_with (fun () -> In_channel.read_all "server/message.html")
  in
  let%bind.Or_error template =
    Or_error.try_with (fun () -> Mustache.of_string contents)
  in
  let%bind.Or_error zone =
    Or_error.try_with (fun () -> Time_ns_unix.Zone.find_exn "America/New_York")
  in
  let messages =
    List.map messages ~f:(fun message ->
      let written_at =
        Time_ns_unix.format message.written_at "%d %b %Y at %I:%M %p" ~zone
      in
      `O [ "text", `String message.text; "written_at", `String written_at ])
  in
  Or_error.try_with (fun () ->
    Mustache.render
      template
      (`O
          [ "attempted", `String attempted
          ; ( "error"
            , match error with
              | None -> `Null
              | Some error -> `String (Error.to_string_hum error) )
          ; "messages", `A messages
          ]))
;;

let respond ~filename ~validate ~body request =
  let page ~status ~messages ~attempted ~error =
    match page_html ~messages ~attempted ~error with
    | Error error ->
      Http.respond_string ~status:`Internal_server_error (Error.to_string_hum error)
    | Ok html ->
      Http.respond_string
        ~status
        ~headers:
          (Cohttp.Header.of_list
             [ "content-type", "text/html; charset=utf-8"; "cache-control", "no-store" ])
        html
  in
  let%bind messages = read ~filename in
  match messages with
  | Error error ->
    Http.respond_string ~status:`Internal_server_error (Error.to_string_hum error)
  | Ok messages ->
    (match Cohttp.Request.meth request with
     | `GET -> page ~status:`OK ~messages ~attempted:"" ~error:None
     | `POST ->
       let%bind contents = Cohttp_async.Body.to_string body in
       let attempted =
         List.Assoc.find (Uri.query_of_encoded contents) ~equal:String.equal "message"
         |> Option.bind ~f:List.hd
         |> Option.value ~default:""
       in
       let validation =
         match String.length attempted > 100 with
         | true -> Or_error.error_string "Messages must be 100 characters or fewer"
         | false -> validate attempted
       in
       (match validation with
        | Error error ->
          page ~status:`Bad_request ~messages ~attempted ~error:(Some error)
        | Ok () ->
          let message = { text = attempted; written_at = Time_ns.now () } in
          let written =
            Or_error.try_with (fun () ->
              Out_channel.with_file filename ~append:true ~f:(fun output ->
                Out_channel.output_string
                  output
                  (Sexp.to_string_mach (sexp_of_t message) ^ "\n")))
          in
          (match written with
           | Error error ->
             Http.respond_string
               ~status:`Internal_server_error
               (Error.to_string_hum error)
           | Ok () ->
             Http.respond_string
               ~status:`See_other
               ~headers:
                 (Cohttp.Header.init_with
                    "location"
                    (Uri.path (Cohttp.Request.uri request)))
               ""))
     | _ ->
       Http.respond_string
         ~status:`Method_not_allowed
         ~headers:(Cohttp.Header.init_with "allow" "GET, POST")
         "Method not allowed")
;;
