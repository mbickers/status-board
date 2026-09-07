open! Core
open! Async

let request_path request =
  match request |> Cohttp.Request.uri |> Uri.path with
  | "" | "/" -> "/"
  | path -> String.chop_suffix_if_exists path ~suffix:"/"
;;

let request_origin request =
  let headers = Cohttp.Request.headers request in
  let scheme =
    Cohttp.Header.get headers "x-forwarded-proto" |> Option.value ~default:"http"
  in
  match
    Cohttp.Header.get headers "x-forwarded-host", Cohttp.Header.get headers "host"
  with
  | Some host, _ | None, Some host -> Ok [%string "%{scheme}://%{host}"]
  | None, None -> Or_error.error_string "Request has no Host header"
;;

let string_response ?headers ?status body =
  let%bind response = Cohttp_async.Server.respond_string ?headers ?status body in
  return (`Response response)
;;

module Handler = struct
  type t =
    body:Cohttp_async.Body.t
    -> Cohttp.Request.t
    -> Cohttp_async.Server.response_action Deferred.t

  module Route = struct
    type handler = t
    type t = [ `Exact of string | `Prefix of string ] * handler

    let create path ~f =
      let respond = f ~path in
      let path =
        match path with
        | `Exact path -> `Exact path
        | `Prefix path -> `Prefix path
      in
      path, respond
    ;;

    let respond routes ~body request =
      let path = request_path request in
      match
        List.find routes ~f:(fun (route_path, _) ->
          match route_path with
          | `Exact exact -> String.equal path exact
          | `Prefix prefix ->
            String.equal path prefix || String.is_prefix path ~prefix:(prefix ^ "/"))
      with
      | Some (_, respond) -> respond ~body request
      | None -> string_response ~status:`Not_found "Not found"
    ;;
  end
end

let respond_file ~path:(`Exact _) ~content_type filename ~body:_ request =
  match Cohttp.Request.meth request with
  | `GET ->
    let%bind contents =
      Monitor.try_with_or_error (fun () -> Reader.file_contents filename)
    in
    (match contents with
     | Ok contents ->
       string_response
         ~headers:(Cohttp.Header.init_with "content-type" content_type)
         contents
     | Error error ->
       string_response ~status:`Internal_server_error (Error.to_string_hum error))
  | _ -> string_response ~status:`Not_found "Not found"
;;
