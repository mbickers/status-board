open! Core
open! Async

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

let respond_string ?headers ?status body =
  let%bind response = Cohttp_async.Server.respond_string ?headers ?status body in
  return (`Response response)
;;
