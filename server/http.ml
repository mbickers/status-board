open! Core
open! Async

let get_body url =
  let%bind.Deferred.Or_error uri =
    Or_error.try_with (fun () -> Uri.of_string url) |> return
  in
  let%bind.Deferred.Or_error response, body =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Client.get uri)
  in
  let%bind.Deferred.Or_error contents =
    Deferred.Or_error.try_with (fun () -> Cohttp_async.Body.to_string body)
  in
  let status_code = response |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
  if Cohttp.Code.is_success status_code
  then return (Ok contents)
  else Deferred.Or_error.errorf "GET %s failed with HTTP %d: %s" url status_code contents
;;

let respond_string ?headers ?status body =
  let%bind response = Cohttp_async.Server.respond_string ?headers ?status body in
  return (`Response response)
;;
