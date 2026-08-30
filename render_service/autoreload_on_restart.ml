open! Core
open! Async

let endpoint = "/_preview/wait-for-restart"

type t = { instance_id : string }

let create () =
  { instance_id = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string }
;;

let handle_request t request =
  let uri = Cohttp.Request.uri request in
  if not (String.equal (Uri.path uri) endpoint)
  then None
  else (
    match Uri.get_query_param uri "instance-id" with
    | Some instance_id when String.equal instance_id t.instance_id ->
      let response =
        Cohttp.Response.make
          ~headers:(Cohttp.Header.init_with "cache-control" "no-store")
          ~status:`OK
          ()
      in
      Some
        (return (`Expert (response, fun _reader writer -> Writer.close_finished writer)))
    | Some _ ->
      Some
        (let%map response =
           Cohttp_async.Server.respond_string
             ~headers:(Cohttp.Header.init_with "cache-control" "no-store")
             ""
         in
         `Response response)
    | None ->
      Some
        (let%map response =
           Cohttp_async.Server.respond_string ~status:`Bad_request "Missing instance-id"
         in
         `Response response))
;;

let script t =
  [%string
    {|
(async function autoreloadOnRestart() {
  const endpoint = "%{endpoint}?instance-id=%{t.instance_id}";
  while (true) {
    try {
      const response = await fetch(endpoint, { cache: "no-store" });
      await response.text();
      if (response.ok) {
        window.location.reload();
        return;
      }
    } catch (_) {}
    await new Promise(resolve => setTimeout(resolve, 25));
  }
})();|}]
;;
