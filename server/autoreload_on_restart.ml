open! Core
open! Async

type t =
  { instance_id : string
  ; monitor_path : string list
  }

let create ~monitor_path =
  { instance_id = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string
  ; monitor_path
  }
;;

let respond t request =
  let uri = Cohttp.Request.uri request in
  match Uri.get_query_param uri "instance-id" with
  | Some instance_id when String.equal instance_id t.instance_id ->
    (* Hold the response open while this server instance is alive. When server is restarted or killed, clients retry connection. When they connect to a new server instance, server tells them to reload because [instance_id] is different. *)
    let response =
      Cohttp.Response.make
        ~headers:(Cohttp.Header.init_with "cache-control" "no-store")
        ~status:`OK
        ()
    in
    return (`Expert (response, fun _reader writer -> Writer.close_finished writer))
  | Some _ ->
    Http.respond_string ~headers:(Cohttp.Header.init_with "cache-control" "no-store") ""
  | None -> Http.respond_string ~status:`Bad_request "Missing instance-id"
;;

let script t =
  let endpoint_path = "/" ^ String.concat t.monitor_path ~sep:"/" in
  [%string
    {|
(async function autoreloadOnRestart() {
  const endpoint = "%{endpoint_path}?instance-id=%{t.instance_id}";
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
