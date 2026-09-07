open! Core
open! Async

type t =
  { instance_id : string
  ; monitor_path : string
  }

let create ~monitor_path =
  { instance_id = Time_ns.now () |> Time_ns.to_int63_ns_since_epoch |> Int63.to_string
  ; monitor_path
  }
;;

let respond t ~path:(`Exact _) ~body:_ request =
  match Cohttp.Request.meth request with
  | `GET ->
    let uri = Cohttp.Request.uri request in
    (match Uri.get_query_param uri "instance-id" with
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
       Http.string_response
         ~headers:(Cohttp.Header.init_with "cache-control" "no-store")
         ""
     | None -> Http.string_response ~status:`Bad_request "Missing instance-id")
  | _ -> Http.string_response ~status:`Not_found "Not found"
;;

let script t =
  [%string
    {|
(async function autoreloadOnRestart() {
  const endpoint = "%{t.monitor_path}?instance-id=%{t.instance_id}";
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
