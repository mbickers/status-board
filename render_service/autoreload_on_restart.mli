open! Core
open! Async

type t

val create : unit -> t

(** Returns [Some response] for the autoreload endpoint and [None] for every
    other request. A request from this server instance is held until the
    process exits. A request carrying an older instance ID completes
    immediately. *)
val handle_request
  :  t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t option

(** Javascript which reloads the page as soon as a replacement server accepts
    requests. *)
val script : t -> string
