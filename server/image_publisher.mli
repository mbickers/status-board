open! Core
open! Async

module Publish_record : sig
  type t =
    { image_url : string
    ; filename : string
    }
end

type t

val create : unit -> t
val publish : t -> name:string -> buffer:Image.image -> Publish_record.t
val respond : t -> name:string -> Cohttp_async.Server.response_action Deferred.t
