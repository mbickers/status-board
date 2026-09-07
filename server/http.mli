open! Core
open! Async

val request_path : Cohttp.Request.t -> string
val request_origin : Cohttp.Request.t -> string Or_error.t

val string_response
  :  ?headers:Cohttp.Header.t
  -> ?status:Cohttp.Code.status_code
  -> string
  -> Cohttp_async.Server.response_action Deferred.t

module Handler : sig
  type t =
    body:Cohttp_async.Body.t
    -> Cohttp.Request.t
    -> Cohttp_async.Server.response_action Deferred.t

  module Route : sig
    type handler := t
    type t

    val create
      :  ([< `Exact of string | `Prefix of string ] as 'path)
      -> f:(path:'path -> handler)
      -> t

    val respond : t list -> handler
  end
end

val respond_file : path:[ `Exact of string ] -> content_type:string -> string -> Handler.t
