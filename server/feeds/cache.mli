open! Core
open! Async

module Key : sig
  type 'a t

  val create : (module Sexpable.S with type t = 'a) -> filename:string -> 'a t
end

type t

val create : path:string -> t

val get
  :  t
  -> 'a Key.t
  -> max_age:Time_ns.Span.t
  -> fetch:(unit -> 'a Deferred.Or_error.t)
  -> 'a Latest_result.t Deferred.t
