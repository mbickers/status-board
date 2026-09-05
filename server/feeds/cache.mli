open! Core
open! Async

type t

val create : path:string -> t

val get
  :  t
  -> (module Sexpable.S with type t = 'a)
  -> max_age:Time_ns.Span.t
  -> fetch:(unit -> 'a Deferred.Or_error.t)
  -> key:string
  -> 'a Latest_result.t Deferred.t
