open! Core
open! Async

module type Sexpable = sig
  type t

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

type t

val create : path:string -> t

val get
  :  t
  -> (module Sexpable with type t = 'a)
  -> max_age:Time_ns.Span.t
  -> fetch:(unit -> 'a Deferred.Or_error.t)
  -> key:string
  -> 'a Latest_result.t Deferred.t
