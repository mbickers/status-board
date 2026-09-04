open! Core

module Completed : sig
  type 'a t =
    { value : 'a
    ; at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp]
end

module Outcome : sig
  type 'a t =
    | Success of 'a
    | Error of
        { error : Error.t
        ; last_good : 'a Completed.t option
        }
  [@@deriving sexp]
end

type 'a t = 'a Outcome.t Completed.t [@@deriving sexp]

val map : 'a t -> f:('a -> 'b) -> 'b t
val latest_success : 'a t -> 'a Completed.t Or_error.t
