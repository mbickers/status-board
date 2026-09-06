open! Core

module Completed = struct
  type 'a t =
    { value : 'a
    ; at : Time_ns.Alternate_sexp.t
    }
  [@@deriving sexp]
end

module Outcome = struct
  type 'a t =
    | Success of 'a
    | Error of
        { error : Error.t
        ; last_good : 'a Completed.t option
        }
  [@@deriving sexp]
end

type 'a t = 'a Outcome.t Completed.t [@@deriving sexp]

let map t ~f =
  let value =
    match t.Completed.value with
    | Outcome.Success value -> Outcome.Success (f value)
    | Error { error; last_good } ->
      let last_good =
        Option.map last_good ~f:(fun t -> { Completed.value = f t.value; at = t.at })
      in
      Error { error; last_good }
  in
  { Completed.value; at = t.at }
;;

let latest_success t =
  match t.Completed.value with
  | Outcome.Success value -> Ok { Completed.value; at = t.at }
  | Error { last_good = Some last_good; _ } -> Ok last_good
  | Error { error; last_good = None } -> Error error
;;
