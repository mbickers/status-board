open! Core

module Completed = struct
  type 'a t =
    { value : 'a
    ; at : Time_ns.Alternate_sexp.t
    }
  [@@deriving bin_io, sexp]
end

type 'a outcome =
  | Success of 'a
  | Error of
      { error : Error.t
      ; last_good : 'a Completed.t option
      }
[@@deriving bin_io, sexp]

type 'a t = 'a outcome Completed.t [@@deriving bin_io, sexp]

let map (t : _ t) ~f =
  let value =
    match t.value with
    | Success value -> Success (f value)
    | Error { error; last_good } ->
      let last_good =
        Option.map last_good ~f:(fun (t : _ Completed.t) ->
          let mapped : _ Completed.t = { value = f t.value; at = t.at } in
          mapped)
      in
      Error { error; last_good }
  in
  let mapped : _ t = { value; at = t.at } in
  mapped
;;
