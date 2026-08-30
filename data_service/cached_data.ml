open! Core
open! Async
module Latest_result = Data_service_rpc.Latest_result
module Completed = Latest_result.Completed

type 'a state =
  | Idle of 'a Latest_result.t option
  | Fetching of 'a Latest_result.t Deferred.t

type 'a t =
  { max_age : Time_ns.Span.t
  ; fetch : unit -> 'a Deferred.Or_error.t
  ; mutable state : 'a state
  }

let create ~max_age ~fetch = { max_age; fetch; state = Idle None }

let get t =
  let now = Time_ns.now () in
  match t.state with
  | Idle (Some result) when Time_ns.Span.(Time_ns.diff now result.at <= t.max_age) ->
    return result
  | Fetching result -> result
  | Idle previous ->
    let open Deferred.Let_syntax in
    let result =
      let%map fetched = t.fetch () in
      let value =
        match fetched with
        | Ok value -> Latest_result.Success value
        | Error error ->
          let last_good =
            match previous with
            | None -> None
            | Some { value = Latest_result.Success value; at } ->
              let last_good : _ Completed.t = { value; at } in
              Some last_good
            | Some { value = Latest_result.Error { last_good; _ }; _ } -> last_good
          in
          Latest_result.Error { error; last_good }
      in
      let result : _ Latest_result.t = { value; at = Time_ns.now () } in
      result
    in
    t.state <- Fetching result;
    upon result (fun result -> t.state <- Idle (Some result));
    result
;;
