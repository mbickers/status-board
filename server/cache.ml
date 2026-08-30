open! Core
open! Async

module type Sexpable = sig
  type t

  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

type t = { path : string }

let create ~path = { path }
let path_for_key t ~key = Filename.concat t.path [%string "%{key}.sexp"]

let read t (type value) (module M : Sexpable with type t = value) ~key =
  let path = path_for_key t ~key in
  let%bind exists = Sys.file_exists path in
  match exists with
  | `No -> return None
  | `Unknown ->
    [%log.global.error "Failed to check cache file" (path : string)];
    return None
  | `Yes ->
    let%bind contents = Monitor.try_with_or_error (fun () -> Reader.file_contents path) in
    (match contents with
     | Error error ->
       [%log.global.error "Failed to read cache file" (path : string) (error : Error.t)];
       return None
     | Ok contents ->
       let parsed =
         let%bind.Or_error sexp = Or_error.try_with (fun () -> Sexp.of_string contents) in
         Or_error.try_with (fun () -> Latest_result.t_of_sexp M.t_of_sexp sexp)
       in
       (match parsed with
        | Ok value -> return (Some value)
        | Error error ->
          [%log.global.error
            "Failed to parse cache file" (path : string) (error : Error.t)];
          return None))
;;

let write t (type value) (module M : Sexpable with type t = value) ~key value =
  let path = path_for_key t ~key in
  let contents = Latest_result.sexp_of_t M.sexp_of_t value |> Sexp.to_string_hum in
  let%bind directory = Monitor.try_with_or_error (fun () -> Unix.mkdir ~p:() t.path) in
  match directory with
  | Error error ->
    [%log.global.error
      "Failed to create cache directory" (t.path : string) (error : Error.t)];
    return ()
  | Ok () ->
    let%bind written =
      Monitor.try_with_or_error (fun () ->
        Writer.with_file path ~f:(fun writer ->
          Writer.write writer contents;
          return ()))
    in
    return
      (match written with
       | Ok () -> ()
       | Error error ->
         [%log.global.error
           "Failed to write cache file" (path : string) (error : Error.t)])
;;

let last_good =
  Option.bind ~f:(fun result -> Latest_result.latest_success result |> Result.ok)
;;

let get t m ~max_age ~fetch ~key =
  let%bind previous = read t m ~key in
  let now = Time_ns.now () in
  match previous with
  | Some result when Time_ns.Span.(Time_ns.diff now result.at <= max_age) -> return result
  | previous ->
    let%bind fetched = fetch () in
    let value =
      match fetched with
      | Ok value -> Latest_result.Outcome.Success value
      | Error error -> Error { error; last_good = last_good previous }
    in
    let result = { Latest_result.Completed.value; at = Time_ns.now () } in
    let%bind () = write t m ~key result in
    return result
;;
