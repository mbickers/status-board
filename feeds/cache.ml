open! Core
open! Async

module Key = struct
  type 'a t =
    { filename : string
    ; sexpable : (module Sexpable.S with type t = 'a)
    ; id : 'a Latest_result.t Deferred.t Type_equal.Id.t
    }

  let create sexpable ~filename =
    { filename; sexpable; id = Type_equal.Id.create ~name:filename sexp_of_opaque }
  ;;
end

type t =
  { path : string
  ; mutable in_flight : Univ_map.t
  }

let create ~path = { path; in_flight = Univ_map.empty }
let path_for_key t ~key = Filename.concat t.path [%string "%{key}.sexp"]

let read t (type value) (key : value Key.t) =
  let module M = (val key.sexpable) in
  let path = path_for_key t ~key:key.filename in
  let%bind contents =
    Monitor.try_with ~extract_exn:true (fun () -> Reader.file_contents path)
  in
  match contents with
  | Error (Unix.Unix_error (Unix.Error.ENOENT, _, _)) -> return None
  | Error exn ->
    let error = Error.of_exn exn in
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
       [%log.global.error "Failed to parse cache file" (path : string) (error : Error.t)];
       return None)
;;

let write t (type value) (key : value Key.t) value =
  let module M = (val key.sexpable) in
  let path = path_for_key t ~key:key.filename in
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

let get t key ~max_age ~fetch =
  match Univ_map.find t.in_flight key.Key.id with
  | Some result -> result
  | None ->
    let result =
      Monitor.protect
        (fun () ->
           (* Register the deferred before starting the request. *)
           let%bind () = Scheduler.yield () in
           let%bind previous = read t key in
           let now = Time_ns.now () in
           match previous with
           | Some result when Time_ns.Span.(Time_ns.diff now result.at <= max_age) ->
             return result
           | previous ->
             let%bind fetched = Monitor.try_with_or_error fetch in
             let fetched = Or_error.join fetched in
             let value =
               match fetched with
               | Ok value -> Latest_result.Outcome.Success value
               | Error error ->
                 Error
                   { error
                   ; last_good =
                       Option.bind previous ~f:(fun result ->
                         Latest_result.latest_success result |> Result.ok)
                   }
             in
             let result = { Latest_result.Completed.value; at = Time_ns.now () } in
             let%bind () = write t key result in
             return result)
        ~finally:(fun () ->
          t.in_flight <- Univ_map.remove t.in_flight key.id;
          return ())
    in
    t.in_flight <- Univ_map.set t.in_flight ~key:key.id ~data:result;
    result
;;

let with_tmp_dir ~f =
  let%bind path = Unix.mkdtemp (Filename.concat Filename.temp_dir_name "cache_test") in
  Monitor.protect
    (fun () -> f path)
    ~finally:(fun () ->
      let%bind files = Sys.readdir path in
      let%bind () =
        Deferred.Array.iter ~how:`Sequential files ~f:(fun file ->
          Unix.unlink (Filename.concat path file))
      in
      Unix.rmdir path)
;;

let%expect_test "concurrent requests share results and failed requests can retry" =
  with_tmp_dir ~f:(fun path ->
    let cache = create ~path in
    let key = Key.create (module Int) ~filename:"number" in
    let other_key = Key.create (module String) ~filename:"text" in
    let calls = ref 0 in
    let%bind () =
      Deferred.List.iter
        ~how:`Sequential
        [ `Success; `Error; `Raise; `Success ]
        ~f:(fun outcome ->
          let started = Ivar.create () in
          let release = Ivar.create () in
          let fetch () =
            incr calls;
            Ivar.fill_if_empty started ();
            let%bind () = Ivar.read release in
            match outcome with
            | `Success -> return (Ok !calls)
            | `Error -> return (Or_error.error_string "failed")
            | `Raise -> failwith "failed"
          in
          let request () =
            get cache key ~max_age:Time_ns.Span.min_value_representable ~fetch
          in
          let first = request () in
          let second = request () in
          let%bind () = Ivar.read started in
          let third = request () in
          let%bind other =
            get cache other_key ~max_age:Time_ns.Span.zero ~fetch:(fun () ->
              return (Ok "text"))
          in
          assert (not (Deferred.is_determined first));
          assert (Result.is_ok (Latest_result.latest_success other));
          Ivar.fill_if_empty release ();
          let%bind results = Deferred.all [ first; second; third ] in
          print_s
            [%sexp
              (!calls : int)
            , (List.map results ~f:(fun result ->
                 Latest_result.latest_success result
                 |> Or_error.map ~f:(fun completed -> completed.value))
               : int Or_error.t list)];
          return ())
    in
    [%expect
      {|
         (1 ((Ok 1) (Ok 1) (Ok 1)))
         (2 ((Ok 1) (Ok 1) (Ok 1)))
         (3 ((Ok 1) (Ok 1) (Ok 1)))
         (4 ((Ok 4) (Ok 4) (Ok 4)))
         |}];
    let%bind cached =
      get cache key ~max_age:(Time_ns.Span.of_hr 1.) ~fetch:(fun () ->
        incr calls;
        return (Ok 5))
    in
    print_s
      [%sexp
        (!calls : int)
      , (Latest_result.latest_success cached
         |> Or_error.map ~f:(fun completed -> completed.value)
         : int Or_error.t)];
    [%expect {| (4 (Ok 4)) |}];
    return ())
;;
