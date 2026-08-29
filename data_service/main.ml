open! Core
open! Async

let () =
  don't_wait_for (Data_service.run ());
  never_returns (Scheduler.go ())
;;
