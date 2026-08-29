open! Core
open! Async

let () =
  don't_wait_for (Render_service.run ());
  never_returns (Scheduler.go ())
;;
