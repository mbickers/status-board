open! Core
open! Async

val respond
  :  path:[ `Exact of string ]
  -> autoreload_script:string option
  -> image_path:(string option -> string)
  -> status_board:Status_board.t
  -> Http.Handler.t
