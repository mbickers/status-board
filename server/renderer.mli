open! Core
open! Async

type t

val create
  :  path:string
  -> cache:Feeds.Cache.t
  -> message_manager:Message_manager.t
  -> status_board:Status_board.t
  -> t

val image_path : t -> Status_board.Input.t -> string
val respond : t -> path:[ `Exact of string ] -> Http.Handler.t
