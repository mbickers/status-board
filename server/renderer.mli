open! Core
open! Async

val url_query_string : Status_board.Input.t -> string

val respond
  :  path:[ `Exact of string ]
  -> cache:Feeds.Cache.t
  -> message_manager:Message_manager.t
  -> status_board:Status_board.t
  -> Http.Handler.t
