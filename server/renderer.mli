open! Core
open! Async

val url_query_string : Status_board.Input.t -> string

val respond
  :  cache:Feeds.Cache.t
  -> status_board:Status_board.t
  -> Cohttp.Request.t
  -> Cohttp_async.Server.response_action Deferred.t
