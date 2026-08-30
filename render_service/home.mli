open! Core
open! Async

val render
  :  get_data:
       (Data_service_rpc.Get_data.Query.t
        -> Data_service_rpc.Get_data.Response.t Deferred.Or_error.t)
  -> Render.t Deferred.t
