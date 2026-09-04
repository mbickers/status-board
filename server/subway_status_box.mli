open! Core

module Row : sig
  type t =
    { bullet : string * Drawing.Fill.t
    ; route_ids : string list
    ; minimum_minutes : int
    ; westbound_mta_direction : string
    }
end

val width : Status_box.Style.t -> int
val height : Status_box.Style.t -> row_count:int -> int

val draw
  :  Drawing.Context.t
  -> anchor:Drawing.Anchor.t
  -> style:Status_box.Style.t
  -> title:string
  -> now:Time_ns.t
  -> stop_status:Mta_subway.Stop_status.t
  -> rows:Row.t list
  -> unit
