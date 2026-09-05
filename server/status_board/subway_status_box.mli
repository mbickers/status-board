open! Core

module Row : sig
  type t =
    { bullet : string * Graphics.Drawing.Fill.t
    ; route_ids : string list
    ; minimum_minutes : int
    ; westbound_mta_direction : string
    }
end

val width : Status_box.Style.t -> int
val height : Status_box.Style.t -> row_count:int -> int

val draw
  :  Graphics.Drawing.Context.t
  -> anchor:Graphics.Drawing.Anchor.t
  -> style:Status_box.Style.t
  -> title:string
  -> now:Time_ns.t
  -> stop_status:Feeds.Mta_subway.Stop_status.t
  -> rows:Row.t list
  -> unit
