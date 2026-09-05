open! Core

module Row : sig
  type 'display_route t =
    { display_route : 'display_route
    ; westbound_minutes : int list
    ; eastbound_minutes : int list
    }
end

type 'display_route t =
  { title : string
  ; rows : 'display_route Row.t list
  }

module Selection : sig
  type 'display_route t =
    { display_route : 'display_route
    ; route_ids : string list
    ; minimum_minutes : int
    ; westbound_mta_direction : Feeds.Mta_subway.Direction.t
    }
end

val create
  :  Feeds.Mta_subway.Status.t
  -> now:Time_ns.t
  -> station_id:string
  -> title:string
  -> rows:'display_route Selection.t list
  -> 'display_route t Or_error.t

val width : Status_box.Style.t -> int
val height : Status_box.Style.t -> 'display_route t -> int

val draw
  :  Graphics.Drawing.Context.t
  -> anchor:Graphics.Drawing.Anchor.t
  -> style:Status_box.Style.t
  -> display_route_text:('display_route -> string)
  -> route_fill:('display_route -> Graphics.Drawing.Fill.t)
  -> 'display_route t
  -> unit
