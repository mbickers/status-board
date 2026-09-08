open! Core

module Line : sig
  type t =
    | L
    | M
    | J_and_Z_but_call_it_J

  val fill : t -> Drawing.Primitives.Fill.t
end

val stroke
  :  casing_fill:Drawing.Primitives.Fill.t
  -> Drawing.Primitives.Fill.t
  -> Drawing.Primitives.Stroke.t

val stroke_safe_padding : int

module Status : sig
  module Row : sig
    type t =
      { line : Line.t
      ; westbound_minutes : int list
      ; eastbound_minutes : int list
      }
  end

  type t =
    { rows : Row.t list
    ; has_alert : bool
    }

  module Selection : sig
    type t =
      { line : Line.t
      ; minimum_minutes : int
      ; westbound_mta_direction : Feeds.Mta_subway.Direction.t
      }
  end

  val create
    :  Feeds.Mta_subway.Status.t
    -> now:Time_ns.t
    -> station_id:string
    -> rows:Selection.t list
    -> t Or_error.t

  module Testing_data : sig
    val dense_text : widest_two_digit_number:int -> lines:Line.t list -> t
    val errors : lines:Line.t list -> t
  end

  val element : style:Status_box.Style.t -> label:string -> t -> Drawing.Element.t
end
