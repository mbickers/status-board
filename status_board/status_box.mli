open! Core

module Style : sig
  type t

  val create
    :  font:Drawing.Font.t
    -> base_padding:int
    -> primary_font_size:float
    -> label_size:float
    -> error_fill:Drawing.Primitives.Fill.t
    -> t

  val font : t -> Drawing.Font.t
  val base_padding : t -> int
  val horizontal_padding_between_text : t -> int
  val baseline_padding : t -> int
  val primary_font_size : t -> float
  val label_size : t -> float
  val error_fill : t -> Drawing.Primitives.Fill.t
end

val create
  :  ?fill:Drawing.Primitives.Fill.t
  -> style:Style.t
  -> label:string
  -> content:Drawing.Element.t
  -> unit
  -> Drawing.Element.t
