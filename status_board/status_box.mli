open! Core

module Style : sig
  type t

  val create
    :  font:Graphics.Font.t
    -> base_padding:int
    -> primary_font_size:float
    -> error_fill:Graphics.Drawing.Fill.t
    -> t

  val font : t -> Graphics.Font.t
  val base_padding : t -> int
  val horizontal_padding_between_text : t -> int
  val baseline_padding : t -> int
  val primary_font_size : t -> float
  val error_fill : t -> Graphics.Drawing.Fill.t
end

val draw
  :  ?fill:(Graphics.Drawing.Context.t -> Graphics.Drawing.Fill.t)
  -> Graphics.Drawing.Context.t
  -> int * int
  -> int * int
  -> style:Style.t
  -> title:string
  -> f:(Graphics.Drawing.Context.t -> fill:Graphics.Drawing.Fill.t -> unit)
  -> unit
