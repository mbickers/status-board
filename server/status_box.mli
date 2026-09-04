open! Core

module Style : sig
  type t

  val create
    :  font:Font.t
    -> horizontal_padding:int
    -> horizontal_padding_between_text:int
    -> baseline_padding:int
    -> primary_font_size:float
    -> t

  val font : t -> Font.t
  val horizontal_padding : t -> int
  val horizontal_padding_between_text : t -> int
  val baseline_padding : t -> int
  val primary_font_size : t -> float
end

val draw
  :  ?fill:(Drawing.Context.t -> Drawing.Fill.t)
  -> Drawing.Context.t
  -> int * int
  -> int * int
  -> style:Style.t
  -> title:string
  -> f:(Drawing.Context.t -> fill:Drawing.Fill.t -> unit)
  -> unit
