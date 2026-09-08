open! Core

val render_message
  :  fill:Drawing.Primitives.Fill.t
  -> string
  -> Drawing.Element.t Or_error.t

val status_board : Status_board.t
