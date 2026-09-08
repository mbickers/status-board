open! Core

module Tick : sig
  type t =
    { position_frac : float
    ; label : string option
    }
end

module Point : sig
  type t =
    { x_frac : float
    ; y_frac : float option
    }
end

module Style : sig
  type t =
    { font : Font.t
    ; label_size : float
    ; label_fill : Primitives.Fill.t
    ; label_halo : (int * Primitives.Fill.t) option
    ; stroke : Primitives.Stroke.t
    ; tick_length : int
    ; labeled_tick_length : int
    }
end

val create
  :  size:int * int
  -> style:Style.t
  -> x_ticks:Tick.t list
  -> y_ticks:Tick.t list
  -> points:Point.t list
  -> Element.t
