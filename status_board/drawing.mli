open! Core

module Context : sig
  type t

  val create : Bitmap.t -> t
  val crop : t -> size:int * int -> offset:int * int -> t
  val size : t -> int * int
  val write : t -> int * int -> [ `b | `w ] -> unit
end

module Element : sig
  module Horizontal_alignment : sig
    type t =
      | Left
      | Center
      | Right
  end

  module Vertical_alignment : sig
    type t =
      | Top
      | Center
      | Bottom
      | Baseline
  end

  module Anchor : sig
    type t = (Horizontal_alignment.t * int) * (Vertical_alignment.t * int)
  end

  type t

  val create
    :  ?baseline:int
    -> size:int * int
    -> draw:(Context.t -> upper_left:int * int -> unit)
    -> unit
    -> t

  (* Elements may draw beyond their reported size (e.g. text halos). Size represents visual bounds for layout, not strict bounds on where we draw. *)
  val size : t -> int * int
  val draw : t -> Context.t -> Anchor.t -> unit
  val column : gap:int -> align:Horizontal_alignment.t -> t list -> t
end

module Fill : sig
  type t = int * int -> [ `b | `w ]

  val solid : [ `b | `w ] -> t
  val invert : t -> t
  val bayer_exn : ?size:int -> ?offset:int * int -> white_frac:float -> t
  val fade_to : t -> color:[ `b | `w ] -> color_frac:(int * int -> float) -> t
  val fractional : frac:float -> frontier_angle_degrees:float -> size:int * int -> t
end

module Stroke : sig
  type t

  val create : ?casing:t -> Fill.t -> int -> t
  val solid : ?casing:t -> [ `b | `w ] -> int -> t
  val safe_padding : t -> int
end

module Path_resolver_step : sig
  type t =
    | Point of int * int
    | Offset of int * int

  val resolve : t list -> (int * int) list
end

module Shapes : sig
  val rect : Context.t -> fill:Fill.t -> int * int -> int * int -> unit
  val polygon : Context.t -> fill:Fill.t -> (int * int) list -> unit
  val circle : Context.t -> fill:Fill.t -> center:int * int -> radius:int -> unit
  val star : Context.t -> stroke:Stroke.t -> radius:int -> center:int * int -> unit
  val draw_line : Context.t -> stroke:Stroke.t -> float * float -> float * float -> unit

  val draw_quadratic_curve
    :  Context.t
    -> stroke:Stroke.t
    -> (float * float) * (float * float) * (float * float)
    -> unit

  val rounded_path
    :  Context.t
    -> radius:int
    -> stroke:Stroke.t
    -> (int * int) list
    -> unit

  val rounded_polygon
    :  Context.t
    -> radius:int
    -> fill:Fill.t
    -> ?stroke:Stroke.t
    -> ?round_corner:(int -> bool)
    -> (int * int) list
    -> unit
end

module Text : sig
  val create
    :  ?halo:int * Fill.t
    -> font:Font.t
    -> fill:Fill.t
    -> size:float
    -> string
    -> Element.t
end

module Graph : sig
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
      ; label_fill : Fill.t
      ; label_halo : (int * Fill.t) option
      ; stroke : Stroke.t
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
end

module O : sig
  module Graph = Graph
  module Context = Context
  module Path_resolver_step = Path_resolver_step
  module Stroke = Stroke
  module Text = Text
  module Element = Element
  include module type of Fill with type t := Fill.t
  include module type of Shapes
end
