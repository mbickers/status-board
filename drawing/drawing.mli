open! Core
module Bitmap = Bitmap
module Context = Context
module Element = Element
module Font = Font
module Graph = Graph
module Path_resolver = Path_resolver
module Primitives = Primitives

module O : sig
  module Context = Context
  module Element = Element
  module Graph = Graph
  module Path_resolver = Path_resolver
  include module type of Primitives
  include module type of Primitives.Fill with type t := Primitives.Fill.t
end
