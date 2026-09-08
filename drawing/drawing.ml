open! Core
module Bitmap = Bitmap
module Context = Context
module Element = Element
module Font = Font
module Graph = Graph
module Path_resolver = Path_resolver
module Primitives = Primitives

module O = struct
  module Context = Context
  module Element = Element
  module Graph = Graph
  module Path_resolver = Path_resolver
  include Primitives
  include Fill
end
