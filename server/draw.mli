open! Core

val status_box
  :  Drawing.Context.t
  -> int * int
  -> int * int
  -> font:Font.t
  -> title:string
  -> f:(Drawing.Context.t -> unit)
  -> unit
