open! Core

type t =
  { buffer : Image.image
  ; time_until_refresh : Time_ns.Span.t
  ; debug_info : string
  }
