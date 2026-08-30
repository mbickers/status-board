open! Core

module Size : sig
  type t =
    { width : int
    ; height : int
    }
end

type t =
  { buffer : Image.image
  ; time_until_refresh : Time_ns.Span.t
  ; display_resolution : Size.t
  ; debug_info : string
  }
