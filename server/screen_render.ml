open! Core

module Size = struct
  type t =
    { width : int
    ; height : int
    }
end

type t =
  { buffer : Cairo.Surface.t
  ; time_until_refresh : Time_ns.Span.t
  ; display_resolution : Size.t
  ; debug_info : string
  }
