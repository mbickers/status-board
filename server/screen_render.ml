open! Core

module Display_size = struct
  type t =
    { width_cm : float
    ; height_cm : float
    }
end

type t =
  { text : string
  ; time_until_refresh : Time_ns.Span.t
  ; display_size : Display_size.t
  ; debug_info : string
  }
