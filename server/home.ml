open! Core
open! Async

let render cache =
  let%map stations = Citibike.query cache in
  { Screen_render.text = "hello world"
  ; time_until_refresh = Time_ns.Span.of_sec 30.
  ; display_size = { Screen_render.Display_size.width_cm = 16.3; height_cm = 9.8 }
  ; debug_info =
      stations
      |> Latest_result.map ~f:(fun stations ->
        Map.find stations "66dc8768-0aca-11e7-82f6-3863bb44ef7c")
      |> Latest_result.sexp_of_t (Option.sexp_of_t Citibike.Station.sexp_of_t)
      |> Sexp.to_string_hum
  }
;;
