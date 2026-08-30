open! Core
open! Async

let render cache =
  let%bind stations = Citibike.query cache in
  let display_resolution = { Screen_render.Size.width = 800; height = 480 } in
  let image =
    Image.create_grey ~max_val:1 display_resolution.width display_resolution.height
  in
  for y = 0 to display_resolution.height - 1 do
    for x = 0 to display_resolution.width - 1 do
      Image.write_grey image x y ((x + y) % 2)
    done
  done;
  return
    { Screen_render.buffer = image
    ; time_until_refresh = Time_ns.Span.of_sec 30.
    ; display_resolution
    ; debug_info =
        stations
        |> Latest_result.map ~f:(fun stations ->
          Map.find stations "66dc8768-0aca-11e7-82f6-3863bb44ef7c")
        |> Latest_result.sexp_of_t (Option.sexp_of_t Citibike.Station.sexp_of_t)
        |> Sexp.to_string_hum
    }
;;
