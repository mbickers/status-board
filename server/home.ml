open! Core
open! Async

let render cache =
  let%bind stations = Citibike.query cache in
  let display_resolution = { Screen_render.Size.width = 800; height = 480 } in
  let buffer =
    Or_error.try_with (fun () ->
      let buffer =
        Cairo.Image.create
          Cairo.Image.RGB24
          ~w:display_resolution.width
          ~h:display_resolution.height
      in
      let context = Cairo.create buffer in
      Cairo.set_source_rgb context 1. 1. 1.;
      Cairo.paint context;
      Cairo.Font_face.set
        context
        (Cairo.Ft.create_for_ft_face (Cairo.Ft.face "server/fonts/inter_medium.ttf"));
      Cairo.set_font_size context 80.;
      let text = "hello world" in
      let text_extents = Cairo.text_extents context text in
      Cairo.move_to
        context
        (((Float.of_int display_resolution.width -. text_extents.width) /. 2.)
         -. text_extents.x_bearing)
        (((Float.of_int display_resolution.height -. text_extents.height) /. 2.)
         -. text_extents.y_bearing);
      Cairo.set_source_rgb context 0. 0. 0.;
      Cairo.show_text context text;
      buffer)
  in
  return
    (let%map.Or_error buffer = buffer in
     { Screen_render.buffer
     ; time_until_refresh = Time_ns.Span.of_sec 30.
     ; display_resolution
     ; debug_info =
         stations
         |> Latest_result.map ~f:(fun stations ->
           Map.find stations "66dc8768-0aca-11e7-82f6-3863bb44ef7c")
         |> Latest_result.sexp_of_t (Option.sexp_of_t Citibike.Station.sexp_of_t)
         |> Sexp.to_string_hum
     })
;;
