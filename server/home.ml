open! Core
open! Async

let rect buffer ~color (x1, y1) (x2, y2) =
  for y = y1 to y2 - 1 do
    for x = x1 to x2 - 1 do
      Image.write_grey buffer x y color
    done
  done
;;

let text buffer ~font ~origin_x ~baseline_y ~size string =
  let rendered_text = Font.render_text font string ~size in
  for y = 0 to rendered_text.height - 1 do
    for x = 0 to rendered_text.width - 1 do
      if Bigarray.Array1.get rendered_text.buffer ((y * rendered_text.width) + x) >= 128
      then
        Image.write_grey
          buffer
          (origin_x - rendered_text.origin_x + x)
          (baseline_y - rendered_text.baseline_y + y)
          0
    done
  done
;;

let render cache =
  let%bind stations = Citibike.query cache in
  let w = 800
  and h = 480 in
  return
    (let%bind.Or_error font_contents =
       Or_error.try_with (fun () -> In_channel.read_all "server/fonts/inter_medium.ttf")
     in
     let%map.Or_error font = Font.create font_contents in
     let buffer = Image.create_grey ~max_val:1 w h in
     rect buffer ~color:1 (0, 0) (w, h);
     text buffer ~font ~origin_x:222 ~baseline_y:277 ~size:80. "hello world";
     { Screen_render.buffer
     ; time_until_refresh = Time_ns.Span.of_sec 30.
     ; debug_info =
         stations
         |> Latest_result.map ~f:(fun stations ->
           Map.find stations "66dc8768-0aca-11e7-82f6-3863bb44ef7c")
         |> Latest_result.sexp_of_t (Option.sexp_of_t Citibike.Station.sexp_of_t)
         |> Sexp.to_string_hum
     })
;;
