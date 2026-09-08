open! Core

type t =
  { bitmap : Bitmap.t
  ; offset : int * int
  ; size : int * int
  }

let create bitmap = { bitmap; offset = 0, 0; size = Bitmap.size bitmap }

let crop t ~size ~offset =
  let x, y = t.offset
  and offset_x, offset_y = offset in
  { t with offset = x + offset_x, y + offset_y; size }
;;

let size t = t.size

let write t (x, y) color =
  let width, height = t.size in
  match x >= 0 && y >= 0 && x < width && y < height with
  | true ->
    let offset_x, offset_y = t.offset
    and bitmap = t.bitmap in
    let x = x + offset_x
    and y = y + offset_y in
    let bitmap_width, bitmap_height = Bitmap.size bitmap in
    (match x >= 0 && y >= 0 && x < bitmap_width && y < bitmap_height with
     | true -> Bitmap.write_exn bitmap ~x ~y color
     | false -> ())
  | false -> ()
;;
