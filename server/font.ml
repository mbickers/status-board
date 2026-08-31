open! Core

module Rendered_text = struct
  type t =
    { buffer : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    ; width : int
    ; height : int
    ; origin_x : int
    ; baseline_y : int
    }
end

type t =
  { _buffer : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
  ; font : Stb_truetype.t
  }

let create ~ttf_file =
  let%bind.Or_error contents =
    Or_error.try_with (fun () -> In_channel.read_all ttf_file)
  in
  let buffer =
    Bigarray.Array1.create
      Bigarray.int8_unsigned
      Bigarray.c_layout
      (String.length contents)
  in
  String.iteri contents ~f:(fun index byte ->
    Bigarray.Array1.set buffer index (Char.to_int byte));
  let%bind.Or_error offset =
    match List.hd (Stb_truetype.enum buffer) with
    | Some offset -> Ok offset
    | None -> Or_error.error_string "Font file contains no fonts"
  in
  match Stb_truetype.init buffer offset with
  | Some font -> Ok { _buffer = buffer; font }
  | None -> Or_error.error_string "Failed to initialize font"
;;

let render_text t string ~size =
  let scale = Stb_truetype.scale_for_pixel_height t.font size in
  let _, _, glyphs =
    string
    |> String.to_list
    |> List.map ~f:(fun character -> Stb_truetype.get t.font (Char.to_int character))
    |> List.fold ~init:(None, 0, []) ~f:(fun (previous_glyph, cursor, glyphs) glyph ->
      let cursor =
        cursor
        + Option.value_map previous_glyph ~default:0 ~f:(fun previous_glyph ->
          Stb_truetype.kern_advance t.font previous_glyph glyph)
      in
      ( Some glyph
      , cursor + Stb_truetype.glyph_advance t.font glyph
      , ( Float.of_int cursor *. scale |> Int.of_float
        , Stb_truetype.get_glyph_bitmap t.font glyph ~scale_x:scale ~scale_y:scale )
        :: glyphs ))
  in
  let glyphs = List.rev glyphs in
  let left, top, right, bottom =
    List.fold
      glyphs
      ~init:(Int.max_value, Int.max_value, Int.min_value, Int.min_value)
      ~f:(fun (left, top, right, bottom) (x, bitmap) ->
        ( Int.min left (x + bitmap.Stb_truetype.xoff)
        , Int.min top bitmap.yoff
        , Int.max right (x + bitmap.xoff + bitmap.w)
        , Int.max bottom (bitmap.yoff + bitmap.h) ))
  in
  let width = right - left
  and height = bottom - top in
  let buffer =
    Bigarray.Array1.create Bigarray.int8_unsigned Bigarray.c_layout (width * height)
  in
  Bigarray.Array1.fill buffer 0;
  List.iter glyphs ~f:(fun (glyph_x, bitmap) ->
    for y = 0 to bitmap.h - 1 do
      for x = 0 to bitmap.w - 1 do
        Bigarray.Array1.set
          buffer
          (((y + bitmap.yoff - top) * width) + glyph_x + bitmap.xoff + x - left)
          (Bigarray.Array1.get bitmap.buf ((y * bitmap.w) + x))
      done
    done);
  { Rendered_text.buffer; width; height; origin_x = -left; baseline_y = -top }
;;
