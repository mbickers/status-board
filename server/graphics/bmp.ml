open! Core

let set_little_endian bytes ~offset ~byte_count value =
  for i = 0 to byte_count - 1 do
    Bytes.set bytes (offset + i) (Char.of_int_exn ((value lsr (i * 8)) land 0xff))
  done
;;

let encode image =
  let row_size = (image.Image.width + 31) / 32 * 4 in
  let pixel_data_size = row_size * image.height in
  let header_size = 62 in
  let result = Bytes.make (header_size + pixel_data_size) '\000' in
  Bytes.set result 0 'B';
  Bytes.set result 1 'M';
  set_little_endian result ~offset:2 ~byte_count:4 (Bytes.length result);
  set_little_endian result ~offset:10 ~byte_count:4 header_size;
  set_little_endian result ~offset:14 ~byte_count:4 40;
  set_little_endian result ~offset:18 ~byte_count:4 image.width;
  set_little_endian result ~offset:22 ~byte_count:4 image.height;
  set_little_endian result ~offset:26 ~byte_count:2 1;
  set_little_endian result ~offset:28 ~byte_count:2 1;
  set_little_endian result ~offset:34 ~byte_count:4 pixel_data_size;
  set_little_endian result ~offset:46 ~byte_count:4 2;
  set_little_endian result ~offset:50 ~byte_count:4 2;
  Bytes.set result 58 '\255';
  Bytes.set result 59 '\255';
  Bytes.set result 60 '\255';
  for output_y = 0 to image.height - 1 do
    let image_y = image.height - output_y - 1 in
    for x = 0 to image.width - 1 do
      Image.read_grey image x image_y (fun grey ->
        match grey <> 0 with
        | true ->
          let byte_index = header_size + (output_y * row_size) + (x / 8) in
          Bytes.set
            result
            byte_index
            (Char.to_int (Bytes.get result byte_index) lor (0x80 lsr (x mod 8))
             |> Char.of_int_exn)
        | false -> ())
    done
  done;
  Bytes.to_string result
;;
