let rec utf_8_scalar_width_aux count s pos len =
  if len <= 0 then
    count
  else
    let c = String.unsafe_get s pos in
    let c_len =
      match Stdcompat__uchar.utf_8_decode_length_of_byte c with
      | 0 -> 1
      | c_len -> c_len in
    utf_8_scalar_width_aux (succ count) s (pos + c_len) (len - c_len)

let utf_8_scalar_width s ~pos ~len =
  if pos < 0 || len < 0 || String.length s < pos + len then
    invalid_arg "utf_8_scalar_width: invalid bounds";
  utf_8_scalar_width_aux 0 s pos len
