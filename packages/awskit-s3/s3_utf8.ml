type analysis = { bytes : int; scalar_count : int; utf16_units : int }

let invalid ~field message =
  Error (Awskit.Error.Internal.validation ~field message)

let is_continuation byte = byte land 0xC0 = 0x80
let byte value index = Char.code value.[index]

let continuation value index =
  index < String.length value && is_continuation (byte value index)

let decode_2 b0 b1 = ((b0 land 0x1F) lsl 6) lor (b1 land 0x3F)

let decode_3 b0 b1 b2 =
  ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)

let decode_4 b0 b1 b2 b3 =
  ((b0 land 0x07) lsl 18)
  lor ((b1 land 0x3F) lsl 12)
  lor ((b2 land 0x3F) lsl 6)
  lor (b3 land 0x3F)

let utf16_units_of_codepoint codepoint = if codepoint > 0xFFFF then 2 else 1

let analyze value =
  let len = String.length value in
  let rec loop index scalar_count utf16_units =
    if index = len then Some { bytes = len; scalar_count; utf16_units }
    else
      let b0 = byte value index in
      if b0 <= 0x7F then loop (index + 1) (scalar_count + 1) (utf16_units + 1)
      else if b0 >= 0xC2 && b0 <= 0xDF then
        if continuation value (index + 1) then
          let codepoint = decode_2 b0 (byte value (index + 1)) in
          loop (index + 2) (scalar_count + 1)
            (utf16_units + utf16_units_of_codepoint codepoint)
        else None
      else if b0 = 0xE0 then
        if
          index + 2 < len
          && byte value (index + 1) >= 0xA0
          && byte value (index + 1) <= 0xBF
          && continuation value (index + 2)
        then
          let codepoint =
            decode_3 b0 (byte value (index + 1)) (byte value (index + 2))
          in
          loop (index + 3) (scalar_count + 1)
            (utf16_units + utf16_units_of_codepoint codepoint)
        else None
      else if (b0 >= 0xE1 && b0 <= 0xEC) || b0 = 0xEE || b0 = 0xEF then
        if continuation value (index + 1) && continuation value (index + 2) then
          let codepoint =
            decode_3 b0 (byte value (index + 1)) (byte value (index + 2))
          in
          loop (index + 3) (scalar_count + 1)
            (utf16_units + utf16_units_of_codepoint codepoint)
        else None
      else if b0 = 0xED then
        if
          index + 2 < len
          && byte value (index + 1) >= 0x80
          && byte value (index + 1) <= 0x9F
          && continuation value (index + 2)
        then
          let codepoint =
            decode_3 b0 (byte value (index + 1)) (byte value (index + 2))
          in
          loop (index + 3) (scalar_count + 1)
            (utf16_units + utf16_units_of_codepoint codepoint)
        else None
      else if b0 = 0xF0 then
        if
          index + 3 < len
          && byte value (index + 1) >= 0x90
          && byte value (index + 1) <= 0xBF
          && continuation value (index + 2)
          && continuation value (index + 3)
        then
          let codepoint =
            decode_4 b0
              (byte value (index + 1))
              (byte value (index + 2))
              (byte value (index + 3))
          in
          loop (index + 4) (scalar_count + 1)
            (utf16_units + utf16_units_of_codepoint codepoint)
        else None
      else if b0 >= 0xF1 && b0 <= 0xF3 then
        if
          continuation value (index + 1)
          && continuation value (index + 2)
          && continuation value (index + 3)
        then
          let codepoint =
            decode_4 b0
              (byte value (index + 1))
              (byte value (index + 2))
              (byte value (index + 3))
          in
          loop (index + 4) (scalar_count + 1)
            (utf16_units + utf16_units_of_codepoint codepoint)
        else None
      else if b0 = 0xF4 then
        if
          index + 3 < len
          && byte value (index + 1) >= 0x80
          && byte value (index + 1) <= 0x8F
          && continuation value (index + 2)
          && continuation value (index + 3)
        then
          let codepoint =
            decode_4 b0
              (byte value (index + 1))
              (byte value (index + 2))
              (byte value (index + 3))
          in
          loop (index + 4) (scalar_count + 1)
            (utf16_units + utf16_units_of_codepoint codepoint)
        else None
      else None
  in
  loop 0 0 0

let for_all_scalars value ~f =
  let len = String.length value in
  let rec loop index =
    if index = len then true
    else
      let b0 = byte value index in
      if b0 <= 0x7F then f (Uchar.of_int b0) && loop (index + 1)
      else if b0 >= 0xC2 && b0 <= 0xDF && continuation value (index + 1) then
        let codepoint = decode_2 b0 (byte value (index + 1)) in
        f (Uchar.of_int codepoint) && loop (index + 2)
      else if
        b0 = 0xE0
        && index + 2 < len
        && byte value (index + 1) >= 0xA0
        && byte value (index + 1) <= 0xBF
        && continuation value (index + 2)
      then
        let codepoint =
          decode_3 b0 (byte value (index + 1)) (byte value (index + 2))
        in
        f (Uchar.of_int codepoint) && loop (index + 3)
      else if
        ((b0 >= 0xE1 && b0 <= 0xEC) || b0 = 0xEE || b0 = 0xEF)
        && continuation value (index + 1)
        && continuation value (index + 2)
      then
        let codepoint =
          decode_3 b0 (byte value (index + 1)) (byte value (index + 2))
        in
        f (Uchar.of_int codepoint) && loop (index + 3)
      else if
        b0 = 0xED
        && index + 2 < len
        && byte value (index + 1) >= 0x80
        && byte value (index + 1) <= 0x9F
        && continuation value (index + 2)
      then
        let codepoint =
          decode_3 b0 (byte value (index + 1)) (byte value (index + 2))
        in
        f (Uchar.of_int codepoint) && loop (index + 3)
      else if
        b0 = 0xF0
        && index + 3 < len
        && byte value (index + 1) >= 0x90
        && byte value (index + 1) <= 0xBF
        && continuation value (index + 2)
        && continuation value (index + 3)
      then
        let codepoint =
          decode_4 b0
            (byte value (index + 1))
            (byte value (index + 2))
            (byte value (index + 3))
        in
        f (Uchar.of_int codepoint) && loop (index + 4)
      else if
        b0 >= 0xF1
        && b0 <= 0xF3
        && continuation value (index + 1)
        && continuation value (index + 2)
        && continuation value (index + 3)
      then
        let codepoint =
          decode_4 b0
            (byte value (index + 1))
            (byte value (index + 2))
            (byte value (index + 3))
        in
        f (Uchar.of_int codepoint) && loop (index + 4)
      else if
        b0 = 0xF4
        && index + 3 < len
        && byte value (index + 1) >= 0x80
        && byte value (index + 1) <= 0x8F
        && continuation value (index + 2)
        && continuation value (index + 3)
      then
        let codepoint =
          decode_4 b0
            (byte value (index + 1))
            (byte value (index + 2))
            (byte value (index + 3))
        in
        f (Uchar.of_int codepoint) && loop (index + 4)
      else false
  in
  loop 0

let validate ?max_bytes ?max_scalars ?max_utf16_units ?(allow_empty = false)
    ~field ~name value =
  if (not allow_empty) && value = "" then
    invalid ~field (Printf.sprintf "%s must be non-empty" name)
  else
    match analyze value with
    | None -> invalid ~field (Printf.sprintf "%s must be valid UTF-8" name)
    | Some analysis -> (
        match max_bytes with
        | Some max_bytes when analysis.bytes > max_bytes ->
            invalid ~field
              (Printf.sprintf "%s must be at most %d UTF-8 bytes" name max_bytes)
        | _ -> (
            match max_scalars with
            | Some max_scalars when analysis.scalar_count > max_scalars ->
                invalid ~field
                  (Printf.sprintf "%s must be at most %d Unicode characters"
                     name max_scalars)
            | _ -> (
                match max_utf16_units with
                | Some max_utf16_units
                  when analysis.utf16_units > max_utf16_units ->
                    invalid ~field
                      (Printf.sprintf "%s must be at most %d UTF-16 positions"
                         name max_utf16_units)
                | _ -> Ok value)))
