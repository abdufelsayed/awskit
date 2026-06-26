let prefix = "x-amz-meta-"

let hex_value = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'A' .. 'F' as c -> Some (10 + Char.code c - Char.code 'A')
  | 'a' .. 'f' as c -> Some (10 + Char.code c - Char.code 'a')
  | _ -> None

let find_char value start char =
  try Some (String.index_from value start char) with Not_found -> None

let find_encoded_word_end value start =
  let len = String.length value in
  let rec loop index =
    if index + 1 >= len then None
    else if Char.equal value.[index] '?' && Char.equal value.[index + 1] '='
    then Some index
    else loop (index + 1)
  in
  loop start

let decode_q_encoded value =
  let len = String.length value in
  let buffer = Buffer.create len in
  let rec loop index =
    if index = len then Some (Buffer.contents buffer)
    else
      match value.[index] with
      | '_' ->
          Buffer.add_char buffer ' ';
          loop (index + 1)
      | '=' when index + 2 < len -> (
          match (hex_value value.[index + 1], hex_value value.[index + 2]) with
          | Some high, Some low ->
              Buffer.add_char buffer (Char.chr ((high lsl 4) lor low));
              loop (index + 3)
          | _ -> None)
      | '=' -> None
      | c ->
          Buffer.add_char buffer c;
          loop (index + 1)
  in
  loop 0

let supported_charset value =
  match String.lowercase_ascii value with
  | "utf-8" | "utf8" -> true
  | _ -> false

let decode_encoded_word value index =
  if not (S3_string.substring_equal value index "=?") then None
  else
    match find_char value (index + 2) '?' with
    | None -> None
    | Some charset_end -> (
        match find_char value (charset_end + 1) '?' with
        | None -> None
        | Some encoding_end -> (
            match find_encoded_word_end value (encoding_end + 1) with
            | None -> None
            | Some word_end ->
                let charset =
                  String.sub value (index + 2) (charset_end - index - 2)
                in
                let encoding =
                  String.sub value (charset_end + 1)
                    (encoding_end - charset_end - 1)
                in
                let encoded =
                  String.sub value (encoding_end + 1)
                    (word_end - encoding_end - 1)
                in
                if not (supported_charset charset) then None
                else
                  let decoded =
                    match String.uppercase_ascii encoding with
                    | "B" -> Result.to_option (Base64.decode encoded)
                    | "Q" -> decode_q_encoded encoded
                    | _ -> None
                  in
                  Option.map (fun decoded -> (decoded, word_end + 2)) decoded))

let skip_spaces_before_encoded_word value index =
  let len = String.length value in
  let rec loop cursor =
    if cursor < len && Char.equal value.[cursor] ' ' then loop (cursor + 1)
    else cursor
  in
  let cursor = loop index in
  if cursor > index && S3_string.substring_equal value cursor "=?" then cursor
  else index

let decode_rfc2047_value value =
  let len = String.length value in
  let buffer = Buffer.create len in
  let rec loop decoded_any index =
    if index >= len then if decoded_any then Buffer.contents buffer else value
    else
      match decode_encoded_word value index with
      | Some (decoded, next) ->
          Buffer.add_string buffer decoded;
          loop true (skip_spaces_before_encoded_word value next)
      | None ->
          Buffer.add_char buffer value.[index];
          loop decoded_any (index + 1)
  in
  loop false 0

let of_headers headers =
  let metadata =
    List.filter_map
      (fun (key, value) ->
        let lower = String.lowercase_ascii key in
        if S3_string.is_prefix ~prefix lower then
          Some
            ( String.sub key (String.length prefix)
                (String.length key - String.length prefix),
              decode_rfc2047_value value )
        else None)
      headers
  in
  match Metadata.of_list metadata with
  | Ok _ as result -> result
  | Error error ->
      Error
        (S3_error_context.decode_with_context ~what:"S3 metadata headers"
           (Awskit.Error.to_string_hum error))

let to_headers metadata =
  List.map
    (fun (key, value) -> (prefix ^ key, value))
    (Metadata.to_list metadata)
