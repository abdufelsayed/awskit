let uri_encode ?(encode_slash = true) value =
  let buffer = Buffer.create (String.length value) in
  let add_encoded char =
    Buffer.add_string buffer (Fmt.str "%%%02X" (Char.code char))
  in
  String.iter
    (function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '~' | '.') as char
        ->
          Buffer.add_char buffer char
      | '/' when not encode_slash -> Buffer.add_char buffer '/'
      | char -> add_encoded char)
    value;
  Buffer.contents buffer

let expand_query params =
  List.concat_map
    (fun (key, values) ->
      match values with
      | [] -> [ (key, "") ]
      | values -> List.map (fun value -> (key, value)) values)
    params

let canonical_query params =
  params
  |> expand_query
  |> List.map (fun (key, value) -> (uri_encode key, uri_encode value))
  |> List.sort (fun (left_key, left_value) (right_key, right_value) ->
      match String.compare left_key right_key with
      | 0 -> String.compare left_value right_value
      | order -> order)
  |> List.map (fun (key, value) -> key ^ "=" ^ value)
  |> String.concat "&"

let xml_escape value =
  let buffer = Buffer.create (String.length value) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | '"' -> Buffer.add_string buffer "&quot;"
      | '\'' -> Buffer.add_string buffer "&apos;"
      | char -> Buffer.add_char buffer char)
    value;
  Buffer.contents buffer

let tagging_xml tags =
  tags
  |> List.map (fun (key, value) ->
      Fmt.str "<Tag><Key>%s</Key><Value>%s</Value></Tag>" (xml_escape key)
        (xml_escape value))
  |> String.concat ""
  |> Fmt.str "<Tagging><TagSet>%s</TagSet></Tagging>"

let content_range_header (start, finish, complete_length) =
  let length =
    match complete_length with
    | None -> "*"
    | Some length -> Int64.to_string length
  in
  Fmt.str "bytes %Ld-%Ld/%s" start finish length

let content_range_matches (start, finish, complete_length)
    (value : Awskit_s3.Range.Content_range.t) =
  Int64.equal start value.start
  && Int64.equal finish value.finish
  && Option.equal Int64.equal complete_length value.complete_length
