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

let raw_sorted_query_pairs params =
  expand_query params
  |> List.sort (fun (left_key, left_value) (right_key, right_value) ->
      match String.compare left_key right_key with
      | 0 -> String.compare left_value right_value
      | order -> order)

let encoded_sorted_query_pairs params =
  expand_query params
  |> List.sort (fun (left_key, left_value) (right_key, right_value) ->
      match String.compare (uri_encode left_key) (uri_encode right_key) with
      | 0 -> String.compare (uri_encode left_value) (uri_encode right_value)
      | order -> order)

let query_sort_changes_after_encoding params =
  raw_sorted_query_pairs params <> encoded_sorted_query_pairs params

let normalize_header_value value =
  let pieces = ref [] in
  let current = Buffer.create (String.length value) in
  let flush_piece () =
    if Buffer.length current > 0 then (
      pieces := Buffer.contents current :: !pieces;
      Buffer.clear current)
  in
  String.iter
    (function
      | ' ' | '\t' | '\r' | '\n' -> flush_piece ()
      | char -> Buffer.add_char current char)
    value;
  flush_piece ();
  String.concat " " (List.rev !pieces)

let add_header_group groups name value =
  let rec loop = function
    | [] -> [ (name, [ value ]) ]
    | (key, values) :: rest when String.equal key name ->
        (key, values @ [ value ]) :: rest
    | group :: rest -> group :: loop rest
  in
  loop groups

let canonical_headers headers =
  headers
  |> List.fold_left
       (fun groups (name, value) ->
         add_header_group groups
           (String.lowercase_ascii name)
           (normalize_header_value value))
       []
  |> List.sort (fun (left_name, _) (right_name, _) ->
      String.compare left_name right_name)
  |> List.map (fun (name, values) -> (name, String.concat "," values))

let signed_header_names headers =
  canonical_headers headers |> List.map fst |> String.concat ";"

let canonical_headers_block headers =
  canonical_headers headers
  |> List.map (fun (name, value) -> name ^ ":" ^ value ^ "\n")
  |> String.concat ""

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
