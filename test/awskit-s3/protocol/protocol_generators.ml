type family =
  | Query
  | Duplicate_query
  | Duplicate_empty_query
  | Empty_absent_query
  | Percent_encoded_query
  | Encoded_sort_query
  | Endpoint_malformed
  | Endpoint_mutation
  | Header_canonical
  | Header_invalid_boundary
  | Header_newline
  | Invalid_content_range
  | Invalid_tag_field
  | Object_key
  | Oversized_tag_set
  | Percent_encoded_object_key
  | Tagging_xml_mutation

let family_bin = function
  | Query -> "protocol.family.query"
  | Duplicate_query -> "protocol.family.duplicate-query"
  | Duplicate_empty_query -> "protocol.family.duplicate-empty-query"
  | Empty_absent_query -> "protocol.family.empty-absent-query"
  | Percent_encoded_query -> "protocol.family.percent-encoded-query"
  | Encoded_sort_query -> "protocol.family.encoded-sort-query"
  | Endpoint_malformed -> "protocol.family.endpoint-malformed"
  | Endpoint_mutation -> "protocol.family.endpoint-mutation"
  | Header_canonical -> "protocol.family.header-canonical"
  | Header_invalid_boundary -> "protocol.family.header-invalid-boundary"
  | Header_newline -> "protocol.family.header-newline"
  | Invalid_content_range -> "protocol.family.invalid-content-range"
  | Invalid_tag_field -> "protocol.family.invalid-tag-field"
  | Object_key -> "protocol.family.object-key"
  | Oversized_tag_set -> "protocol.family.oversized-tag-set"
  | Percent_encoded_object_key -> "protocol.family.percent-encoded-object-key"
  | Tagging_xml_mutation -> "protocol.family.tagging-xml-mutation"

let chars_of_string value = List.init (String.length value) (String.get value)
let gen_from_chars chars = QCheck.Gen.oneof_list (chars_of_string chars)

let gen_string ~min ~max ~chars =
  let open QCheck.Gen in
  string_size ~gen:(gen_from_chars chars) (int_range min max)

let query_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -_.~"

let tag_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 +-_=.:/@"

let lower_digit_chars = "abcdefghijklmnopqrstuvwxyz0123456789"
let valid_bucket_name = gen_string ~min:3 ~max:40 ~chars:lower_digit_chars

let valid_dotted_bucket_name =
  let open QCheck.Gen in
  let label = gen_string ~min:1 ~max:12 ~chars:lower_digit_chars in
  let* first = label in
  let* rest = list_size (int_range 1 3) label in
  let labels =
    match List.rev rest with
    | [] -> [ first; "z" ]
    | last :: prefix -> first :: List.rev ((last ^ "z") :: prefix)
  in
  return (String.concat "." labels)

let object_key_segment_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ._-~+="

let object_key_segment =
  let open QCheck.Gen in
  let* first = gen_from_chars lower_digit_chars in
  let* rest = gen_string ~min:0 ~max:15 ~chars:object_key_segment_chars in
  return (String.make 1 first ^ rest)

let protocol_object_key =
  let open QCheck.Gen in
  oneof
    [
      return "soap";
      (let* first = object_key_segment in
       let* rest = list_size (int_range 0 5) object_key_segment in
       return (String.concat "/" (first :: rest)));
    ]

let percent_encoded_object_key =
  QCheck.Gen.oneof_list
    [
      "space%20key.txt";
      "%2Fleading-encoded-slash";
      "folder%2Fchild.txt";
      "plus+literal";
      "unicode-%CE%B4.txt";
      "bad-percent-%";
      "bad-hex-%GG";
      "literal-percent-%25";
    ]

let upload_id =
  gen_string ~min:1 ~max:64
    ~chars:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_./="

let valid_range =
  let open QCheck.Gen in
  oneof
    [
      (let* start = int_range 0 100_000 in
       let* length = int_range 0 10_000 in
       return (`Bytes (Int64.of_int start, Int64.of_int (start + length))));
      map (fun start -> `From (Int64.of_int start)) (int_range 0 100_000);
      map (fun length -> `Suffix (Int64.of_int length)) (int_range 1 100_000);
    ]

let invalid_presign_expires_seconds =
  QCheck.Gen.oneof_list [ -3600; -1; 0; 604_801; 604_802; 1_000_000 ]

let query_params =
  let open QCheck.Gen in
  let key = gen_string ~min:1 ~max:12 ~chars:query_chars in
  let value = gen_string ~min:0 ~max:12 ~chars:query_chars in
  list_size (int_range 0 12) (pair key (list_size (int_range 0 3) value))

let duplicate_query_params =
  let open QCheck.Gen in
  let key_gen = gen_string ~min:1 ~max:12 ~chars:query_chars in
  let value = gen_string ~min:0 ~max:12 ~chars:query_chars in
  let* key = key_gen in
  let* first_values = list_size (int_range 0 3) value in
  let* second_values = list_size (int_range 0 3) value in
  let* prefix =
    list_size (int_range 0 4) (pair key_gen (list_size (int_range 0 2) value))
  in
  let* suffix =
    list_size (int_range 0 4) (pair key_gen (list_size (int_range 0 2) value))
  in
  return (prefix @ [ (key, first_values); (key, second_values) ] @ suffix)

let duplicate_empty_query_params =
  let open QCheck.Gen in
  let key_gen = gen_string ~min:1 ~max:12 ~chars:query_chars in
  let value = gen_string ~min:0 ~max:12 ~chars:query_chars in
  let* key = key_gen in
  let* prefix =
    list_size (int_range 0 3) (pair key_gen (list_size (int_range 0 2) value))
  in
  let* suffix =
    list_size (int_range 0 3) (pair key_gen (list_size (int_range 0 2) value))
  in
  return (prefix @ [ (key, []); (key, []) ] @ suffix)

let empty_absent_query_case =
  let open QCheck.Gen in
  let key_gen = gen_string ~min:1 ~max:12 ~chars:query_chars in
  let value = gen_string ~min:0 ~max:12 ~chars:query_chars in
  let* key = key_gen in
  let* prefix =
    list_size (int_range 0 3) (pair key_gen (list_size (int_range 0 2) value))
  in
  let* suffix =
    list_size (int_range 0 3) (pair key_gen (list_size (int_range 0 2) value))
  in
  return (prefix, key, suffix)

let percent_triplet =
  QCheck.Gen.oneof_list
    [
      "%00";
      "%2B";
      "%2b";
      "%2F";
      "%2f";
      "%7E";
      "%7e";
      "%C3%A9";
      "%c3%a9";
      "%FF";
      "%ff";
    ]

let percent_encoded_component =
  let open QCheck.Gen in
  let plain = gen_string ~min:0 ~max:6 ~chars:query_chars in
  let* prefix = plain in
  let* encoded = percent_triplet in
  let* suffix = plain in
  return (prefix ^ encoded ^ suffix)

let percent_encoded_query_params =
  let open QCheck.Gen in
  let* first_key = percent_encoded_component in
  let* first_value = percent_encoded_component in
  let* second_key = percent_encoded_component in
  let* second_value = percent_encoded_component in
  return [ (first_key, [ first_value ]); (second_key, [ second_value ]) ]

let encoded_sort_query_params =
  let open QCheck.Gen in
  let prefix = gen_string ~min:0 ~max:6 ~chars:"abcdefghijklmnopqrstuvwxyz" in
  let value = gen_string ~min:0 ~max:6 ~chars:"ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
  let* key_prefix = prefix in
  let* value_prefix = value in
  let key_dot = key_prefix ^ "." in
  let key_slash = key_prefix ^ "/" in
  let value_dot = value_prefix ^ "." in
  let value_slash = value_prefix ^ "/" in
  return
    [
      (key_dot, [ "key-order" ]);
      (key_slash, [ "key-order" ]);
      ("value-order", [ value_dot; value_slash ]);
    ]

let valid_content_range =
  let open QCheck.Gen in
  let* start = int_range 0 100_000 in
  let* length = int_range 1 10_000 in
  let finish = start + length - 1 in
  let* complete_length =
    oneof_weighted
      [
        (1, return None);
        (3, map (fun extra -> Some (finish + extra)) (int_range 1 10_000));
      ]
  in
  return
    ( Int64.of_int start,
      Int64.of_int finish,
      Option.map Int64.of_int complete_length )

let invalid_content_range =
  let open QCheck.Gen in
  let* start = int_range 0 1000 in
  let* finish_delta = int_range 0 1000 in
  let finish = start + finish_delta in
  oneof
    [
      return (Fmt.str "items %d-%d/%d" start finish (finish + 1));
      return (Fmt.str "bytes x-%d/%d" finish (finish + 1));
      return (Fmt.str "bytes %d-x/%d" start (finish + 1));
      return (Fmt.str "bytes %d-%d/x" start finish);
      return (Fmt.str "bytes %d-%d/%d" (finish + 1) finish (finish + 2));
      return (Fmt.str "bytes %d-%d/%d" start finish finish);
      return
        (Fmt.str "bytes %d-%d/%d/%d" start finish (finish + 1) (finish + 2));
      return
        (Fmt.str "bytes %d-%d-%d/%d" start finish (finish + 1) (finish + 2));
    ]

let malformed_endpoint_authority =
  let open QCheck.Gen in
  let host = gen_string ~min:3 ~max:20 ~chars:"abcdefghijklmnopqrstuvwxyz" in
  let path = gen_string ~min:1 ~max:12 ~chars:"abcdefghijklmnopqrstuvwxyz" in
  let fragment =
    map2 (fun host path -> Fmt.str "https://%s#%s" host path) host path
  in
  let bad_port =
    map2
      (fun host port -> Fmt.str "https://%s:%s" host port)
      host
      (oneof_list [ "0"; "65536"; "-1"; "abc"; "" ])
  in
  let empty_host =
    oneof_list [ "https://"; "http://"; "https://:443"; "http://:80" ]
  in
  let malformed_ipv6 =
    oneof_list
      [
        "https://[::1"; "https://[::1]extra"; "https://::1"; "https://[::1]:abc";
      ]
  in
  let unsupported_scheme =
    map2
      (fun scheme host -> Fmt.str "%s://%s" scheme host)
      (oneof_list [ "ftp"; "s3"; "file"; "https+unix" ])
      host
  in
  let userinfo =
    map2 (fun host path -> Fmt.str "https://user:%s@%s" path host) host path
  in
  let with_path =
    map2 (fun host path -> Fmt.str "https://%s/%s" host path) host path
  in
  let with_query =
    map2 (fun host path -> Fmt.str "https://%s?%s=1" host path) host path
  in
  let control =
    map2
      (fun host char -> Fmt.str "https://%s%cexample.com" host char)
      host
      (oneof_list [ '\000'; '\n'; '\r'; '\t'; '\127' ])
  in
  oneof
    [
      fragment;
      bad_port;
      empty_host;
      malformed_ipv6;
      unsupported_scheme;
      userinfo;
      with_path;
      with_query;
      control;
    ]

let newline_header_value =
  let open QCheck.Gen in
  let piece =
    gen_string ~min:0 ~max:20
      ~chars:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  in
  map2 (fun left right -> left ^ "\n" ^ right) piece piece

let header_name_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-"

let header_value_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789._/-"

let safe_header_name =
  QCheck.Gen.map
    (fun suffix -> "x-amz-meta-" ^ suffix)
    (gen_string ~min:1 ~max:12 ~chars:"abcdefghijklmnopqrstuvwxyz0123456789")

let header_value_token = gen_string ~min:1 ~max:12 ~chars:header_value_chars

let whitespace =
  QCheck.Gen.oneof_list [ ""; " "; "  "; "\t"; "\t "; " \t"; " \t " ]

let canonical_header_value =
  let open QCheck.Gen in
  let* leading = whitespace in
  let* first = header_value_token in
  let* middle = oneof_list [ " "; "  "; "\t"; " \t "; "\t  " ] in
  let* second = header_value_token in
  let* trailing = whitespace in
  return (leading ^ first ^ middle ^ second ^ trailing)

let canonical_header_list =
  let open QCheck.Gen in
  let* repeated_name = safe_header_name in
  let* other_name = safe_header_name in
  let other_name =
    if String.equal repeated_name other_name then other_name ^ "-other"
    else other_name
  in
  let* first = canonical_header_value in
  let* second = canonical_header_value in
  let* other = canonical_header_value in
  return
    [
      (String.uppercase_ascii repeated_name, first);
      (other_name, other);
      (repeated_name, second);
      ("Host", " s3.us-east-1.amazonaws.com ");
    ]

type invalid_header_boundary =
  | Empty_header_name of string
  | Header_name_colon of string * string
  | Header_name_control of string * char * string
  | Header_value_newline of string * string * char * string

let invalid_header_boundary_headers = function
  | Empty_header_name value -> [ ("", value) ]
  | Header_name_colon (left, right) -> [ (left ^ ":" ^ right, "value") ]
  | Header_name_control (left, char, right) ->
      [ (left ^ String.make 1 char ^ right, "value") ]
  | Header_value_newline (name, left, char, right) ->
      [ (name, left ^ String.make 1 char ^ right) ]

let invalid_header_boundary =
  let open QCheck.Gen in
  let name_piece = gen_string ~min:1 ~max:8 ~chars:header_name_chars in
  let value_piece = gen_string ~min:0 ~max:12 ~chars:header_value_chars in
  oneof
    [
      map (fun value -> Empty_header_name value) value_piece;
      map2
        (fun left right -> Header_name_colon (left, right))
        name_piece name_piece;
      map3
        (fun left char right -> Header_name_control (left, char, right))
        name_piece
        (oneof_list [ '\000'; '\001'; '\t'; '\n'; '\r'; '\127' ])
        name_piece;
      map4
        (fun name left char right ->
          Header_value_newline (name, left, char, right))
        safe_header_name value_piece
        (oneof_list [ '\n'; '\r' ])
        value_piece;
    ]

let safe_tag_suffix =
  gen_string ~min:1 ~max:20 ~chars:"abcdefghijklmnopqrstuvwxyz0123456789"

let valid_tag_key = QCheck.Gen.map (( ^ ) "tag-") safe_tag_suffix
let valid_tag_value = gen_string ~min:0 ~max:24 ~chars:tag_chars

let invalid_tag_field =
  let open QCheck.Gen in
  let suffix = gen_string ~min:0 ~max:12 ~chars:tag_chars in
  oneof
    [
      map (fun suffix -> `Key ("aws:" ^ suffix)) suffix;
      map (fun suffix -> `Key ("AWS:" ^ suffix)) suffix;
      map (fun suffix -> `Key ("team," ^ suffix)) suffix;
      map (fun suffix -> `Key ("team\n" ^ suffix)) suffix;
      return (`Key (String.make 129 'a'));
      map (fun suffix -> `Value ("bad&" ^ suffix)) suffix;
      map (fun suffix -> `Value ("bad\n" ^ suffix)) suffix;
      map (fun suffix -> `Value ("emoji-\240\159\152\128" ^ suffix)) suffix;
      return (`Value (String.make 257 'a'));
    ]

let oversized_tag_set =
  let open QCheck.Gen in
  let* count = int_range 11 20 in
  let* values = list_size (return count) valid_tag_value in
  return
    (List.mapi (fun index value -> (Fmt.str "tag-%02d" index, value)) values)

let transfer_download_size =
  QCheck.Gen.(pair (int_range 0 10_000) (int_range 1 4096))

let transfer_upload_size =
  let open QCheck.Gen in
  pair (int_range 1 30_000_000)
    (int_range Awskit_s3.Transfer.min_part_size 9_000_000)
