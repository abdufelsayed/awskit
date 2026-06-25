let chars_of_string value = List.init (String.length value) (String.get value)
let gen_from_chars chars = QCheck.Gen.oneof_list (chars_of_string chars)

let gen_string ~min ~max ~chars =
  let open QCheck.Gen in
  string_size ~gen:(gen_from_chars chars) (int_range min max)

let query_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -_.~"

let tag_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 +-_=.:/@"

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
