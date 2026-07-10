open Awskit_s3

let count_from_env ~default =
  match Sys.getenv_opt "AWSKIT_QCHECK_COUNT" with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let broad_count = count_from_env ~default:500
let boundary_count = count_from_env ~default:250
let default_count = count_from_env ~default:200
let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true

let expect_ok label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let expect_error_field field = function
  | Ok _ -> Alcotest.failf "expected validation error for %s" field
  | Error error ->
      Alcotest.(check (option string))
        ("validation field " ^ field)
        (Some field)
        (Awskit.Error.validation_field error)

let repeat count value = List.init count (Fun.const value) |> String.concat ""
let repeat_char count char = String.init count (Fun.const char)
let qstring gen = QCheck.make ~print:(fun value -> String.escaped value) gen

let qpair pp_left pp_right gen =
  QCheck.make
    ~print:(fun (left, right) ->
      Fmt.str "%s=%s" (pp_left left) (pp_right right))
    gen

let gen_string ~min ~max ~chars =
  Protocol_generators.gen_string ~min ~max ~chars

let lower_digit_chars = "abcdefghijklmnopqrstuvwxyz0123456789"
let digit_chars = "0123456789"

let header_value_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_/.;= "

let tag_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 +-_=.:/@"

let object_key_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_ ;="

let valid_bucket_name =
  QCheck.Gen.(gen_string ~min:3 ~max:63 ~chars:lower_digit_chars)

let valid_bucket_name_with_separators =
  let open QCheck.Gen in
  let label = gen_string ~min:1 ~max:10 ~chars:lower_digit_chars in
  let* first_suffix = gen_string ~min:0 ~max:9 ~chars:lower_digit_chars in
  let* rest =
    list_size (int_range 1 4) (pair (oneof_list [ "."; "-" ]) label)
  in
  let first = "b" ^ first_suffix in
  let rest =
    match List.rev rest with
    | [] -> []
    | (separator, label) :: prefix ->
        List.rev ((separator, label ^ "z") :: prefix)
  in
  return
    (List.fold_left
       (fun acc (separator, label) -> acc ^ separator ^ label)
       first rest)

let invalid_length_bucket_name =
  let open QCheck.Gen in
  oneof
    [
      gen_string ~min:0 ~max:2 ~chars:lower_digit_chars;
      gen_string ~min:64 ~max:80 ~chars:lower_digit_chars;
    ]

let uppercase_bucket_name =
  let open QCheck.Gen in
  let* suffix = gen_string ~min:2 ~max:30 ~chars:lower_digit_chars in
  return ("A" ^ suffix)

let underscore_bucket_name =
  let open QCheck.Gen in
  let* left = gen_string ~min:1 ~max:20 ~chars:lower_digit_chars in
  let* right = gen_string ~min:1 ~max:20 ~chars:lower_digit_chars in
  return (left ^ "_" ^ right)

let edge_punctuation_bucket_name =
  let open QCheck.Gen in
  let* body = gen_string ~min:2 ~max:30 ~chars:lower_digit_chars in
  oneof_list [ "." ^ body; "-" ^ body; body ^ "."; body ^ "-" ]

let adjacent_dot_bucket_name =
  let open QCheck.Gen in
  let* middle = gen_string ~min:0 ~max:58 ~chars:lower_digit_chars in
  return ("a.." ^ middle ^ "z")

let ipv4_bucket_name =
  let open QCheck.Gen in
  let* a = int_range 0 255 in
  let* b = int_range 0 255 in
  let* c = int_range 0 255 in
  let* d = int_range 0 255 in
  return (Fmt.str "%d.%d.%d.%d" a b c d)

let reserved_bucket_name =
  let open QCheck.Gen in
  let prefix =
    oneof_list [ "xn--"; "sthree-"; "amzn-s3-demo-" ] >|= fun prefix ->
    prefix ^ "reserved"
  in
  let suffix =
    oneof_list [ "-s3alias"; "--ol-s3"; ".mrap"; "--x-s3"; "--table-s3" ]
    >|= fun suffix -> "reserved" ^ suffix
  in
  oneof [ prefix; suffix ]

let prop_bucket_names_round_trip =
  QCheck.Test.make ~count:broad_count ~name:"valid bucket names round-trip"
    (qstring valid_bucket_name) (fun value ->
      match Bucket_name.of_string value with
      | Ok bucket -> String.equal value (Bucket_name.to_string bucket)
      | Error _ -> false)

let prop_bucket_names_with_separators_round_trip =
  QCheck.Test.make ~count:broad_count
    ~name:"valid bucket names with separators round-trip"
    (qstring valid_bucket_name_with_separators) (fun value ->
      match (Bucket_name.of_string value, Bucket_name.of_string value) with
      | Ok left, Ok right ->
          String.equal value (Bucket_name.to_string left)
          && Bucket_name.equal left right
      | Error _, _ | _, Error _ -> false)

let prop_bucket_rejects_invalid_boundary_families =
  QCheck.Test.make ~count:boundary_count
    ~name:"invalid bucket boundary families are rejected"
    (qstring
       QCheck.Gen.(
         oneof
           [
             invalid_length_bucket_name;
             uppercase_bucket_name;
             underscore_bucket_name;
             edge_punctuation_bucket_name;
             adjacent_dot_bucket_name;
             ipv4_bucket_name;
             reserved_bucket_name;
           ]))
    (fun value -> is_error (Bucket_name.of_string value))

let valid_object_token = gen_string ~min:1 ~max:128 ~chars:header_value_chars
let valid_object_key_token = gen_string ~min:1 ~max:128 ~chars:object_key_chars

let oversized_object_key =
  QCheck.Gen.map
    (fun extra -> repeat_char (1024 + extra) 'a')
    (QCheck.Gen.int_range 1 32)

let prop_object_keys_round_trip =
  QCheck.Test.make ~count:broad_count ~name:"object keys round-trip"
    (qstring valid_object_key_token) (fun value ->
      match Object_key.of_string value with
      | Ok key -> String.equal value (Object_key.to_string key)
      | Error _ -> false)

let prop_object_prefixes_round_trip =
  QCheck.Test.make ~count:default_count ~name:"object prefixes round-trip"
    (qstring valid_object_token) (fun value ->
      match Object_key.Prefix.of_string value with
      | Ok prefix -> String.equal value (Object_key.Prefix.to_string prefix)
      | Error _ -> false)

let prop_object_delimiters_round_trip =
  QCheck.Test.make ~count:default_count ~name:"object delimiters round-trip"
    (qstring (gen_string ~min:1 ~max:8 ~chars:header_value_chars))
    (fun value ->
      match Object_key.Delimiter.of_string value with
      | Ok delimiter ->
          String.equal value (Object_key.Delimiter.to_string delimiter)
      | Error _ -> false)

let prop_object_keys_reject_over_1024_bytes =
  QCheck.Test.make ~count:boundary_count
    ~name:"object keys over 1024 bytes are rejected"
    (qstring oversized_object_key) (fun value ->
      is_error (Object_key.of_string value))

let prop_object_key_equality_agrees_with_original_spelling =
  QCheck.Test.make ~count:default_count
    ~name:"object key equality agrees with original spelling"
    (qstring valid_object_key_token) (fun value ->
      let other = value ^ "-other" in
      match
        ( Object_key.of_string value,
          Object_key.of_string value,
          Object_key.of_string other )
      with
      | Ok left, Ok same, Ok different ->
          Object_key.equal left same && not (Object_key.equal left different)
      | _ -> false)

let valid_account_id =
  QCheck.Gen.string_size
    ~gen:(Protocol_generators.gen_from_chars digit_chars)
    (QCheck.Gen.return 12)

let invalid_account_id =
  let open QCheck.Gen in
  oneof
    [
      gen_string ~min:0 ~max:11 ~chars:digit_chars;
      gen_string ~min:13 ~max:20 ~chars:digit_chars;
      map
        (fun suffix -> "12345678901" ^ suffix)
        (gen_string ~min:1 ~max:1 ~chars:"abcdefghijklmnopqrstuvwxyz");
    ]

let prop_account_ids_round_trip =
  QCheck.Test.make ~count:default_count ~name:"account ids round-trip"
    (qstring valid_account_id) (fun value ->
      match Account_id.of_string value with
      | Ok account_id -> String.equal value (Account_id.to_string account_id)
      | Error _ -> false)

let prop_account_ids_reject_invalid_boundaries =
  QCheck.Test.make ~count:boundary_count
    ~name:"invalid account id boundaries are rejected"
    (qstring invalid_account_id) (fun value ->
      is_error (Account_id.of_string value))

let known_checksum_algorithms =
  let module Algorithm = Object.Checksum.Algorithm in
  let open Algorithm in
  [
    Crc32;
    Crc32c;
    Crc64nvme;
    Sha1;
    Sha256;
    Sha512;
    Md5;
    Xxhash64;
    Xxhash3;
    Xxhash128;
  ]

let known_checksum_types =
  let module Type = Object.Checksum.Type in
  let open Type in
  [ Composite; Full_object ]

let valid_header_value = gen_string ~min:1 ~max:64 ~chars:header_value_chars

let invalid_header_value =
  let open QCheck.Gen in
  let* left = gen_string ~min:0 ~max:12 ~chars:header_value_chars in
  let* right = gen_string ~min:0 ~max:12 ~chars:header_value_chars in
  oneof_list
    [ ""; left ^ "\n" ^ right; left ^ "\r" ^ right; left ^ "\127" ^ right ]

let prop_content_types_round_trip =
  QCheck.Test.make ~count:default_count ~name:"content types round-trip"
    (qstring valid_header_value) (fun value ->
      match Content_type.of_string value with
      | Ok content_type ->
          String.equal value (Content_type.to_string content_type)
      | Error _ -> false)

let prop_header_values_reject_empty_or_control =
  QCheck.Test.make ~count:boundary_count
    ~name:"header values reject empty and control characters"
    (qstring invalid_header_value) (fun value ->
      is_error (Header_value.of_string ~field:"cache-control" value))

let prop_checksum_values_reject_empty_or_control =
  QCheck.Test.make ~count:boundary_count
    ~name:"checksum values reject empty and control characters"
    (qstring invalid_header_value) (fun value ->
      is_error
        (Object.Checksum.value ~algorithm:Object.Checksum.Algorithm.Sha256
           ~value))

let prop_checksum_algorithm_render_parse_is_stable =
  QCheck.Test.make ~count:default_count
    ~name:"known checksum algorithms render and parse stably"
    (QCheck.make ~print:Object.Checksum.Algorithm.to_string
       (QCheck.Gen.oneof_list known_checksum_algorithms))
    (fun algorithm ->
      Object.Checksum.Algorithm.(of_string (to_string algorithm) = Ok algorithm))

let prop_checksum_type_render_parse_is_stable =
  QCheck.Test.make ~count:default_count
    ~name:"known checksum types render and parse stably"
    (QCheck.make ~print:Object.Checksum.Type.to_string
       (QCheck.Gen.oneof_list known_checksum_types))
    (fun checksum_type ->
      Object.Checksum.Type.(
        of_string (to_string checksum_type) = Ok checksum_type))

let metadata_entries =
  let open QCheck.Gen in
  let key index = Fmt.str "k%d" index in
  let* count = int_range 0 20 in
  let* values = list_size (return count) valid_header_value in
  return (List.mapi (fun index value -> (key index, value)) values)

let print_entries entries =
  entries
  |> List.map (fun (key, value) -> Fmt.str "%s=%S" key value)
  |> String.concat ";"

let equal_pair (lk, lv) (rk, rv) = String.equal lk rk && String.equal lv rv

let prop_metadata_preserves_order =
  QCheck.Test.make ~count:default_count ~name:"metadata preserves order"
    (QCheck.make ~print:print_entries metadata_entries) (fun entries ->
      match Metadata.of_list entries with
      | Ok metadata -> List.equal equal_pair entries (Metadata.to_list metadata)
      | Error _ -> false)

let prop_metadata_entries_match_raw_list =
  QCheck.Test.make ~count:default_count
    ~name:"metadata entries match raw insertion list"
    (QCheck.make ~print:print_entries metadata_entries) (fun entries ->
      match Metadata.of_list entries with
      | Error _ -> false
      | Ok metadata ->
          let typed =
            Metadata.entries metadata
            |> List.map (fun (entry : Metadata.entry) ->
                (entry.key, Header_value.to_string entry.value))
          in
          List.equal equal_pair entries typed)

let prop_metadata_rejects_case_insensitive_duplicates =
  let open QCheck.Gen in
  QCheck.Test.make ~count:default_count
    ~name:"metadata rejects case-insensitive duplicate keys"
    (qpair Fun.id Fun.id
       (pair
          (gen_string ~min:1 ~max:12 ~chars:"abcdefghijklmnopqrstuvwxyz")
          valid_header_value))
    (fun (key, value) ->
      is_error
        (Metadata.of_list
           [ (String.capitalize_ascii key, value); (key, value ^ "x") ]))

let prop_metadata_add_rejects_existing_key_case_insensitively =
  let open QCheck.Gen in
  QCheck.Test.make ~count:default_count
    ~name:"metadata add rejects existing key case-insensitively"
    (qpair Fun.id Fun.id
       (pair
          (gen_string ~min:1 ~max:12 ~chars:"abcdefghijklmnopqrstuvwxyz")
          valid_header_value))
    (fun (key, value) ->
      match Metadata.of_list [ (key, value) ] with
      | Error _ -> false
      | Ok metadata ->
          is_error
            (Metadata.add
               ~key:(String.uppercase_ascii key)
               ~value:(value ^ "x") metadata))

let valid_tag_key = gen_string ~min:1 ~max:64 ~chars:lower_digit_chars
let valid_tag_value = gen_string ~min:0 ~max:64 ~chars:tag_chars

let prop_tags_round_trip =
  QCheck.Test.make ~count:default_count ~name:"tags round-trip"
    (qpair Fun.id Fun.id QCheck.Gen.(pair valid_tag_key valid_tag_value))
    (fun (key, value) ->
      match Tag.create ~key ~value with
      | Ok tag ->
          String.equal key (Tag.key tag) && String.equal value (Tag.value tag)
      | Error _ -> false)

let prop_tag_sets_reject_duplicate_keys =
  QCheck.Test.make ~count:default_count ~name:"tag sets reject duplicate keys"
    (qpair Fun.id Fun.id QCheck.Gen.(pair valid_tag_key valid_tag_value))
    (fun (key, value) ->
      let first = Tag.create ~key ~value in
      let second = Tag.create ~key ~value:(value ^ "x") in
      match (first, second) with
      | Ok first, Ok second -> is_error (Tag.Set.of_list [ first; second ])
      | _ -> false)

let prop_tag_sets_treat_key_case_as_distinct =
  QCheck.Test.make ~count:default_count
    ~name:"tag sets treat key case as distinct"
    (qpair Fun.id Fun.id
       QCheck.Gen.(
         pair
           (gen_string ~min:1 ~max:12 ~chars:"abcdefghijklmnopqrstuvwxyz")
           valid_tag_value))
    (fun (key, value) ->
      let upper_key = String.uppercase_ascii key in
      match
        (Tag.create ~key ~value, Tag.create ~key:upper_key ~value:(value ^ "x"))
      with
      | Ok lower, Ok upper -> (
          match
            Result.map Tag.Set.to_list (Tag.Set.of_list [ lower; upper ])
          with
          | Ok tags -> List.equal Tag.equal [ lower; upper ] tags
          | Error _ -> false)
      | _ -> false)

let valid_byte_range =
  let open QCheck.Gen in
  let* start = int_range 0 100_000 in
  let* length = int_range 0 10_000 in
  return (Int64.of_int start, Int64.of_int (start + length))

let valid_range_start = QCheck.Gen.(map Int64.of_int (int_range 0 100_000))
let valid_suffix_length = QCheck.Gen.(map Int64.of_int (int_range 1 100_000))

let prop_byte_ranges_render_and_view =
  QCheck.Test.make ~count:default_count
    ~name:"byte ranges render exact inclusive bounds"
    (QCheck.make
       ~print:(fun (start, finish) -> Fmt.str "%Ld-%Ld" start finish)
       valid_byte_range)
    (fun (start, finish) ->
      match Range.bytes ~start ~finish with
      | Error _ -> false
      | Ok range -> (
          String.equal
            (Fmt.str "bytes=%Ld-%Ld" start finish)
            (Range.to_header range)
          &&
          match Range.view range with
          | Range.Bytes (actual_start, actual_finish) ->
              Int64.equal start actual_start && Int64.equal finish actual_finish
          | Range.From _ | Range.Suffix _ -> false))

let prop_from_ranges_render_and_view =
  QCheck.Test.make ~count:default_count
    ~name:"from ranges render open-ended starts"
    (QCheck.make ~print:Int64.to_string valid_range_start) (fun start ->
      match Range.from start with
      | Error _ -> false
      | Ok range -> (
          String.equal (Fmt.str "bytes=%Ld-" start) (Range.to_header range)
          &&
          match Range.view range with
          | Range.From actual -> Int64.equal start actual
          | Range.Bytes _ | Range.Suffix _ -> false))

let prop_suffix_ranges_render_and_view =
  QCheck.Test.make ~count:default_count
    ~name:"suffix ranges render positive lengths"
    (QCheck.make ~print:Int64.to_string valid_suffix_length) (fun length ->
      match Range.suffix length with
      | Error _ -> false
      | Ok range -> (
          String.equal (Fmt.str "bytes=-%Ld" length) (Range.to_header range)
          &&
          match Range.view range with
          | Range.Suffix actual -> Int64.equal length actual
          | Range.Bytes _ | Range.From _ -> false))

let valid_upload_id = gen_string ~min:1 ~max:80 ~chars:header_value_chars

let prop_upload_ids_round_trip =
  QCheck.Test.make ~count:default_count ~name:"upload ids round-trip"
    (qstring valid_upload_id) (fun value ->
      match Multipart.Upload_id.of_string value with
      | Ok upload_id ->
          String.equal value (Multipart.Upload_id.to_string upload_id)
      | Error _ -> false)

let prop_part_numbers_round_trip =
  QCheck.Test.make ~count:default_count ~name:"part numbers round-trip"
    (QCheck.make ~print:string_of_int QCheck.Gen.(int_range 1 10_000))
    (fun value ->
      match Multipart.Part_number.of_int value with
      | Ok part_number -> value = Multipart.Part_number.to_int part_number
      | Error _ -> false)

let prop_part_number_markers_round_trip =
  QCheck.Test.make ~count:default_count ~name:"part number markers round-trip"
    (QCheck.make ~print:string_of_int QCheck.Gen.(int_range 0 10_000))
    (fun value ->
      match Multipart.Part_number_marker.of_int value with
      | Ok marker -> value = Multipart.Part_number_marker.to_int marker
      | Error _ -> false)

let prop_upload_of_strings_preserves_valid_identifiers =
  QCheck.Test.make ~count:default_count
    ~name:"multipart upload raw-string constructor validates and preserves ids"
    (QCheck.make
       ~print:(fun (bucket, key, upload_id) ->
         Fmt.str "%s/%s/%s" bucket key upload_id)
       QCheck.Gen.(triple valid_bucket_name valid_object_token valid_upload_id))
    (fun (bucket, key, upload_id) ->
      let upload_id = Multipart.Upload_id.of_string_exn upload_id in
      match Multipart.Upload.of_strings ~bucket ~key ~upload_id with
      | Error _ -> false
      | Ok upload ->
          String.equal bucket
            (Multipart.Upload.bucket upload |> Bucket_name.to_string)
          && String.equal key
               (Multipart.Upload.key upload |> Object_key.to_string)
          && Multipart.Upload_id.equal upload_id
               (Multipart.Upload.upload_id upload))

let prop_multipart_parts_preserve_known_checksum_and_size =
  QCheck.Test.make ~count:default_count
    ~name:"multipart parts preserve checksum and size"
    (QCheck.make
       ~print:(fun (part_number, size, checksum) ->
         Fmt.str "part=%d size=%Ld checksum=%S" part_number size checksum)
       QCheck.Gen.(
         triple (int_range 1 10_000)
           (map Int64.of_int (int_range 0 10_000_000))
           valid_header_value))
    (fun (part_number, size, checksum_value) ->
      let part_number = Multipart.Part_number.of_int_exn part_number in
      let etag = Object.Etag.of_string_exn "\"etag\"" in
      let checksum =
        Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
          ~value:checksum_value
      in
      match Multipart.Part.create ~part_number ~etag ~checksum ~size () with
      | Error _ -> false
      | Ok part ->
          Multipart.Part_number.equal part_number
            (Multipart.Part.part_number part)
          && Object.Etag.equal etag (Multipart.Part.etag part)
          && Multipart.Part.checksum part = Some checksum
          && Multipart.Part.size part = Some size)

let prop_complete_parts_accept_matching_non_negative_object_size =
  QCheck.Test.make ~count:default_count
    ~name:"complete parts accept matching non-negative object sizes"
    (QCheck.make ~print:Int64.to_string
       QCheck.Gen.(map Int64.of_int (int_range 0 100_000_000)))
    (fun multipart_object_size ->
      let part =
        Multipart.Part.create_exn
          ~part_number:(Multipart.Part_number.of_int_exn 1)
          ~etag:(Object.Etag.of_string_exn "\"etag\"")
          ~size:multipart_object_size ()
      in
      match
        Multipart.Complete.Parts.of_list ~multipart_object_size [ part ]
      with
      | Ok parts ->
          Option.equal Int64.equal (Some multipart_object_size)
            (Multipart.Complete.Parts.multipart_object_size parts)
      | Error _ -> false)

let test_bucket_boundaries () =
  let bucket =
    expect_ok "ordinary bucket" (Bucket_name.of_string "example-bucket")
  in
  Alcotest.(check string)
    "bucket round trips" "example-bucket"
    (Bucket_name.to_string bucket);
  expect_error_field "bucket" (Bucket_name.of_string "ab");
  expect_error_field "bucket" (Bucket_name.of_string (repeat_char 64 'a'));
  expect_error_field "bucket" (Bucket_name.of_string "Example");
  expect_error_field "bucket" (Bucket_name.of_string "bucket_name");
  expect_error_field "bucket" (Bucket_name.of_string "a..b");
  expect_error_field "bucket" (Bucket_name.of_string "192.168.0.1");
  expect_error_field "bucket" (Bucket_name.of_string "xn--reserved");
  expect_error_field "bucket" (Bucket_name.of_string "bucket--x-s3")

let test_object_key_prefix_delimiter_boundaries () =
  let key = expect_ok "key" (Object_key.of_string "logs/2026/06/file.txt") in
  Alcotest.(check string)
    "key round trips" "logs/2026/06/file.txt" (Object_key.to_string key);
  ignore
    (expect_ok "balanced relative key"
       (Object_key.of_string "videos/2014/../../video1.wmv"));
  ignore
    (expect_ok "period segment key" (Object_key.of_string "folder/./file.txt"));
  ignore
    (expect_ok "max byte key" (Object_key.of_string (repeat_char 1024 'a')));
  ignore
    (expect_ok "max byte utf8 key"
       (Object_key.of_string (repeat 512 "\195\169")));
  expect_error_field "key" (Object_key.of_string "");
  expect_error_field "key" (Object_key.of_string (repeat_char 1025 'a'));
  expect_error_field "key" (Object_key.of_string (repeat 513 "\195\169"));
  expect_error_field "key" (Object_key.of_string "\xC0\x80");
  expect_error_field "key" (Object_key.of_string "../video1.wmv");
  expect_error_field "key" (Object_key.of_string "videos/../../video1.wmv");
  let prefix = expect_ok "prefix" (Object_key.Prefix.of_string "logs/") in
  Alcotest.(check string)
    "prefix round trips" "logs/"
    (Object_key.Prefix.to_string prefix);
  expect_error_field "prefix" (Object_key.Prefix.of_string "");
  expect_error_field "prefix" (Object_key.Prefix.of_string "\xFF");
  let delimiter = expect_ok "delimiter" (Object_key.Delimiter.of_string "/") in
  Alcotest.(check string)
    "delimiter round trips" "/"
    (Object_key.Delimiter.to_string delimiter);
  expect_error_field "delimiter" (Object_key.Delimiter.of_string "")

let test_account_header_and_checksum_boundaries () =
  let account = expect_ok "account" (Account_id.of_string "123456789012") in
  Alcotest.(check string)
    "account round trips" "123456789012"
    (Account_id.to_string account);
  expect_error_field "account_id" (Account_id.of_string "12345678901");
  expect_error_field "account_id" (Account_id.of_string "12345678901x");
  let content_type =
    expect_ok "content type"
      (Content_type.of_string "text/plain; charset=utf-8")
  in
  Alcotest.(check string)
    "content type round trips" "text/plain; charset=utf-8"
    (Content_type.to_string content_type);
  expect_error_field "content_type" (Content_type.of_string "");
  expect_error_field "content_type" (Content_type.of_string "text/plain\r\n");
  let header =
    expect_ok "header"
      (Header_value.of_string ~field:"cache-control" "max-age=60")
  in
  Alcotest.(check string)
    "header round trips" "max-age=60"
    (Header_value.to_string header);
  expect_error_field "cache-control"
    (Header_value.of_string ~field:"cache-control" "");
  let checksum =
    expect_ok "checksum"
      (Object.Checksum.value ~algorithm:Object.Checksum.Algorithm.Sha256
         ~value:"provided-sha256")
  in
  Alcotest.(check string) "checksum value" "provided-sha256" checksum.value;
  expect_error_field "checksum_value"
    (Object.Checksum.value ~algorithm:Object.Checksum.Algorithm.Sha256 ~value:"");
  expect_error_field "checksum_algorithm"
    (Object.Checksum.Algorithm.of_string "FUTURE");
  let observed = Object.Checksum.Algorithm.observed_of_string "FUTURE" in
  Alcotest.(check string)
    "observed checksum algorithm" "FUTURE"
    (Object.Checksum.Algorithm.observed_to_string observed)

let test_object_identity_boundaries () =
  let etag = expect_ok "etag" (Object.Etag.of_string "\"abc\"") in
  let same_etag = Object.Etag.of_string_exn "\"abc\"" in
  let other_etag = Object.Etag.of_string_exn "\"def\"" in
  Alcotest.(check string) "etag spelling" "\"abc\"" (Object.Etag.to_string etag);
  Alcotest.(check bool) "same etag" true (Object.Etag.equal etag same_etag);
  Alcotest.(check bool)
    "different etag" false
    (Object.Etag.equal etag other_etag);
  expect_error_field "etag" (Object.Etag.of_string "");
  expect_error_field "etag" (Object.Etag.of_string "bad\retag");
  let version = expect_ok "version" (Object.Version_id.of_string "v1") in
  Alcotest.(check string)
    "version spelling" "v1"
    (Object.Version_id.to_string version);
  expect_error_field "version_id" (Object.Version_id.of_string "");
  expect_error_field "version_id" (Object.Version_id.of_string "bad\nversion");
  let zero_keys =
    expect_ok "zero max keys" (Object.List.options ~max_keys:0 ())
  in
  Alcotest.(check (option int)) "zero max keys" (Some 0) zero_keys.max_keys;
  expect_error_field "max_keys" (Object.List.options ~max_keys:(-1) ());
  expect_error_field "max_keys" (Object.List.options ~max_keys:1001 ());
  ignore
    (expect_ok "zero version max keys" (Object.Versions.options ~max_keys:0 ()));
  expect_error_field "max_keys" (Object.Versions.options ~max_keys:(-1) ());
  expect_error_field "max_keys" (Object.Versions.options ~max_keys:1001 ());
  expect_error_field "version_id_marker"
    (Object.Versions.options ~version_id_marker:version ());
  ignore
    (expect_ok "version marker pair"
       (Object.Versions.options
          ~key_marker:(Object_key.of_string_exn "key")
          ~version_id_marker:version ()));
  let owner = Object.Owner.create ~id:"owner-id" ~display_name:"owner" () in
  Alcotest.(check (option string))
    "owner id" (Some "owner-id")
    (Option.bind owner (fun (owner : Object.Owner.t) -> owner.id));
  Alcotest.(check bool)
    "empty owner omitted" true
    (Option.is_none (Object.Owner.create ~id:"" ~display_name:"" ()))

let test_metadata_collection_boundaries () =
  let metadata =
    expect_ok "metadata"
      (Metadata.of_list [ ("source", "api"); ("trace", "abc123") ])
  in
  Alcotest.(check (list (pair string string)))
    "metadata preserves order"
    [ ("source", "api"); ("trace", "abc123") ]
    (Metadata.to_list metadata);
  let metadata =
    expect_ok "metadata add" (Metadata.add ~key:"team" ~value:"sdk" metadata)
  in
  Alcotest.(check (list (pair string string)))
    "metadata add appends"
    [ ("source", "api"); ("trace", "abc123"); ("team", "sdk") ]
    (Metadata.to_list metadata);
  expect_error_field "metadata" (Metadata.of_list [ ("", "value") ]);
  expect_error_field "metadata"
    (Metadata.of_list [ ("x-amz-meta-source", "value") ]);
  expect_error_field "metadata"
    (Metadata.of_list [ ("Source", "one"); ("source", "two") ]);
  expect_error_field "metadata source"
    (Metadata.of_list [ ("source", "bad\nvalue") ]);
  ignore
    (expect_ok "metadata 2 KiB aggregate"
       (Metadata.of_list [ ("k", repeat 1023 "\195\169" ^ "x") ]));
  expect_error_field "metadata"
    (Metadata.of_list [ ("k", repeat 1023 "\195\169" ^ "xx") ]);
  let full_metadata =
    expect_ok "full metadata" (Metadata.of_list [ ("a", repeat_char 2047 'x') ])
  in
  expect_error_field "metadata" (Metadata.add ~key:"b" ~value:"c" full_metadata)

let test_range_boundaries () =
  let bytes = expect_ok "bytes range" (Range.bytes ~start:2L ~finish:5L) in
  Alcotest.(check string) "bytes header" "bytes=2-5" (Range.to_header bytes);
  (match Range.view bytes with
  | Range.Bytes (start, finish) ->
      Alcotest.(check int64) "bytes start" 2L start;
      Alcotest.(check int64) "bytes finish" 5L finish
  | Range.From _ | Range.Suffix _ -> Alcotest.fail "expected byte range");
  let from = expect_ok "from range" (Range.from 10L) in
  Alcotest.(check string) "from header" "bytes=10-" (Range.to_header from);
  let suffix = expect_ok "suffix range" (Range.suffix 10L) in
  Alcotest.(check string) "suffix header" "bytes=-10" (Range.to_header suffix);
  expect_error_field "range start" (Range.bytes ~start:(-1L) ~finish:1L);
  expect_error_field "range finish" (Range.bytes ~start:0L ~finish:(-1L));
  expect_error_field "range" (Range.bytes ~start:5L ~finish:4L);
  expect_error_field "range start" (Range.from (-1L));
  expect_error_field "range suffix" (Range.suffix 0L);
  expect_error_field "range suffix" (Range.suffix (-1L))

let test_tag_boundaries () =
  let env = expect_ok "tag env" (Tag.create ~key:"env" ~value:"dev") in
  let region = expect_ok "tag region" (Tag.create ~key:"region" ~value:"") in
  let unicode =
    expect_ok "unicode tag"
      (Tag.create ~key:"team-\206\180" ~value:"zone\194\160a")
  in
  Alcotest.(check string) "tag key" "env" (Tag.key env);
  Alcotest.(check string) "unicode tag key" "team-\206\180" (Tag.key unicode);
  let tags = expect_ok "tag set" (Tag.Set.of_list [ env; region ]) in
  Alcotest.(check (list string))
    "tag set preserves order" [ "env"; "region" ]
    (List.map Tag.key (Tag.Set.to_list tags));
  ignore
    (expect_ok "astral tag key at UTF-16 limit"
       (Tag.create ~key:(repeat 64 "\240\144\144\128") ~value:"v"));
  expect_error_field "tag key" (Tag.create ~key:"" ~value:"dev");
  expect_error_field "tag key"
    (Tag.create ~key:(repeat_char 129 'a') ~value:"dev");
  expect_error_field "tag key"
    (Tag.create ~key:(repeat 65 "\240\144\144\128") ~value:"dev");
  expect_error_field "tag key" (Tag.create ~key:"aws:team" ~value:"dev");
  expect_error_field "tag key" (Tag.create ~key:"AWS:team" ~value:"dev");
  expect_error_field "tag key" (Tag.create ~key:"env,team" ~value:"dev");
  expect_error_field "tag value"
    (Tag.create ~key:"env" ~value:(repeat_char 257 'a'));
  expect_error_field "tag value" (Tag.create ~key:"env" ~value:"bad\nvalue");
  expect_error_field "tag value" (Tag.create ~key:"env" ~value:"bad&value");
  expect_error_field "tag value"
    (Tag.create ~key:"env" ~value:"emoji-\240\159\152\128");
  let duplicate =
    expect_ok "duplicate tag" (Tag.create ~key:"env" ~value:"prod")
  in
  expect_error_field "tag key" (Tag.Set.of_list [ env; duplicate ]);
  let eleven_tags =
    List.init 11 (fun index ->
        expect_ok "tag" (Tag.create ~key:(Fmt.str "k%d" index) ~value:"v"))
  in
  expect_error_field "tags" (Tag.Set.of_list eleven_tags)

let test_multipart_boundaries () =
  let bucket = Bucket_name.of_string_exn "multipart-bucket" in
  let key = Object_key.of_string_exn "large.bin" in
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  Alcotest.(check string)
    "upload id round trips" "upload-1"
    (Multipart.Upload_id.to_string upload_id);
  expect_error_field "upload_id" (Multipart.Upload_id.of_string "");
  expect_error_field "upload_id" (Multipart.Upload_id.of_string "bad\nid");
  let first = Multipart.Part_number.of_int_exn 1 in
  let last = Multipart.Part_number.of_int_exn 10_000 in
  Alcotest.(check int) "first part" 1 (Multipart.Part_number.to_int first);
  Alcotest.(check int) "last part" 10_000 (Multipart.Part_number.to_int last);
  expect_error_field "part_number" (Multipart.Part_number.of_int 0);
  expect_error_field "part_number" (Multipart.Part_number.of_int 10_001);
  let first_marker = Multipart.Part_number_marker.of_int_exn 0 in
  let last_marker = Multipart.Part_number_marker.of_int_exn 10_000 in
  Alcotest.(check int)
    "first marker" 0
    (Multipart.Part_number_marker.to_int first_marker);
  Alcotest.(check int)
    "last marker" 10_000
    (Multipart.Part_number_marker.to_int last_marker);
  expect_error_field "part_number_marker"
    (Multipart.Part_number_marker.of_int (-1));
  expect_error_field "part_number_marker"
    (Multipart.Part_number_marker.of_int 10_001);
  let resumed = Multipart.Upload.resume ~bucket ~key ~upload_id in
  Alcotest.(check string)
    "resume upload id" "upload-1"
    (Multipart.Upload.upload_id resumed |> Multipart.Upload_id.to_string);
  let caller_owned =
    Multipart.Upload.created ~bucket ~key ~upload_id
    |> Multipart.Upload.as_caller_owned
  in
  Alcotest.(check string)
    "caller owned upload id" "upload-1"
    (Multipart.Upload.upload_id caller_owned |> Multipart.Upload_id.to_string);
  let etag = Object.Etag.of_string_exn "\"etag-1\"" in
  expect_error_field "etag" (Object.Etag.of_string "");
  expect_error_field "etag" (Object.Etag.of_string "bad\netag");
  let part = Multipart.Part.create_exn ~part_number:first ~etag ~size:0L () in
  Alcotest.(check (option int64))
    "part size" (Some 0L) (Multipart.Part.size part);
  expect_error_field "size"
    (Multipart.Part.create ~part_number:first ~etag ~size:(-1L) ());
  let zero_parts =
    expect_ok "zero max parts" (Multipart.List_parts.options ~max_parts:0 ())
  in
  Alcotest.(check (option int)) "zero max parts" (Some 0) zero_parts.max_parts;
  expect_error_field "max_parts"
    (Multipart.List_parts.options ~max_parts:(-1) ());
  expect_error_field "max_parts"
    (Multipart.List_parts.options ~max_parts:1001 ())

let test_multipart_option_boundaries () =
  let upload_id = Multipart.Upload_id.of_string_exn "upload-1" in
  let upload =
    expect_ok "raw upload"
      (Multipart.Upload.of_strings ~bucket:"valid-bucket" ~key:"file.bin"
         ~upload_id)
  in
  Alcotest.(check string)
    "raw upload bucket" "valid-bucket"
    (Multipart.Upload.bucket upload |> Bucket_name.to_string);
  expect_error_field "bucket"
    (Multipart.Upload.of_strings ~bucket:"Invalid" ~key:"file.bin" ~upload_id);
  expect_error_field "key"
    (Multipart.Upload.of_strings ~bucket:"valid-bucket" ~key:"" ~upload_id);
  let bad_checksum =
    Object.Checksum.value ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"bad\nchecksum"
  in
  expect_error_field "checksum_value" bad_checksum;
  let future_storage_class =
    expect_ok "future storage class" (Storage_class.of_string "FUTURE")
  in
  ignore (Multipart.Create.options ~storage_class:future_storage_class ());
  Alcotest.(check string)
    "future storage class spelling" "FUTURE"
    (Storage_class.to_string future_storage_class);
  expect_error_field "storage_class" (Storage_class.of_string "bad\nvalue");
  expect_error_field "checksum_algorithm"
    (Object.Checksum.Algorithm.of_string "FUTURE");
  expect_error_field "checksum_type" (Object.Checksum.Type.of_string "FUTURE");
  let full_object_algorithms =
    [
      Object.Checksum.Algorithm.Crc32;
      Object.Checksum.Algorithm.Crc32c;
      Object.Checksum.Algorithm.Crc64nvme;
    ]
  in
  List.iter
    (fun algorithm ->
      let default =
        expect_ok "multipart checksum with service-default type"
          (Multipart.Create.Checksum.create ~algorithm ())
      in
      Alcotest.(check bool)
        "default checksum algorithm" true
        (default.algorithm = algorithm);
      let composite =
        Multipart.Create.Checksum.create ~algorithm
          ~checksum_type:Object.Checksum.Type.Composite ()
      in
      if algorithm = Object.Checksum.Algorithm.Crc64nvme then
        expect_error_field "checksum_type" composite
      else ignore (expect_ok "composite multipart checksum" composite);
      let full_object =
        Multipart.Create.Checksum.create ~algorithm
          ~checksum_type:Object.Checksum.Type.Full_object ()
      in
      if List.mem algorithm full_object_algorithms then
        ignore (expect_ok "full-object multipart checksum" full_object)
      else expect_error_field "checksum_type" full_object)
    known_checksum_algorithms;
  let checksum =
    Multipart.Create.Checksum.create_exn
      ~algorithm:Object.Checksum.Algorithm.Crc32
      ~checksum_type:Object.Checksum.Type.Full_object ()
  in
  let create_options = Multipart.Create.options ~checksum () in
  Alcotest.(check bool)
    "validated create checksum retained" true
    (create_options.checksum = Some checksum);
  let completion_part =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn 1)
      ~etag:(Object.Etag.of_string_exn "\"completion\"")
      ~size:0L ()
  in
  ignore
    (expect_ok "complete zero object size"
       (Multipart.Complete.Parts.of_list ~multipart_object_size:0L
          [ completion_part ]));
  expect_error_field "multipart_object_size"
    (Multipart.Complete.Parts.of_list ~multipart_object_size:(-1L)
       [ completion_part ]);
  let sha512 =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha512
      ~value:"sha512"
  in
  expect_error_field "checksum_type"
    (Multipart.Complete.options ~checksum:sha512
       ~checksum_type:Object.Checksum.Type.Full_object ());
  ignore
    (expect_ok "checksum type assertion without value"
       (Multipart.Complete.options
          ~checksum_type:Object.Checksum.Type.Full_object ()));
  let md5 =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Md5
      ~value:"md5"
  in
  let checksummed_part number checksum size =
    Multipart.Part.create_exn
      ~part_number:(Multipart.Part_number.of_int_exn number)
      ~etag:(Object.Etag.of_string_exn (Fmt.str "\"part-%d\"" number))
      ~checksum ~size ()
  in
  expect_error_field "checksum_algorithm"
    (Multipart.Complete.Parts.of_list
       [ checksummed_part 1 sha512 5_242_880L; checksummed_part 2 md5 0L ]);
  let observed_type = Object.Checksum.Type.observed_of_string "FUTURE" in
  Alcotest.(check string)
    "observed checksum type" "FUTURE"
    (Object.Checksum.Type.observed_to_string observed_type);
  let sse_kms =
    expect_ok "SSE-KMS"
      (Encryption.Destination.sse_kms ~key_id:"key-id" ~bucket_key_enabled:true
         ())
  in
  (match sse_kms with
  | Encryption.Destination.Sse_kms { bucket_key_enabled = Some true; _ } -> ()
  | _ -> Alcotest.fail "expected SSE-KMS bucket-key setting");
  let dsse_kms =
    expect_ok "DSSE-KMS" (Encryption.Destination.dsse_kms ~key_id:"key-id" ())
  in
  (match dsse_kms with
  | Encryption.Destination.Dsse_kms { key_id = Some "key-id" } -> ()
  | _ -> Alcotest.fail "expected DSSE-KMS key id");
  expect_error_field "sse_kms_key_id"
    (Encryption.Destination.dsse_kms ~key_id:"bad\nkey" ())

let test_delete_many_object_boundaries () =
  let member key =
    Object.Delete_many.object_ ~key:(Object_key.of_string_exn key) ()
  in
  expect_error_field "objects" (Object.Delete_many.Objects.of_list []);
  let one =
    expect_ok "one delete member"
      (Object.Delete_many.Objects.of_list [ member "one" ])
  in
  Alcotest.(check int)
    "one delete member" 1
    (Object.Delete_many.Objects.length one);
  let ordered =
    expect_ok "ordered delete members"
      (Object.Delete_many.Objects.of_list
         [ member "first"; member "second"; member "first" ])
  in
  Alcotest.(check (list string))
    "order and duplicates"
    [ "first"; "second"; "first" ]
    (Object.Delete_many.Objects.to_list ordered
    |> List.map (fun (object_ : Object.Delete_many.object_) ->
        Object_key.to_string object_.key));
  let maximum =
    List.init Object.Delete_many.max_objects (fun index ->
        member (Fmt.str "key-%d" index))
  in
  let maximum =
    expect_ok "maximum delete members"
      (Object.Delete_many.Objects.of_list maximum)
  in
  Alcotest.(check int)
    "maximum delete members" Object.Delete_many.max_objects
    (Object.Delete_many.Objects.length maximum);
  let too_many =
    member "overflow" :: Object.Delete_many.Objects.to_list maximum
  in
  expect_error_field "objects" (Object.Delete_many.Objects.of_list too_many)

let suite =
  [
    ( "pbt:awskit-s3:domain:bucket",
      List.map Protocol_support.to_alcotest
        [
          prop_bucket_names_round_trip;
          prop_bucket_names_with_separators_round_trip;
          prop_bucket_rejects_invalid_boundary_families;
        ] );
    ( "pbt:awskit-s3:domain:object",
      List.map Protocol_support.to_alcotest
        [
          prop_object_keys_round_trip;
          prop_object_prefixes_round_trip;
          prop_object_delimiters_round_trip;
          prop_object_keys_reject_over_1024_bytes;
          prop_object_key_equality_agrees_with_original_spelling;
        ] );
    ( "pbt:awskit-s3:domain:headers",
      List.map Protocol_support.to_alcotest
        [
          prop_account_ids_round_trip;
          prop_account_ids_reject_invalid_boundaries;
          prop_content_types_round_trip;
          prop_header_values_reject_empty_or_control;
          prop_checksum_values_reject_empty_or_control;
          prop_checksum_algorithm_render_parse_is_stable;
          prop_checksum_type_render_parse_is_stable;
        ] );
    ( "pbt:awskit-s3:domain:metadata-tags",
      List.map Protocol_support.to_alcotest
        [
          prop_metadata_preserves_order;
          prop_metadata_entries_match_raw_list;
          prop_metadata_rejects_case_insensitive_duplicates;
          prop_metadata_add_rejects_existing_key_case_insensitively;
          prop_tags_round_trip;
          prop_tag_sets_reject_duplicate_keys;
          prop_tag_sets_treat_key_case_as_distinct;
        ] );
    ( "pbt:awskit-s3:domain:range",
      List.map Protocol_support.to_alcotest
        [
          prop_byte_ranges_render_and_view;
          prop_from_ranges_render_and_view;
          prop_suffix_ranges_render_and_view;
        ] );
    ( "pbt:awskit-s3:domain:multipart",
      List.map Protocol_support.to_alcotest
        [
          prop_upload_ids_round_trip;
          prop_part_numbers_round_trip;
          prop_part_number_markers_round_trip;
          prop_upload_of_strings_preserves_valid_identifiers;
          prop_multipart_parts_preserve_known_checksum_and_size;
          prop_complete_parts_accept_matching_non_negative_object_size;
        ] );
    ( "unit:awskit-s3:domain:regression",
      [
        Alcotest.test_case "bucket boundaries" `Quick test_bucket_boundaries;
        Alcotest.test_case "object key prefix delimiter boundaries" `Quick
          test_object_key_prefix_delimiter_boundaries;
        Alcotest.test_case "account header checksum boundaries" `Quick
          test_account_header_and_checksum_boundaries;
        Alcotest.test_case "object identity boundaries" `Quick
          test_object_identity_boundaries;
        Alcotest.test_case "metadata collection boundaries" `Quick
          test_metadata_collection_boundaries;
        Alcotest.test_case "range boundaries" `Quick test_range_boundaries;
        Alcotest.test_case "tag boundaries" `Quick test_tag_boundaries;
        Alcotest.test_case "multipart boundaries" `Quick
          test_multipart_boundaries;
        Alcotest.test_case "multipart option boundaries" `Quick
          test_multipart_option_boundaries;
        Alcotest.test_case "delete-many object boundaries" `Quick
          test_delete_many_object_boundaries;
      ] );
  ]

let () = Alcotest.run "awskit-s3-domain-pbt" suite
