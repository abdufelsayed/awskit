open Awskit_s3

let expect_ok label = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "%s: unexpected error: %s" label
        (Awskit.Error.to_string_hum error)

let expect_error_field field = function
  | Ok _ -> Alcotest.failf "expected validation error for %s" field
  | Error error ->
      Alcotest.(check (option string))
        ("validation field " ^ field)
        (Some field)
        (Awskit.Error.validation_field error)

let repeat_char count char = String.init count (fun _ -> char)

let repeat_string count value =
  List.init count (fun _ -> value) |> String.concat ""

let is_ok = function Ok _ -> true | Error _ -> false
let is_error = function Ok _ -> false | Error _ -> true
let chars_of_string value = List.init (String.length value) (String.get value)
let gen_from_chars chars = QCheck.Gen.oneof_list (chars_of_string chars)

let gen_string ~min ~max ~chars =
  let open QCheck.Gen in
  string_size ~gen:(gen_from_chars chars) (int_range min max)

let gen_fixed_string ~len ~chars =
  QCheck.Gen.string_size ~gen:(gen_from_chars chars) (QCheck.Gen.return len)

let digit_chars = "0123456789"
let lower_digit_chars = "abcdefghijklmnopqrstuvwxyz0123456789"

let header_value_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_/.;= "

let tag_chars =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 +-_=.:/@"

let qcheck_seed = 0xA5111
let to_alcotest = Awskit_test.Qcheck.to_alcotest ~seed:qcheck_seed
let qstring gen = QCheck.make ~print:(fun value -> value) gen

let equal_string_pair (left_key, left_value) (right_key, right_value) =
  String.equal left_key right_key && String.equal left_value right_value

let valid_bucket_gen =
  let open QCheck.Gen in
  let* len = int_range 3 63 in
  gen_fixed_string ~len ~chars:lower_digit_chars

let bucket_with_dot_dash_pair_gen =
  let open QCheck.Gen in
  let* prefix = gen_string ~min:0 ~max:20 ~chars:lower_digit_chars in
  let* suffix = gen_string ~min:0 ~max:20 ~chars:lower_digit_chars in
  oneof_list
    [ "a" ^ prefix ^ ".-" ^ suffix ^ "z"; "a" ^ prefix ^ "-." ^ suffix ^ "z" ]

let uppercase_bucket_gen =
  let open QCheck.Gen in
  let* rest = gen_string ~min:2 ~max:62 ~chars:lower_digit_chars in
  return ("A" ^ rest)

let adjacent_dot_bucket_gen =
  let open QCheck.Gen in
  let* middle = gen_string ~min:0 ~max:58 ~chars:lower_digit_chars in
  return ("a.." ^ middle ^ "z")

let ipv4_bucket_gen =
  let open QCheck.Gen in
  let* a = int_range 0 255 in
  let* b = int_range 0 255 in
  let* c = int_range 0 255 in
  let* d = int_range 0 255 in
  return (Printf.sprintf "%d.%d.%d.%d" a b c d)

let object_key_gen = gen_string ~min:1 ~max:128 ~chars:header_value_chars
let object_prefix_gen = gen_string ~min:1 ~max:128 ~chars:header_value_chars
let delimiter_gen = gen_string ~min:1 ~max:8 ~chars:header_value_chars
let account_id_gen = gen_fixed_string ~len:12 ~chars:digit_chars
let invalid_short_account_id_gen = gen_string ~min:0 ~max:11 ~chars:digit_chars
let header_value_gen = gen_string ~min:1 ~max:64 ~chars:header_value_chars
let tag_key_gen = gen_string ~min:1 ~max:64 ~chars:lower_digit_chars
let tag_value_gen = gen_string ~min:0 ~max:64 ~chars:tag_chars

let metadata_entries_gen =
  let open QCheck.Gen in
  let* count = int_range 0 20 in
  let* values = list_size (return count) header_value_gen in
  return
    (List.mapi (fun index value -> (Printf.sprintf "k%d" index, value)) values)

let prop_valid_bucket_names_round_trip =
  QCheck.Test.make ~count:1000 ~name:"valid bucket names round trip"
    (qstring valid_bucket_gen) (fun value ->
      match Bucket_name.of_string value with
      | Ok bucket -> String.equal (Bucket_name.to_string bucket) value
      | Error _ -> false)

let prop_dot_dash_bucket_pairs_are_general_names =
  QCheck.Test.make ~count:500
    ~name:"dot-hyphen and hyphen-dot buckets are general names"
    (qstring bucket_with_dot_dash_pair_gen) (fun value ->
      is_ok (Bucket_name.of_string value))

let prop_uppercase_bucket_names_fail =
  QCheck.Test.make ~count:500 ~name:"uppercase bucket names fail"
    (qstring uppercase_bucket_gen) (fun value ->
      is_error (Bucket_name.of_string value))

let prop_adjacent_dot_bucket_names_fail =
  QCheck.Test.make ~count:500 ~name:"adjacent-dot bucket names fail"
    (qstring adjacent_dot_bucket_gen) (fun value ->
      is_error (Bucket_name.of_string value))

let prop_ipv4_bucket_names_fail =
  QCheck.Test.make ~count:500 ~name:"IPv4-shaped bucket names fail"
    (qstring ipv4_bucket_gen) (fun value ->
      is_error (Bucket_name.of_string value))

let prop_object_keys_round_trip =
  QCheck.Test.make ~count:1000 ~name:"object keys round trip"
    (qstring object_key_gen) (fun value ->
      match Object_key.of_string value with
      | Ok key -> String.equal (Object_key.to_string key) value
      | Error _ -> false)

let prop_object_prefixes_round_trip =
  QCheck.Test.make ~count:500 ~name:"object prefixes round trip"
    (qstring object_prefix_gen) (fun value ->
      match Object_key.Prefix.of_string value with
      | Ok prefix -> String.equal (Object_key.Prefix.to_string prefix) value
      | Error _ -> false)

let prop_delimiters_round_trip =
  QCheck.Test.make ~count:500 ~name:"delimiters round trip"
    (qstring delimiter_gen) (fun value ->
      match Object_key.Delimiter.of_string value with
      | Ok delimiter ->
          String.equal (Object_key.Delimiter.to_string delimiter) value
      | Error _ -> false)

let prop_account_ids_round_trip =
  QCheck.Test.make ~count:500 ~name:"account ids round trip"
    (qstring account_id_gen) (fun value ->
      match Account_id.of_string value with
      | Ok account -> String.equal (Account_id.to_string account) value
      | Error _ -> false)

let prop_short_account_ids_fail =
  QCheck.Test.make ~count:500 ~name:"short account ids fail"
    (qstring invalid_short_account_id_gen) (fun value ->
      is_error (Account_id.of_string value))

let prop_metadata_round_trips =
  QCheck.Test.make ~count:500 ~name:"metadata preserves order"
    (QCheck.make
       ~print:(fun entries ->
         entries
         |> List.map (fun (key, value) -> Printf.sprintf "%s=%S" key value)
         |> String.concat ";")
       metadata_entries_gen)
    (fun entries ->
      match Metadata.of_list entries with
      | Ok metadata ->
          List.equal equal_string_pair (Metadata.to_list metadata) entries
      | Error _ -> false)

let prop_metadata_duplicate_keys_fail_case_insensitively =
  QCheck.Test.make ~count:500
    ~name:"metadata duplicate keys fail case-insensitively"
    (qstring header_value_gen) (fun value ->
      is_error (Metadata.of_list [ ("Trace", value); ("trace", value) ]))

let prop_tags_round_trip =
  QCheck.Test.make ~count:500 ~name:"tags round trip"
    QCheck.(pair (make tag_key_gen) (make tag_value_gen))
    (fun (key, value) ->
      match Tag.create ~key ~value with
      | Ok tag ->
          String.equal (Tag.key tag) key && String.equal (Tag.value tag) value
      | Error _ -> false)

let prop_tag_set_rejects_duplicate_keys =
  QCheck.Test.make ~count:500 ~name:"tag set rejects duplicate keys"
    QCheck.(pair (make tag_key_gen) (make tag_value_gen))
    (fun (key, value) ->
      match (Tag.create ~key ~value, Tag.create ~key ~value:(value ^ "x")) with
      | Ok left, Ok right -> is_error (Tag.Set.of_list [ left; right ])
      | _ -> false)

let test_bucket_name_validation () =
  let bucket = expect_ok "bucket" (Bucket_name.of_string "example-bucket") in
  Alcotest.(check string)
    "bucket round trips" "example-bucket"
    (Bucket_name.to_string bucket);
  ignore (expect_ok "dot-hyphen bucket" (Bucket_name.of_string "a-.b"));
  ignore (expect_ok "hyphen-dot bucket" (Bucket_name.of_string "a.-b"));
  expect_error_field "bucket" (Bucket_name.of_string "ab");
  expect_error_field "bucket" (Bucket_name.of_string "Example");
  expect_error_field "bucket" (Bucket_name.of_string "192.168.0.1");
  expect_error_field "bucket" (Bucket_name.of_string "xn--reserved");
  expect_error_field "bucket" (Bucket_name.of_string "bucket--x-s3")

let test_object_key_prefix_and_delimiter () =
  let key = expect_ok "key" (Object_key.of_string "logs/2026/06/file.txt") in
  Alcotest.(check string)
    "key round trips" "logs/2026/06/file.txt" (Object_key.to_string key);
  let newline_key =
    expect_ok "newline key" (Object_key.of_string "line\nkey")
  in
  Alcotest.(check string)
    "newline key round trips" "line\nkey"
    (Object_key.to_string newline_key);
  expect_error_field "key" (Object_key.of_string "");
  expect_error_field "key" (Object_key.of_string (repeat_char 1025 'a'));
  expect_error_field "key" (Object_key.of_string "\xC0\x80");
  expect_error_field "prefix" (Object_key.Prefix.of_string "");
  let prefix = expect_ok "prefix" (Object_key.Prefix.of_string "logs/") in
  Alcotest.(check string)
    "prefix round trips" "logs/"
    (Object_key.Prefix.to_string prefix);
  expect_error_field "prefix" (Object_key.Prefix.of_string "\xFF");
  let delimiter = expect_ok "delimiter" (Object_key.Delimiter.of_string "/") in
  Alcotest.(check string)
    "delimiter round trips" "/"
    (Object_key.Delimiter.to_string delimiter);
  expect_error_field "delimiter" (Object_key.Delimiter.of_string "")

let test_account_content_and_header_values () =
  let owner = expect_ok "account" (Account_id.of_string "123456789012") in
  Alcotest.(check string)
    "account round trips" "123456789012"
    (Account_id.to_string owner);
  expect_error_field "account_id" (Account_id.of_string "123");
  expect_error_field "account_id" (Account_id.of_string "12345678901x");
  let content_type =
    expect_ok "content type"
      (Content_type.of_string "text/plain; charset=utf-8")
  in
  Alcotest.(check string)
    "content type preserves spelling" "text/plain; charset=utf-8"
    (Content_type.to_string content_type);
  expect_error_field "content_type" (Content_type.of_string "");
  expect_error_field "content_type" (Content_type.of_string "text/plain\r\n");
  let header =
    expect_ok "header value"
      (Header_value.of_string ~field:"cache-control" "max-age=60")
  in
  Alcotest.(check string)
    "header round trips" "max-age=60"
    (Header_value.to_string header);
  expect_error_field "cache-control"
    (Header_value.of_string ~field:"cache-control" "")

let test_checksum_values_validate_header_payloads () =
  let checksum =
    expect_ok "checksum value"
      (Object.Checksum.value ~algorithm:Object.Checksum.Algorithm.Sha256
         ~value:"provided-sha256")
  in
  Alcotest.(check string)
    "checksum round trips" "provided-sha256" checksum.value;
  expect_error_field "checksum_value"
    (Object.Checksum.value ~algorithm:Object.Checksum.Algorithm.Sha256 ~value:"");
  expect_error_field "checksum_value"
    (Object.Checksum.value ~algorithm:Object.Checksum.Algorithm.Sha256
       ~value:"bad\nchecksum")

let test_storage_class_values () =
  let cases =
    [
      ("STANDARD", Storage_class.Standard);
      ("REDUCED_REDUNDANCY", Storage_class.Reduced_redundancy);
      ("STANDARD_IA", Storage_class.Standard_ia);
      ("ONEZONE_IA", Storage_class.Onezone_ia);
      ("INTELLIGENT_TIERING", Storage_class.Intelligent_tiering);
      ("GLACIER", Storage_class.Glacier);
      ("GLACIER_IR", Storage_class.Glacier_ir);
      ("DEEP_ARCHIVE", Storage_class.Deep_archive);
      ("OUTPOSTS", Storage_class.Outposts);
      ("SNOW", Storage_class.Snow);
      ("EXPRESS_ONEZONE", Storage_class.Express_onezone);
      ("FSX_OPENZFS", Storage_class.Fsx_openzfs);
      ("FSX_ONTAP", Storage_class.Fsx_ontap);
    ]
  in
  List.iter
    (fun (wire, storage_class) ->
      Alcotest.(check string)
        (wire ^ " render") wire
        (Storage_class.to_string storage_class);
      Alcotest.(check bool)
        (wire ^ " parse") true
        (Storage_class.of_string wire = storage_class))
    cases;
  let unknown = Storage_class.of_string "FUTURE_CLASS" in
  Alcotest.(check string)
    "unknown storage class round trips" "FUTURE_CLASS"
    (Storage_class.to_string unknown);
  Alcotest.(check bool)
    "unknown constructor" true
    (unknown = Storage_class.Unknown "FUTURE_CLASS")

let test_metadata_collection () =
  let metadata =
    expect_ok "metadata"
      (Metadata.of_list [ ("source", "api"); ("trace", "abc123") ])
  in
  Alcotest.(check (list (pair string string)))
    "metadata preserves insertion order"
    [ ("source", "api"); ("trace", "abc123") ]
    (Metadata.to_list metadata);
  let same =
    expect_ok "same metadata"
      (Metadata.of_list [ ("source", "api"); ("trace", "abc123") ])
  in
  let reordered =
    expect_ok "reordered metadata"
      (Metadata.of_list [ ("trace", "abc123"); ("source", "api") ])
  in
  let recased =
    expect_ok "recased metadata"
      (Metadata.of_list [ ("Source", "api"); ("trace", "abc123") ])
  in
  Alcotest.(check bool) "metadata equal" true (Metadata.equal metadata same);
  Alcotest.(check bool)
    "metadata equality is order-sensitive" false
    (Metadata.equal metadata reordered);
  Alcotest.(check bool)
    "metadata equality uses exact key spelling" false
    (Metadata.equal metadata recased);
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
    (Metadata.of_list [ ("source", "bad\nvalue") ])

let test_tag_and_set_validation () =
  let env = expect_ok "tag env" (Tag.create ~key:"env" ~value:"dev") in
  let region = expect_ok "tag region" (Tag.create ~key:"region" ~value:"") in
  let unicode =
    expect_ok "unicode tag"
      (Tag.create ~key:"team-\206\180" ~value:"zone\194\160a")
  in
  Alcotest.(check string) "tag key" "env" (Tag.key env);
  Alcotest.(check string) "tag value" "dev" (Tag.value env);
  let tags = expect_ok "tag set" (Tag.Set.of_list [ env; region ]) in
  Alcotest.(check (list string))
    "tag set preserves insertion order" [ "env"; "region" ]
    (List.map Tag.key (Tag.Set.to_list tags));
  let upper_env =
    expect_ok "case-sensitive tag" (Tag.create ~key:"ENV" ~value:"prod")
  in
  Alcotest.(check string) "unicode tag key" "team-\206\180" (Tag.key unicode);
  ignore (expect_ok "case-sensitive set" (Tag.Set.of_list [ env; upper_env ]));
  ignore
    (expect_ok "astral tag key at UTF-16 limit"
       (Tag.create ~key:(repeat_string 64 "\240\144\144\128") ~value:"v"));
  expect_error_field "tag key" (Tag.create ~key:"" ~value:"dev");
  expect_error_field "tag key"
    (Tag.create ~key:(repeat_char 129 'a') ~value:"dev");
  expect_error_field "tag key"
    (Tag.create ~key:(repeat_string 65 "\240\144\144\128") ~value:"dev");
  expect_error_field "tag key" (Tag.create ~key:"aws:team" ~value:"dev");
  expect_error_field "tag key" (Tag.create ~key:"AWS:team" ~value:"dev");
  expect_error_field "tag key" (Tag.create ~key:"env,team" ~value:"dev");
  expect_error_field "tag value"
    (Tag.create ~key:"env" ~value:(repeat_char 257 'a'));
  expect_error_field "tag value" (Tag.create ~key:"env" ~value:"bad\nvalue");
  expect_error_field "tag value" (Tag.create ~key:"env" ~value:"bad&value");
  expect_error_field "tag value"
    (Tag.create ~key:"env" ~value:"emoji-\240\159\152\128");
  expect_error_field "tag key"
    (Tag.Set.of_list
       [ env; expect_ok "dup tag" (Tag.create ~key:"env" ~value:"prod") ]);
  let eleven_tags =
    List.init 11 (fun index ->
        expect_ok "tag"
          (Tag.create ~key:(Printf.sprintf "k%d" index) ~value:"v"))
  in
  expect_error_field "tags" (Tag.Set.of_list eleven_tags)

let test_multipart_domain_values () =
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
  Alcotest.(check int)
    "first part number" 1
    (Multipart.Part_number.to_int first);
  Alcotest.(check int)
    "last part number" 10_000
    (Multipart.Part_number.to_int last);
  expect_error_field "part_number" (Multipart.Part_number.of_int 0);
  expect_error_field "part_number" (Multipart.Part_number.of_int (-1));
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
    "resume bucket" "multipart-bucket"
    (Bucket_name.to_string (Multipart.Upload.bucket resumed));
  Alcotest.(check string)
    "resume key" "large.bin"
    (Object_key.to_string (Multipart.Upload.key resumed));
  Alcotest.(check string)
    "resume upload id" "upload-1"
    (Multipart.Upload.upload_id resumed |> Multipart.Upload_id.to_string);
  let created = Multipart.Upload.created ~bucket ~key ~upload_id in
  let caller_owned = Multipart.Upload.as_caller_owned created in
  Alcotest.(check string)
    "as_caller_owned preserves identity" "upload-1"
    (Multipart.Upload.upload_id caller_owned |> Multipart.Upload_id.to_string);
  let etag = Object.Etag.of_string_exn "\"etag-1\"" in
  let part = Multipart.Part.create_exn ~part_number:first ~etag ~size:0L () in
  Alcotest.(check int)
    "part stores typed number" 1
    (Multipart.Part.part_number part |> Multipart.Part_number.to_int);
  Alcotest.(check (option int64))
    "part size" (Some 0L) (Multipart.Part.size part);
  expect_error_field "size"
    (Multipart.Part.create ~part_number:first ~etag ~size:(-1L) ());
  expect_error_field "checksum_algorithm"
    (Object.Checksum.value
       ~algorithm:(Object.Checksum.Algorithm.Unknown "FUTURE") ~value:"checksum");
  let response_checksum =
    Object.Checksum.response_value ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:"bad\nchecksum"
  in
  expect_error_field "checksum_value"
    (Object.Put.options ~checksum:response_checksum ());
  expect_error_field "checksum_value"
    (Multipart.Part.create ~checksum:response_checksum ~part_number:first ~etag
       ());
  expect_error_field "checksum_value"
    (Multipart.Upload_part.options ~checksum:response_checksum ());
  expect_error_field "checksum_value"
    (Multipart.Complete.options ~checksum:response_checksum ())

let suite =
  [
    ( "pbt:domain:bucket",
      List.map to_alcotest
        [
          prop_valid_bucket_names_round_trip;
          prop_dot_dash_bucket_pairs_are_general_names;
          prop_uppercase_bucket_names_fail;
          prop_adjacent_dot_bucket_names_fail;
          prop_ipv4_bucket_names_fail;
        ] );
    ( "pbt:domain:key",
      List.map to_alcotest
        [
          prop_object_keys_round_trip;
          prop_object_prefixes_round_trip;
          prop_delimiters_round_trip;
        ] );
    ( "pbt:domain:account-metadata-tags",
      List.map to_alcotest
        [
          prop_account_ids_round_trip;
          prop_short_account_ids_fail;
          prop_metadata_round_trips;
          prop_metadata_duplicate_keys_fail_case_insensitively;
          prop_tags_round_trip;
          prop_tag_set_rejects_duplicate_keys;
        ] );
    ( "domain types",
      [
        Alcotest.test_case "bucket name validation" `Quick
          test_bucket_name_validation;
        Alcotest.test_case "object key prefix and delimiter validation" `Quick
          test_object_key_prefix_and_delimiter;
        Alcotest.test_case "account content and header values" `Quick
          test_account_content_and_header_values;
        Alcotest.test_case "checksum values validate header payloads" `Quick
          test_checksum_values_validate_header_payloads;
        Alcotest.test_case "storage class values" `Quick
          test_storage_class_values;
        Alcotest.test_case "metadata collection validation" `Quick
          test_metadata_collection;
        Alcotest.test_case "tag and set validation" `Quick
          test_tag_and_set_validation;
        Alcotest.test_case "multipart domain values" `Quick
          test_multipart_domain_values;
      ] );
  ]
