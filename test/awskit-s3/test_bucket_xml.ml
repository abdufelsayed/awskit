open Awskit_s3
open Awskit_s3_test

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let is_decode_error error =
  match Awskit.Error.kind error with Decode _ -> true | _ -> false

let qcheck_seed = 0xA5111
let to_alcotest = Awskit_test.Qcheck.to_alcotest ~seed:qcheck_seed
let chars_of_string value = List.init (String.length value) (String.get value)
let gen_from_chars chars = QCheck.Gen.oneof_list (chars_of_string chars)

let gen_string ~min ~max ~chars =
  let open QCheck.Gen in
  string_size ~gen:(gen_from_chars chars) (int_range min max)

let tagging_xml tags =
  let tag_xml =
    tags
    |> List.map (fun (key, value) ->
        Fmt.str
          "<Tag><UnknownTagChild>ignored</UnknownTagChild><Key>%s</Key><Value>%s</Value></Tag>"
          key value)
    |> String.concat ""
  in
  Fmt.str
    "<Tagging><UnknownRootChild>ignored</UnknownRootChild><TagSet>%s</TagSet></Tagging>"
    tag_xml

let tagging_result_from_xml body =
  let conn = Recording_runtime.connect [ response 200 body ] in
  Recording_s3.Bucket.Tagging.get conn ~bucket:(bucket_name "my-bucket") ()

let generated_tagging_tags_gen =
  let open QCheck.Gen in
  let value_gen =
    gen_string ~min:0 ~max:12
      ~chars:
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
         +-=._:/@"
  in
  let* count = int_range 0 10 in
  let* values = list_size (return count) value_gen in
  return
    (List.mapi (fun index value -> (Fmt.str "key-%02d" index, value)) values)

let prop_bucket_tagging_decodes_generated_xml =
  QCheck.Test.make ~count:300
    ~name:"bucket tagging decodes generated XML and ignores unknown children"
    (QCheck.make
       ~print:(fun tags ->
         tags
         |> List.map (fun (key, value) -> Fmt.str "%s=%S" key value)
         |> String.concat ";")
       generated_tagging_tags_gen)
    (fun tags ->
      match tagging_result_from_xml (tagging_xml tags) with
      | Error _ -> false
      | Ok result ->
          let actual =
            result.tags
            |> Tag.Set.to_list
            |> List.map (fun tag -> (Tag.key tag, Tag.value tag))
          in
          List.equal
            (fun (left_key, left_value) (right_key, right_value) ->
              String.equal left_key right_key
              && String.equal left_value right_value)
            tags actual)

let malformed_tagging_xml_gen =
  QCheck.Gen.(
    oneof_list
      [
        "<Tagging><TagSet><Tag><Key></Key><Value>prod</Value></Tag></TagSet></Tagging>";
        "<Tagging><TagSet><Tag><Key>env</Key></Tag></TagSet></Tagging>";
      ])

let prop_bucket_tagging_rejects_malformed_known_fields =
  QCheck.Test.make ~count:50
    ~name:"bucket tagging rejects malformed known fields as decode errors"
    (QCheck.make ~print:Fun.id malformed_tagging_xml_gen) (fun body ->
      match tagging_result_from_xml body with
      | Error error -> is_decode_error error
      | Ok _ -> false)

let test_bucket_config_parse () =
  let versioning =
    {|<VersioningConfiguration><Status>Enabled</Status></VersioningConfiguration>|}
  in
  let tagging =
    {|<Tagging><TagSet><Tag><Key>env</Key><Value>prod</Value></Tag></TagSet></Tagging>|}
  in
  let encryption =
    {|<ServerSideEncryptionConfiguration><Rule><ApplyServerSideEncryptionByDefault><SSEAlgorithm>aws:kms</SSEAlgorithm><KMSMasterKeyID>key-1</KMSMasterKeyID></ApplyServerSideEncryptionByDefault></Rule></ServerSideEncryptionConfiguration>|}
  in
  let cors =
    {|<CORSConfiguration><CORSRule><ID>web</ID><AllowedOrigin>https://example.com</AllowedOrigin><AllowedMethod>GET</AllowedMethod><AllowedHeader>*</AllowedHeader><ExposeHeader>etag</ExposeHeader><MaxAgeSeconds>300</MaxAgeSeconds></CORSRule></CORSConfiguration>|}
  in
  let public_access_block =
    {|<PublicAccessBlockConfiguration><BlockPublicAcls>true</BlockPublicAcls><IgnorePublicAcls>false</IgnorePublicAcls><BlockPublicPolicy>true</BlockPublicPolicy><RestrictPublicBuckets>false</RestrictPublicBuckets></PublicAccessBlockConfiguration>|}
  in
  let ownership =
    {|<OwnershipControls><Rule><ObjectOwnership>BucketOwnerEnforced</ObjectOwnership></Rule></OwnershipControls>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 versioning;
        response 200 tagging;
        response 200 encryption;
        response 200 cors;
        response 200 public_access_block;
        response 200 ownership;
      ]
  in
  let versioning =
    Recording_s3.Bucket.Versioning.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "versioning"
  in
  Alcotest.(check bool)
    "versioning enabled" true
    (versioning.status = Some Bucket.Versioning.Status.Enabled);
  let tagging =
    Recording_s3.Bucket.Tagging.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "tagging"
  in
  Alcotest.(check int) "tag count" 1 (tag_count tagging.tags);
  let encryption =
    Recording_s3.Bucket.Encryption.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "encryption"
  in
  (match encryption.config.rules with
  | [ rule ] ->
      Alcotest.(check (option string))
        "algorithm" (Some "aws:kms")
        (Option.map Bucket.Encryption.Algorithm.to_string rule.sse_algorithm);
      Alcotest.(check (option string))
        "kms key" (Some "key-1") rule.kms_master_key_id;
      Alcotest.(check (option bool)) "bucket key" None rule.bucket_key_enabled;
      Alcotest.(check int)
        "blocked types" 0
        (List.length rule.blocked_encryption_types)
  | _ -> Alcotest.fail "expected one encryption rule");
  let cors =
    Recording_s3.Bucket.Cors.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "cors"
  in
  Alcotest.(check int) "cors rule count" 1 (List.length cors.config.rules);
  let public_access_block =
    Recording_s3.Bucket.Public_access_block.get conn
      ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "public access block"
  in
  Alcotest.(check bool)
    "block public policy" true public_access_block.config.block_public_policy;
  let ownership =
    Recording_s3.Bucket.Ownership_controls.get conn
      ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "ownership"
  in
  Alcotest.(check string)
    "ownership" "BucketOwnerEnforced"
    (Bucket.Ownership_controls.Object_ownership.to_string
       ownership.config.object_ownership)

let test_bucket_encryption_extended_xml () =
  let body =
    {|<ServerSideEncryptionConfiguration><Rule><ApplyServerSideEncryptionByDefault><SSEAlgorithm>aws:kms:dsse</SSEAlgorithm><KMSMasterKeyID>key-1</KMSMasterKeyID></ApplyServerSideEncryptionByDefault><BucketKeyEnabled>true</BucketKeyEnabled><BlockedEncryptionTypes><EncryptionType>SSE-C</EncryptionType></BlockedEncryptionTypes></Rule></ServerSideEncryptionConfiguration>|}
  in
  let conn = Recording_runtime.connect [ response 200 body; response 200 "" ] in
  let parsed =
    Recording_s3.Bucket.Encryption.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "extended encryption parse"
  in
  let config = parsed.config in
  (match config.rules with
  | [ rule ] ->
      Alcotest.(check bool)
        "dsse algorithm" true
        (rule.sse_algorithm = Some Bucket.Encryption.Algorithm.Aws_kms_dsse);
      Alcotest.(check (option string))
        "kms key" (Some "key-1") rule.kms_master_key_id;
      Alcotest.(check (option bool))
        "bucket key" (Some true) rule.bucket_key_enabled;
      Alcotest.(check bool)
        "blocked type" true
        (rule.blocked_encryption_types
        = [ Bucket.Encryption.Blocked_encryption_type.Sse_c ])
  | _ -> Alcotest.fail "expected one encryption rule");
  ignore
    (Recording_s3.Bucket.Encryption.put conn ~bucket:(bucket_name "my-bucket")
       ~config ()
    |> ok_or_fail "extended encryption serialize");
  let body = (Recording_runtime.last_call conn).body in
  Alcotest.(check bool)
    "serializes dsse" true
    (string_contains ~substring:"aws:kms:dsse" body);
  Alcotest.(check bool)
    "serializes bucket key" true
    (string_contains ~substring:"<BucketKeyEnabled>true</BucketKeyEnabled>" body);
  Alcotest.(check bool)
    "serializes blocked type" true
    (string_contains ~substring:"<EncryptionType>SSE-C</EncryptionType>" body)

let test_bucket_encryption_unknown_read_values () =
  let body =
    {|<ServerSideEncryptionConfiguration><Rule><ApplyServerSideEncryptionByDefault><SSEAlgorithm>future-value</SSEAlgorithm></ApplyServerSideEncryptionByDefault></Rule></ServerSideEncryptionConfiguration>|}
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  let result =
    Recording_s3.Bucket.Encryption.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "unknown encryption parse"
  in
  match result.config.rules with
  | [
   { Bucket.Encryption.Rule.sse_algorithm = Some (Unknown "future-value"); _ };
  ] ->
      ()
  | _ -> Alcotest.fail "expected unknown encryption algorithm"

let test_bucket_encryption_unknown_write_rejected () =
  let config =
    {
      Bucket.Encryption.rules =
        [
          {
            Bucket.Encryption.Rule.sse_algorithm =
              Some (Bucket.Encryption.Algorithm.Unknown "future-value");
            kms_master_key_id = None;
            bucket_key_enabled = None;
            blocked_encryption_types = [];
          };
        ];
    }
  in
  let conn = Recording_runtime.connect [ response 200 "" ] in
  match
    Recording_s3.Bucket.Encryption.put conn ~bucket:(bucket_name "my-bucket")
      ~config ()
  with
  | Error error when is_validation_field "sse_algorithm" error -> ()
  | Error error ->
      Alcotest.failf "unexpected validation error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected unknown encryption write rejection"

let test_bucket_forward_compatible_read_enums () =
  let versioning_unknown =
    {|<VersioningConfiguration><Status>FutureStatus</Status></VersioningConfiguration>|}
  in
  let versioning_absent = {|<VersioningConfiguration/>|} in
  let ownership_unknown =
    {|<OwnershipControls><Rule><ObjectOwnership>FutureOwnership</ObjectOwnership></Rule></OwnershipControls>|}
  in
  let conn =
    Recording_runtime.connect
      [
        response 200 versioning_unknown;
        response 200 versioning_absent;
        response 200 ownership_unknown;
      ]
  in
  let versioning =
    Recording_s3.Bucket.Versioning.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "unknown versioning parse"
  in
  Alcotest.(check bool)
    "unknown versioning status" true
    (versioning.status = Some (Bucket.Versioning.Status.Unknown "FutureStatus"));
  let absent =
    Recording_s3.Bucket.Versioning.get conn ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "absent versioning parse"
  in
  Alcotest.(check bool)
    "absent versioning status" true
    (Option.is_none absent.status);
  let ownership =
    Recording_s3.Bucket.Ownership_controls.get conn
      ~bucket:(bucket_name "my-bucket") ()
    |> ok_or_fail "unknown ownership parse"
  in
  Alcotest.(check bool)
    "unknown ownership" true
    (ownership.config.object_ownership
    = Bucket.Ownership_controls.Object_ownership.Unknown "FutureOwnership")

let test_bucket_malformed_booleans_are_decode_errors () =
  let public_access_block =
    {|<PublicAccessBlockConfiguration><BlockPublicAcls>maybe</BlockPublicAcls></PublicAccessBlockConfiguration>|}
  in
  let encryption =
    {|<ServerSideEncryptionConfiguration><Rule><BucketKeyEnabled>maybe</BucketKeyEnabled></Rule></ServerSideEncryptionConfiguration>|}
  in
  let check label call field =
    match call () with
    | Error error when is_decode_error error ->
        let text = Awskit.Error.to_string_hum error in
        Alcotest.(check bool)
          (label ^ " mentions field")
          true
          (string_contains text ~substring:field)
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected decode error" label
  in
  let public_access_conn =
    Recording_runtime.connect [ response 200 public_access_block ]
  in
  check "public access block"
    (fun () ->
      Recording_s3.Bucket.Public_access_block.get public_access_conn
        ~bucket:(bucket_name "my-bucket") ())
    "BlockPublicAcls";
  let encryption_conn = Recording_runtime.connect [ response 200 encryption ] in
  check "bucket encryption"
    (fun () ->
      Recording_s3.Bucket.Encryption.get encryption_conn
        ~bucket:(bucket_name "my-bucket") ())
    "BucketKeyEnabled"

let test_bucket_cors_rejects_malformed_max_age () =
  let cases =
    [
      ( "not-int",
        {|<CORSConfiguration><CORSRule><AllowedOrigin>*</AllowedOrigin><AllowedMethod>GET</AllowedMethod><MaxAgeSeconds>not-int</MaxAgeSeconds></CORSRule></CORSConfiguration>|}
      );
      ( "negative",
        {|<CORSConfiguration><CORSRule><AllowedOrigin>*</AllowedOrigin><AllowedMethod>GET</AllowedMethod><MaxAgeSeconds>-1</MaxAgeSeconds></CORSRule></CORSConfiguration>|}
      );
    ]
  in
  List.iter
    (fun (label, body) ->
      let conn = Recording_runtime.connect [ response 200 body ] in
      match
        Recording_s3.Bucket.Cors.get conn ~bucket:(bucket_name "my-bucket") ()
      with
      | Error error when is_decode_error error ->
          let text = Awskit.Error.to_string_hum error in
          Alcotest.(check bool)
            (label ^ " mentions MaxAgeSeconds")
            true
            (string_contains text ~substring:"MaxAgeSeconds")
      | Error error ->
          Alcotest.failf "%s: unexpected error: %a" label Error.pp error
      | Ok _ -> Alcotest.failf "%s: expected MaxAgeSeconds decode error" label)
    cases

let test_bucket_empty_observed_enum_fields_are_decode_errors () =
  let check label call field =
    match call () with
    | Error error when is_decode_error error ->
        let text = Awskit.Error.to_string_hum error in
        Alcotest.(check bool)
          (label ^ " mentions field")
          true
          (string_contains text ~substring:field)
    | Error error ->
        Alcotest.failf "%s: unexpected error: %a" label Error.pp error
    | Ok _ -> Alcotest.failf "%s: expected decode error" label
  in
  let versioning_conn =
    Recording_runtime.connect
      [
        response 200
          {|<VersioningConfiguration><Status></Status></VersioningConfiguration>|};
      ]
  in
  check "bucket versioning"
    (fun () ->
      Recording_s3.Bucket.Versioning.get versioning_conn
        ~bucket:(bucket_name "my-bucket") ())
    "Status";
  let ownership_conn =
    Recording_runtime.connect
      [
        response 200
          {|<OwnershipControls><Rule><ObjectOwnership></ObjectOwnership></Rule></OwnershipControls>|};
      ]
  in
  check "ownership controls"
    (fun () ->
      Recording_s3.Bucket.Ownership_controls.get ownership_conn
        ~bucket:(bucket_name "my-bucket") ())
    "object ownership"

let test_ownership_controls_missing_rule_mentions_xml_context () =
  let conn =
    Recording_runtime.connect [ response 200 "<OwnershipControls/>" ]
  in
  match
    Recording_s3.Bucket.Ownership_controls.get conn
      ~bucket:(bucket_name "my-bucket") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions OwnershipControls" true
        (string_contains text ~substring:"OwnershipControls")
  | Error error ->
      Alcotest.failf "unexpected ownership controls error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected missing ownership rule decode error"

let test_bucket_observed_unknown_writes_rejected () =
  let versioning_conn = Recording_runtime.connect [ response 200 "" ] in
  (match
     Recording_s3.Bucket.Versioning.put versioning_conn
       ~bucket:(bucket_name "my-bucket")
       ~status:(Bucket.Versioning.Status.Unknown "FutureStatus") ()
   with
  | Error error when is_validation_field "status" error -> ()
  | Error error ->
      Alcotest.failf "unexpected versioning validation error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected versioning unknown write rejection");
  Alcotest.(check int)
    "versioning not sent" 0
    (List.length versioning_conn.calls);
  let ownership_conn = Recording_runtime.connect [ response 200 "" ] in
  let config =
    {
      Bucket.Ownership_controls.object_ownership =
        Bucket.Ownership_controls.Object_ownership.Unknown "FutureOwnership";
    }
  in
  (match
     Recording_s3.Bucket.Ownership_controls.put ownership_conn
       ~bucket:(bucket_name "my-bucket") ~config ()
   with
  | Error error when is_validation_field "object_ownership" error -> ()
  | Error error ->
      Alcotest.failf "unexpected ownership validation error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected ownership unknown write rejection");
  Alcotest.(check int) "ownership not sent" 0 (List.length ownership_conn.calls)

let test_decode_error_mentions_xml_document () =
  let conn = Recording_runtime.connect [ response 200 "<not xml" ] in
  match
    Recording_s3.Bucket.get_location conn ~bucket:(bucket_name "my-bucket") ()
  with
  | Ok _ -> Alcotest.fail "expected decode error"
  | Error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions XML document" true
        (string_contains ~substring:"decoding XML document" text)

let test_decode_root_rejects_wrong_root () =
  let conn =
    Recording_runtime.connect
      [
        response 200 "<NotLocationConstraint>us-east-1</NotLocationConstraint>";
      ]
  in
  match
    Recording_s3.Bucket.get_location conn ~bucket:(bucket_name "my-bucket") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions expected root" true
        (string_contains ~substring:"expected LocationConstraint" text)
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected wrong-root decode error"

let test_bucket_tagging_rejects_invalid_tag_xml_as_decode_error () =
  let body =
    "<Tagging><TagSet><Tag><Key></Key><Value>prod</Value></Tag></TagSet></Tagging>"
  in
  let conn = Recording_runtime.connect [ response 200 body ] in
  match
    Recording_s3.Bucket.Tagging.get conn ~bucket:(bucket_name "my-bucket") ()
  with
  | Error error when is_decode_error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions tag key" true
        (string_contains text ~substring:"tag key")
  | Error error -> Alcotest.failf "unexpected error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected invalid tag XML decode error"

let suite =
  [
    ( "pbt:bucket-xml",
      List.map to_alcotest
        [
          prop_bucket_tagging_decodes_generated_xml;
          prop_bucket_tagging_rejects_malformed_known_fields;
        ] );
    ( "bucket xml",
      [
        Alcotest.test_case "bucket config parse" `Quick test_bucket_config_parse;
        Alcotest.test_case "bucket encryption extended xml" `Quick
          test_bucket_encryption_extended_xml;
        Alcotest.test_case "bucket encryption unknown read values" `Quick
          test_bucket_encryption_unknown_read_values;
        Alcotest.test_case "bucket encryption unknown write rejected" `Quick
          test_bucket_encryption_unknown_write_rejected;
        Alcotest.test_case "bucket forward compatible read enums" `Quick
          test_bucket_forward_compatible_read_enums;
        Alcotest.test_case "bucket malformed booleans are decode errors" `Quick
          test_bucket_malformed_booleans_are_decode_errors;
        Alcotest.test_case "bucket cors rejects malformed max age" `Quick
          test_bucket_cors_rejects_malformed_max_age;
        Alcotest.test_case "bucket empty observed enum fields are decode errors"
          `Quick test_bucket_empty_observed_enum_fields_are_decode_errors;
        Alcotest.test_case "ownership missing rule mentions xml context" `Quick
          test_ownership_controls_missing_rule_mentions_xml_context;
        Alcotest.test_case "bucket observed unknown writes rejected" `Quick
          test_bucket_observed_unknown_writes_rejected;
        Alcotest.test_case "decode error mentions XML document" `Quick
          test_decode_error_mentions_xml_document;
        Alcotest.test_case "decode root rejects wrong root" `Quick
          test_decode_root_rejects_wrong_root;
        Alcotest.test_case "bucket tagging rejects invalid tag xml as decode"
          `Quick test_bucket_tagging_rejects_invalid_tag_xml_as_decode_error;
      ] );
  ]
