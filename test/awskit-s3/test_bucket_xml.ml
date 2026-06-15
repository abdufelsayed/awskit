open Awskit_s3
open Awskit_s3_test

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

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
    Recording_s3.Bucket.Versioning.get conn ~bucket:"my-bucket" ()
    |> ok_or_fail "versioning"
  in
  Alcotest.(check bool)
    "versioning enabled" true
    (versioning.status = Some Bucket.Versioning.Status.Enabled);
  let tagging =
    Recording_s3.Bucket.Tagging.get conn ~bucket:"my-bucket" ()
    |> ok_or_fail "tagging"
  in
  Alcotest.(check int) "tag count" 1 (List.length tagging.tags);
  let encryption =
    Recording_s3.Bucket.Encryption.get conn ~bucket:"my-bucket" ()
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
    Recording_s3.Bucket.Cors.get conn ~bucket:"my-bucket" ()
    |> ok_or_fail "cors"
  in
  Alcotest.(check int) "cors rule count" 1 (List.length cors.config.rules);
  let public_access_block =
    Recording_s3.Bucket.Public_access_block.get conn ~bucket:"my-bucket" ()
    |> ok_or_fail "public access block"
  in
  Alcotest.(check bool)
    "block public policy" true public_access_block.config.block_public_policy;
  let ownership =
    Recording_s3.Bucket.Ownership_controls.get conn ~bucket:"my-bucket" ()
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
    Recording_s3.Bucket.Encryption.get conn ~bucket:"my-bucket" ()
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
    (Recording_s3.Bucket.Encryption.put conn ~bucket:"my-bucket" config
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
    Recording_s3.Bucket.Encryption.get conn ~bucket:"my-bucket" ()
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
  match Recording_s3.Bucket.Encryption.put conn ~bucket:"my-bucket" config with
  | Error error when is_validation_field "sse_algorithm" error -> ()
  | Error error ->
      Alcotest.failf "unexpected validation error: %a" Error.pp error
  | Ok _ -> Alcotest.fail "expected unknown encryption write rejection"

let test_decode_error_mentions_xml_document () =
  match Awskit_s3__Bucket_result_xml.parse_location "<not xml" with
  | Ok _ -> Alcotest.fail "expected decode error"
  | Error error ->
      let text = Awskit.Error.to_string_hum error in
      Alcotest.(check bool)
        "mentions XML document" true
        (string_contains ~substring:"decoding XML document" text)

let suite =
  [
    ( "bucket xml",
      [
        Alcotest.test_case "bucket config parse" `Quick test_bucket_config_parse;
        Alcotest.test_case "bucket encryption extended xml" `Quick
          test_bucket_encryption_extended_xml;
        Alcotest.test_case "bucket encryption unknown read values" `Quick
          test_bucket_encryption_unknown_read_values;
        Alcotest.test_case "bucket encryption unknown write rejected" `Quick
          test_bucket_encryption_unknown_write_rejected;
        Alcotest.test_case "decode error mentions XML document" `Quick
          test_decode_error_mentions_xml_document;
      ] );
  ]
