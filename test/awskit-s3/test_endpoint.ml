open Awskit_s3
open Awskit_s3_test

let test_endpoint_resolution () =
  let result =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "dir/file.txt")
      ()
    |> ok_or_fail "presigned default endpoint"
  in
  Alcotest.(check bool)
    "virtual-hosted default" true
    (String.starts_with
       ~prefix:"https://my-bucket.s3.us-east-1.amazonaws.com/dir/file.txt"
       (Presigned.reveal_url result));
  let dotted =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:(bucket_name "my.bucket") ~key:(object_key "file.txt") ()
    |> ok_or_fail "presigned dotted bucket"
  in
  Alcotest.(check bool)
    "dotted bucket path-style" true
    (String.starts_with
       ~prefix:"https://s3.us-east-1.amazonaws.com/my.bucket/file.txt"
       (Presigned.reveal_url dotted));
  let endpoint_config =
    Endpoint_config.local_plaintext
      ~endpoint:(Awskit.Endpoint.http_exn ~host:"localhost" ~port:9000 ())
      ~signing_region:(Region.of_string_exn "us-east-1")
      ~addressing_style:`Path ()
    |> ok_or_fail "local endpoint config"
  in
  let result =
    Presigned.get_object_with_endpoint_config
      ~region:(Region.of_string_exn "us-east-1")
      ~credentials:creds ~now:test_time ~endpoint_config
      ~bucket:(bucket_name "my-bucket")
      ~key:(object_key "dir/file.txt")
      ()
    |> ok_or_fail "presigned local endpoint"
  in
  Alcotest.(check bool)
    "local endpoint path-style" true
    (String.starts_with ~prefix:"http://localhost:9000/my-bucket/dir/file.txt"
       (Presigned.reveal_url result))

let test_endpoint_variants () =
  let dualstack =
    Presigned.get_object ~region:"eu-west-1" ~credentials:creds ~now:test_time
      ~endpoint_variant:`Dualstack ~bucket:(bucket_name "bucket")
      ~key:(object_key "file.txt") ()
    |> ok_or_fail "dualstack endpoint"
  in
  Alcotest.(check bool)
    "dualstack" true
    (String.starts_with
       ~prefix:"https://bucket.s3.dualstack.eu-west-1.amazonaws.com/file.txt"
       (Presigned.reveal_url dualstack));
  let accelerate =
    Presigned.get_object ~region:"us-west-2" ~credentials:creds ~now:test_time
      ~endpoint_variant:`Accelerate_dualstack ~bucket:(bucket_name "bucket")
      ~key:(object_key "file.txt") ()
    |> ok_or_fail "accelerate endpoint"
  in
  Alcotest.(check bool)
    "accelerate" true
    (String.starts_with
       ~prefix:"https://bucket.s3-accelerate.dualstack.amazonaws.com/file.txt"
       (Presigned.reveal_url accelerate))

let test_endpoint_policy_validation () =
  let region = Region.of_string_exn "us-east-1" in
  let client_region = Region.of_string_exn "eu-west-1" in
  let default_endpoint =
    Endpoint_resolver.endpoint Endpoint_config.default ~region
    |> ok_or_fail "default endpoint"
  in
  Alcotest.(check string)
    "default AWS endpoint" "https://s3.us-east-1.amazonaws.com"
    (Awskit.Endpoint.to_url_prefix default_endpoint);
  let local_config =
    Endpoint_config.local_plaintext
      ~endpoint:(Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:9000 ())
      ~signing_region:region ~addressing_style:`Path ()
    |> ok_or_fail "loopback local plaintext"
  in
  Alcotest.(check string)
    "local endpoint" "http://127.0.0.1:9000"
    (Awskit.Endpoint.to_url_prefix
       (Endpoint_resolver.endpoint local_config ~region
       |> ok_or_fail "local endpoint"));
  let ipv6_local_endpoint =
    Awskit.Endpoint.of_string "http://[::1]:9000"
    |> ok_or_fail "IPv6 loopback endpoint"
  in
  let ipv6_local_config =
    Endpoint_config.local_plaintext ~endpoint:ipv6_local_endpoint
      ~signing_region:region ~addressing_style:`Path ()
    |> ok_or_fail "IPv6 loopback local plaintext"
  in
  Alcotest.(check string)
    "IPv6 local endpoint" "http://[::1]:9000"
    (Awskit.Endpoint.to_url_prefix
       (Endpoint_resolver.endpoint ipv6_local_config ~region
       |> ok_or_fail "IPv6 local endpoint"));
  Alcotest.(check bool)
    "unbracketed IPv6 rejected" true
    (Result.is_error (Awskit.Endpoint.of_string "http://::1:9000"));
  let public_http =
    Endpoint_config.local_plaintext
      ~endpoint:(Awskit.Endpoint.http_exn ~host:"minio.internal" ~port:9000 ())
      ~signing_region:region ~addressing_style:`Path ()
  in
  Alcotest.(check bool)
    "public http rejected" true
    (Result.is_error public_http);
  let fake_loopback =
    Endpoint_config.local_plaintext
      ~endpoint:
        (Awskit.Endpoint.http_exn ~host:"127.0.0.1.evil.example" ~port:9000 ())
      ~signing_region:region ~addressing_style:`Path ()
  in
  Alcotest.(check bool)
    "fake loopback hostname rejected" true
    (Result.is_error fake_loopback);
  let https_local_plaintext =
    Endpoint_config.local_plaintext
      ~endpoint:(Awskit.Endpoint.https_exn ~host:"localhost" ~port:9000 ())
      ~signing_region:region ~addressing_style:`Path ()
  in
  Alcotest.(check bool)
    "local_plaintext requires http" true
    (Result.is_error https_local_plaintext);
  let auto_local_plaintext =
    Endpoint_config.s3_compatible
      ~endpoint:(Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:9000 ())
      ~signing_region:region ~addressing_style:`Auto
      ~tls_policy:`Http_local_only ~feature_policy:`S3_compatible ()
  in
  Alcotest.(check bool)
    "local plaintext rejects auto addressing" true
    (Result.is_error auto_local_plaintext);
  let unsafe_config =
    Endpoint_config.unsafe_plaintext
      ~endpoint:(Awskit.Endpoint.http_exn ~host:"minio.internal" ~port:9000 ())
      ~signing_region:region ~addressing_style:`Path ()
  in
  Alcotest.(check string)
    "unsafe http endpoint" "http://minio.internal:9000"
    (Awskit.Endpoint.to_url_prefix
       (Endpoint_resolver.endpoint unsafe_config ~region
       |> ok_or_fail "unsafe endpoint"));
  Alcotest.(check bool)
    "userinfo rejected" true
    (Result.is_error (Awskit.Endpoint.of_string "https://user@example.com"));
  let dotted_auto =
    Endpoint_resolver.resolve_object_request Endpoint_config.default ~region
      ~bucket:(bucket_name "my.bucket") ~key:(object_key "file")
    |> ok_or_fail "dotted auto"
  in
  Alcotest.(check bool) "dotted auto path-style" true (dotted_auto.style = `Path);
  let dotted_virtual =
    Endpoint_resolver.resolve_object_request
      (Endpoint_config.aws ~addressing_style:`Virtual_hosted ())
      ~region ~bucket:(bucket_name "my.bucket") ~key:(object_key "file")
  in
  Alcotest.(check bool)
    "dotted virtual-hosted rejected" true
    (Result.is_error dotted_virtual);
  let accelerate_path =
    Endpoint_resolver.resolve_object_request
      (Endpoint_config.aws ~addressing_style:`Path ~endpoint_variant:`Accelerate
         ())
      ~region ~bucket:(bucket_name "bucket") ~key:(object_key "file")
  in
  Alcotest.(check bool)
    "accelerate path-style rejected" true
    (Result.is_error accelerate_path);
  let accelerate_dotted =
    Endpoint_resolver.resolve_object_request
      (Endpoint_config.aws ~endpoint_variant:`Accelerate ())
      ~region ~bucket:(bucket_name "my.bucket") ~key:(object_key "file")
  in
  Alcotest.(check bool)
    "accelerate dotted bucket rejected" true
    (Result.is_error accelerate_dotted);
  let compatible =
    Endpoint_config.s3_compatible
      ~endpoint:(Awskit.Endpoint.https_exn ~host:"minio.internal" ())
      ~signing_region:region ~addressing_style:`Path ~tls_policy:`Https_required
      ~feature_policy:`S3_compatible ()
    |> ok_or_fail "s3-compatible endpoint config"
  in
  Alcotest.(check string)
    "signing region" "us-east-1"
    (Awskit.Region.to_string
       (Endpoint_config.signing_region compatible ~client_region))

let suite =
  [
    ( "endpoint",
      [
        Alcotest.test_case "endpoint resolution" `Quick test_endpoint_resolution;
        Alcotest.test_case "endpoint variants" `Quick test_endpoint_variants;
        Alcotest.test_case "endpoint policy validation" `Quick
          test_endpoint_policy_validation;
      ] );
  ]
