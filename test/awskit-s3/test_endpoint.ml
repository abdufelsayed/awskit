open Awskit_s3
open Awskit_s3_test

let test_endpoint_resolution () =
  let result =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:"my-bucket" ~key:"dir/file.txt" ()
    |> ok_or_fail "presigned default endpoint"
  in
  Alcotest.(check bool)
    "virtual-hosted default" true
    (String.starts_with
       ~prefix:"https://my-bucket.s3.us-east-1.amazonaws.com/dir/file.txt"
       result.url);
  let dotted =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~bucket:"my.bucket" ~key:"file.txt" ()
    |> ok_or_fail "presigned dotted bucket"
  in
  Alcotest.(check bool)
    "dotted bucket path-style" true
    (String.starts_with
       ~prefix:"https://s3.us-east-1.amazonaws.com/my.bucket/file.txt"
       dotted.url);
  let result =
    Presigned.get_object ~region:"us-east-1" ~credentials:creds ~now:test_time
      ~endpoint:"http://localhost:9000" ~addressing_style:`Path
      ~bucket:"my-bucket" ~key:"dir/file.txt" ()
    |> ok_or_fail "presigned endpoint override"
  in
  Alcotest.(check bool)
    "endpoint override path-style" true
    (String.starts_with ~prefix:"http://localhost:9000/my-bucket/dir/file.txt"
       result.url)

let test_endpoint_variants () =
  let dualstack =
    Presigned.get_object ~region:"eu-west-1" ~credentials:creds ~now:test_time
      ~endpoint_variant:`Dualstack ~bucket:"bucket" ~key:"file.txt" ()
    |> ok_or_fail "dualstack endpoint"
  in
  Alcotest.(check bool)
    "dualstack" true
    (String.starts_with
       ~prefix:"https://bucket.s3.dualstack.eu-west-1.amazonaws.com/file.txt"
       dualstack.url);
  let accelerate =
    Presigned.get_object ~region:"us-west-2" ~credentials:creds ~now:test_time
      ~endpoint_variant:`Accelerate_dualstack ~bucket:"bucket" ~key:"file.txt"
      ()
    |> ok_or_fail "accelerate endpoint"
  in
  Alcotest.(check bool)
    "accelerate" true
    (String.starts_with
       ~prefix:"https://bucket.s3-accelerate.dualstack.amazonaws.com/file.txt"
       accelerate.url)

let test_endpoint_config_accepts_string_endpoint () =
  let check_endpoint label config =
    let endpoint =
      Endpoint_resolver.endpoint config
        ~region:(Region.of_string_exn "us-east-1")
      |> ok_or_fail label
    in
    Alcotest.(check string)
      label "http://localhost:9000"
      (Endpoint.to_url_prefix endpoint)
  in
  check_endpoint "resolver endpoint string"
    (Endpoint_resolver.create ~endpoint:"http://localhost:9000" ());
  check_endpoint "top-level endpoint string"
    (endpoint_config ~endpoint:"http://localhost:9000" ())

let suite =
  [
    ( "endpoint",
      [
        Alcotest.test_case "endpoint resolution" `Quick test_endpoint_resolution;
        Alcotest.test_case "endpoint variants" `Quick test_endpoint_variants;
        Alcotest.test_case "endpoint config accepts string endpoint" `Quick
          test_endpoint_config_accepts_string_endpoint;
      ] );
  ]
