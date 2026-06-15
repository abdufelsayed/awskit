let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()

let region = "us-east-1"

let check_endpoint_config conn =
  match
    Awskit_s3.Endpoint_resolver.endpoint
      (Awskit_s3_lwt_unix.Runtime.s3_endpoint_config conn)
      ~region:(Awskit_s3_lwt_unix.Runtime.region conn)
  with
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
  | Ok endpoint ->
      Alcotest.(check string)
        "endpoint" "http://localhost:9000"
        (Awskit.Endpoint.to_url_prefix endpoint)

let test_connection () =
  match
    Awskit_s3_lwt_unix.create ~region ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~endpoint:"http://localhost:9000" ()
  with
  | Error error -> Alcotest.failf "%a" Awskit_s3.Error.pp error
  | Ok conn ->
      Alcotest.(check string)
        "region" "us-east-1"
        (Awskit.Region.to_string (Awskit_s3_lwt_unix.Runtime.region conn));
      check_endpoint_config conn

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let test_create_rejects_invalid_region_string () =
  expect_validation "invalid region"
    (Awskit_s3_lwt_unix.create ~region:"" ~credentials
       ~clock:(fun () -> Ptime.epoch)
       ())

let test_create_rejects_invalid_endpoint_string () =
  expect_validation "invalid endpoint"
    (Awskit_s3_lwt_unix.create ~region:"us-east-1"
       ~endpoint:"http://localhost:9000/path" ~credentials
       ~clock:(fun () -> Ptime.epoch)
       ())

let suite () =
  [
    ( "connection",
      [
        Alcotest.test_case "create" `Quick test_connection;
        Alcotest.test_case "rejects invalid region string" `Quick
          test_create_rejects_invalid_region_string;
        Alcotest.test_case "rejects invalid endpoint string" `Quick
          test_create_rejects_invalid_endpoint_string;
      ] );
  ]
