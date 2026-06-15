module S3 = Awskit_s3_lwt.Make (Cohttp_lwt_unix.Client)

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()

let region = "eu-west-1"

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let check_endpoint_config conn =
  match
    Awskit_s3.Endpoint_resolver.endpoint
      (S3.Runtime.s3_endpoint_config conn)
      ~region:(S3.Runtime.region conn)
  with
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
  | Ok endpoint ->
      Alcotest.(check string)
        "endpoint" "http://localhost:9000"
        (Awskit.Endpoint.to_url_prefix endpoint)

let test_connection_roundtrip () =
  let conn =
    S3.create ~region ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~endpoint:"http://localhost:9000" ()
    |> conn_or_fail
  in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Awskit.Region.to_string (S3.Runtime.region conn));
  check_endpoint_config conn

let test_create_rejects_invalid_region_string () =
  S3.create ~region:"" ~credentials ~clock:(fun () -> Ptime.epoch) ()
  |> expect_validation "invalid region"

let test_create_rejects_invalid_endpoint_string () =
  S3.create ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~endpoint:"http://localhost:9000/path" ()
  |> expect_validation "invalid endpoint"

let suite =
  [
    ( "connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "rejects invalid region string" `Quick
          test_create_rejects_invalid_region_string;
        Alcotest.test_case "rejects invalid endpoint string" `Quick
          test_create_rejects_invalid_endpoint_string;
      ] );
  ]
