let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()

let region = "us-east-1"

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
      (Awskit_s3_eio.Runtime.s3_endpoint_config conn)
      ~region:(Awskit_s3_eio.Runtime.region conn)
  with
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
  | Ok endpoint ->
      Alcotest.(check string)
        "endpoint" "http://localhost:9000"
        (Awskit.Endpoint.to_url_prefix endpoint)

let test_connection env () =
  Eio.Switch.run @@ fun sw ->
  let conn =
    Awskit_s3_eio.create ~sw ~env ~https:Awskit_eio.http_only ~region
      ~credentials ~endpoint:"http://localhost:9000" ()
    |> conn_or_fail
  in
  Alcotest.(check string)
    "region" "us-east-1"
    (Awskit.Region.to_string (Awskit_s3_eio.Runtime.region conn));
  check_endpoint_config conn

let test_create_rejects_invalid_region_string env () =
  Eio.Switch.run @@ fun sw ->
  Awskit_s3_eio.create ~sw ~env ~https:Awskit_eio.http_only ~region:""
    ~credentials ()
  |> expect_validation "invalid region"

let test_create_rejects_invalid_endpoint_string env () =
  Eio.Switch.run @@ fun sw ->
  Awskit_s3_eio.create ~sw ~env ~https:Awskit_eio.http_only ~region ~credentials
    ~endpoint:"http://localhost:9000/path" ()
  |> expect_validation "invalid endpoint"

let suite env =
  [
    ( "connection",
      [
        Alcotest.test_case "create" `Quick (test_connection env);
        Alcotest.test_case "rejects invalid region string" `Quick
          (test_create_rejects_invalid_region_string env);
        Alcotest.test_case "rejects invalid endpoint string" `Quick
          (test_create_rejects_invalid_endpoint_string env);
      ] );
  ]
