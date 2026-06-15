let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()

let region = Awskit.Region.of_string_exn "us-east-1"

let test_connection env () =
  Eio.Switch.run @@ fun sw ->
  let conn =
    Awskit_s3_eio.create ~sw ~env ~https:Awskit_eio.http_only ~region
      ~credentials ()
  in
  Alcotest.(check string)
    "region" "us-east-1"
    (Awskit.Region.to_string (Awskit_s3_eio.Runtime.region conn))

let suite env =
  [
    ("connection", [ Alcotest.test_case "create" `Quick (test_connection env) ]);
  ]
