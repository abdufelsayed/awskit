let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()

let region = Awskit.Region.of_string_exn "us-east-1"

let test_connection () =
  match
    Awskit_s3_lwt_unix.create ~region ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ()
  with
  | Error error -> Alcotest.failf "%a" Awskit_s3.Error.pp error
  | Ok conn ->
      Alcotest.(check string)
        "region" "us-east-1"
        (Awskit.Region.to_string (Awskit_s3_lwt_unix.Runtime.region conn))

let suite () =
  [ ("connection", [ Alcotest.test_case "create" `Quick test_connection ]) ]
