module S3 = Awskit_s3_lwt.Make (Cohttp_lwt_unix.Client)

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()

let region = Awskit.Region.of_string_exn "eu-west-1"

let test_connection_roundtrip () =
  let conn = S3.create ~region ~credentials ~clock:(fun () -> Ptime.epoch) () in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Awskit.Region.to_string (S3.Runtime.region conn))

let suite =
  [
    ( "connection",
      [ Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip ] );
  ]
