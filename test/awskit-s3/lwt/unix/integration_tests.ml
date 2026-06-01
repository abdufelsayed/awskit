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

let test_object_transfer_helpers_exposed () =
  let upload_string :
      Awskit_s3_lwt_unix.t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.options ->
      string ->
      (Awskit_s3.Transfer.result, Awskit_s3.Error.t) result Lwt.t =
    Awskit_s3_lwt_unix.Object.Transfer.upload_string
  in
  let upload_bytes :
      Awskit_s3_lwt_unix.t ->
      bucket:string ->
      key:string ->
      ?options:Awskit_s3.Transfer.options ->
      bytes ->
      (Awskit_s3.Transfer.result, Awskit_s3.Error.t) result Lwt.t =
    Awskit_s3_lwt_unix.Object.Transfer.upload_bytes
  in
  ignore (upload_string, upload_bytes)

let suite () =
  [
    ("connection", [ Alcotest.test_case "create" `Quick test_connection ]);
    ( "object transfer",
      [
        Alcotest.test_case "generic helpers exposed" `Quick
          test_object_transfer_helpers_exposed;
      ] );
  ]
