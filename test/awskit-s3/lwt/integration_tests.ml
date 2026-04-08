(** Integration tests for the generic S3 + Lwt adapter layer. *)

open Base
module S3 = Awskit_s3_lwt.Make (Cohttp_lwt_unix.Client)

let test_connection_roundtrip () =
  let c =
    Awskit_s3.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let endpoint = Awskit_s3.Endpoint.http ~host:"localhost" ~port:9000 () in
  let conn = S3.create ~region:"eu-west-1" ~credentials:c ~clock ~endpoint () in
  Alcotest.(check string) "region" "eu-west-1" (S3.Runtime.region conn);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map S3.Runtime.(endpoint conn) ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults () =
  let c =
    Awskit_s3.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn = S3.create ~region:"us-east-1" ~credentials:c ~clock () in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map S3.Runtime.(endpoint conn) ~f:Awskit.Endpoint.to_url_prefix)

let suite =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
      ] );
  ]
