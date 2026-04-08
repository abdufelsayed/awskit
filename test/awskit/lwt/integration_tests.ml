(** Integration tests for the generic Lwt runtime adapter. *)

open Base
module Aws = Awskit_lwt.Make (Cohttp_lwt_unix.Client)

let test_connection_roundtrip () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let endpoint = Awskit.Endpoint.http ~host:"localhost" ~port:9000 () in
  let conn =
    Aws.create ~region:"eu-west-1" ~credentials:c ~clock ~endpoint ()
  in
  Alcotest.(check string) "region" "eu-west-1" (Aws.Runtime.region conn);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map Aws.Runtime.(endpoint conn) ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn = Aws.create ~region:"us-east-1" ~credentials:c ~clock () in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map Aws.Runtime.(endpoint conn) ~f:Awskit.Endpoint.to_url_prefix)

let suite =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
      ] );
  ]
