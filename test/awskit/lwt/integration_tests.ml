(** Integration tests for the generic Lwt runtime adapter. *)

open Base
module Aws = Awskit_lwt.Make (Cohttp_lwt_unix.Client)

let test_connection_roundtrip () =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = Awskit.Region.of_string_exn "eu-west-1" in
  let endpoint = Awskit.Endpoint.http_exn ~host:"localhost" ~port:9000 () in
  let conn = Aws.create ~region ~credentials:c ~clock ~endpoint () in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Aws.Runtime.region conn |> Awskit.Region.to_string);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map Aws.Runtime.(endpoint conn) ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults () =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  let conn = Aws.create ~region ~credentials:c ~clock () in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map Aws.Runtime.(endpoint conn) ~f:Awskit.Endpoint.to_url_prefix)

let test_runtime_bodies () =
  let body = Aws.Runtime.string_body "hello" in
  Alcotest.(check int64)
    "content length" 5L
    (Option.value (Aws.Runtime.upload_descriptor body).content_length
       ~default:(-1L))

let suite =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
        Alcotest.test_case "runtime bodies" `Quick test_runtime_bodies;
      ] );
  ]
