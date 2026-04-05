(** Integration tests for Lwt-Unix runtime: connection creation. *)

open Base

let test_connection_roundtrip () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn =
    Awskit_lwt_unix.create ~scheme:`Http ~region:"eu-west-1" ~credentials:c
      ~clock ~endpoint:"localhost" ~port:9000 ()
  in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Awskit_lwt_unix.Runtime.region conn);
  Alcotest.(check (option string))
    "endpoint" (Some "localhost")
    (Awskit_lwt_unix.Runtime.endpoint_host conn);
  Alcotest.(check (option int))
    "port" (Some 9000)
    (Awskit_lwt_unix.Runtime.endpoint_port conn)

let test_connection_defaults () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn =
    Awskit_lwt_unix.create ~region:"us-east-1" ~credentials:c ~clock ()
  in
  Alcotest.(check (option string))
    "no endpoint" None
    (Awskit_lwt_unix.Runtime.endpoint_host conn);
  Alcotest.(check (option int))
    "no port" None
    (Awskit_lwt_unix.Runtime.endpoint_port conn)

let suite () =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
      ] );
  ]
