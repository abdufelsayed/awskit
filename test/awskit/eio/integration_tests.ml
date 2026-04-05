(** Integration tests for Eio runtime: connection creation. *)

open Base

let test_connection_roundtrip env =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn =
    Awskit_eio.create ~env ~region:"eu-west-1" ~credentials:c ~clock
      ~endpoint:"localhost" ~port:9000 ()
  in
  Alcotest.(check string) "region" "eu-west-1" (Awskit_eio.Runtime.region conn);
  Alcotest.(check (option string))
    "endpoint" (Some "localhost")
    (Awskit_eio.Runtime.endpoint_host conn);
  Alcotest.(check (option int))
    "port" (Some 9000)
    (Awskit_eio.Runtime.endpoint_port conn)

let test_connection_defaults env =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn =
    Awskit_eio.create ~env ~region:"us-east-1" ~credentials:c ~clock ()
  in
  Alcotest.(check (option string))
    "no endpoint" None
    (Awskit_eio.Runtime.endpoint_host conn);
  Alcotest.(check (option int))
    "no port" None
    (Awskit_eio.Runtime.endpoint_port conn)

let suite env =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick (fun () ->
            test_connection_roundtrip env);
        Alcotest.test_case "defaults" `Quick (fun () ->
            test_connection_defaults env);
      ] );
  ]
