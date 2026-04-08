(** Integration tests for Eio runtime: connection creation. *)

open Base

let test_connection_roundtrip env =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let endpoint = Awskit.Endpoint.http ~host:"localhost" ~port:9000 () in
  let conn =
    Awskit_eio.create ~env ~region:"eu-west-1" ~credentials:c ~clock ~endpoint
      ()
  in
  Alcotest.(check string) "region" "eu-west-1" (Awskit_eio.Runtime.region conn);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map
       Awskit_eio.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

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
    (Option.map
       Awskit_eio.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

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
