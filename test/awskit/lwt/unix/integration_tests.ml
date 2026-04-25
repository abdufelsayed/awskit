(** Integration tests for Lwt-Unix runtime: connection creation. *)

open Base

let test_connection_roundtrip () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let endpoint = Awskit.Endpoint.http ~host:"localhost" ~port:9000 () in
  let conn =
    match
      Awskit_lwt_unix.create ~region:"eu-west-1" ~credentials:c ~clock ~endpoint
        ()
    with
    | Ok conn -> conn
    | Error e -> Fmt.failwith "%a" Awskit.Error.pp_base e
  in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Awskit_lwt_unix.Runtime.region conn);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map
       Awskit_lwt_unix.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults () =
  let c =
    Awskit.Credentials.make ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let conn =
    match
      Awskit_lwt_unix.create ~region:"us-east-1" ~credentials:c ~clock ()
    with
    | Ok conn -> conn
    | Error e -> Fmt.failwith "%a" Awskit.Error.pp_base e
  in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map
       Awskit_lwt_unix.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let suite () =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
      ] );
  ]
