(** Integration tests for Eio runtime: connection creation. *)

open Base

let test_connection_roundtrip env =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = Awskit.Region.of_string_exn "eu-west-1" in
  let endpoint = Awskit.Endpoint.http_exn ~host:"localhost" ~port:9000 () in
  let conn =
    Eio.Switch.run @@ fun sw ->
    Awskit_eio.create ~env ~sw ~region ~credentials:c ~clock ~endpoint ()
  in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Awskit_eio.Runtime.region conn |> Awskit.Region.to_string);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map
       Awskit_eio.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults env =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  let conn =
    Eio.Switch.run @@ fun sw ->
    Awskit_eio.create ~env ~sw ~region ~credentials:c ~clock ()
  in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map
       Awskit_eio.Runtime.(endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_runtime_bodies env =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  Eio.Switch.run @@ fun sw ->
  let conn = Awskit_eio.create ~env ~sw ~region ~credentials:c () in
  ignore (Awskit_eio.Runtime.region conn : Awskit.Region.t);
  let body = Awskit_eio.Runtime.string_body "hello" in
  Alcotest.(check int64)
    "content length" 5L
    (Option.value (Awskit_eio.Runtime.upload_descriptor body).content_length
       ~default:(-1L))

let suite env =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick (fun () ->
            test_connection_roundtrip env);
        Alcotest.test_case "defaults" `Quick (fun () ->
            test_connection_defaults env);
        Alcotest.test_case "runtime bodies" `Quick (fun () ->
            test_runtime_bodies env);
      ] );
  ]
