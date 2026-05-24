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

let eio_download_body ~max_response_body_bytes body =
  {
    Awskit_eio__Runtime.body = Cohttp_eio.Body.of_string body;
    max_response_body_bytes;
  }

let expect_body_limit label expected = function
  | Error (Awskit.Error.Body { limit = Some limit; _ }) ->
      Alcotest.(check int64) label expected limit
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body limit error" label

let test_discard_download_body_enforces_limit _env =
  eio_download_body ~max_response_body_bytes:3 "abcdef"
  |> Awskit_eio__Runtime.discard_download_body
  |> expect_body_limit "discard limit" 3L

let test_with_download_body_drain_enforces_limit _env =
  let body = eio_download_body ~max_response_body_bytes:3 "abcdef" in
  Awskit_eio__Runtime.with_download_body body ~consume:(fun _ -> Ok ())
  |> expect_body_limit "scoped drain limit" 3L

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
        Alcotest.test_case "discard body limit" `Quick (fun () ->
            test_discard_download_body_enforces_limit env);
        Alcotest.test_case "scoped drain body limit" `Quick (fun () ->
            test_with_download_body_drain_enforces_limit env);
      ] );
  ]
