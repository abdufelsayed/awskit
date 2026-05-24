(** Integration tests for the generic Lwt runtime adapter. *)

open Base
module Aws = Awskit_lwt.Make (Cohttp_lwt_unix.Client)

module Limited_body_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let response_body = ref ""
  let response () = Cohttp.Response.make ~status:`OK ()
  let body () = Cohttp_lwt.Body.of_string !response_body
  let map_context f g ?ctx = g (f ?ctx)

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    Lwt.return (response (), body ())

  let head ?ctx:_ ?headers:_ _uri = Lwt.return (response ())
  let get ?ctx ?headers uri = call ?ctx ?headers `GET uri

  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `DELETE uri

  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `POST uri

  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `PUT uri

  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `PATCH uri

  let set_cache _ = ()

  let post_form ?ctx:_ ?headers:_ ~params:_ _uri =
    Lwt.return (response (), body ())

  let callv ?ctx:_ _uri _requests = Lwt.return (Lwt_stream.of_list [])
end

module LimitedAws = Awskit_lwt.Make (Limited_body_client)

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

let limited_conn ~max_response_body_bytes =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  LimitedAws.create ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~max_response_body_bytes ()

let limited_request =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"localhost" ~path:"/"
      ()
  in
  Awskit.Request.create_exn ~method_:`GET ~target ()

let limited_download_body ~max_response_body_bytes body =
  Limited_body_client.response_body := body;
  let conn = limited_conn ~max_response_body_bytes in
  match
    Lwt_main.run
      (LimitedAws.Runtime.call conn limited_request
         LimitedAws.Runtime.empty_body)
  with
  | Error error ->
      Alcotest.failf "runtime call failed: %a" Awskit.Error.pp error
  | Ok (_, body) -> body

let expect_body_limit label expected = function
  | Error (Awskit.Error.Body { limit = Some limit; _ }) ->
      Alcotest.(check int64) label expected limit
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body limit error" label

let test_discard_download_body_enforces_limit () =
  let body = limited_download_body ~max_response_body_bytes:3 "abcdef" in
  LimitedAws.Runtime.discard_download_body body
  |> Lwt_main.run
  |> expect_body_limit "discard limit" 3L

let test_with_download_body_drain_enforces_limit () =
  let body = limited_download_body ~max_response_body_bytes:3 "abcdef" in
  LimitedAws.Runtime.with_download_body body ~consume:(fun _ ->
      Lwt.return_ok ())
  |> Lwt_main.run
  |> expect_body_limit "scoped drain limit" 3L

let suite =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
        Alcotest.test_case "runtime bodies" `Quick test_runtime_bodies;
        Alcotest.test_case "discard body limit" `Quick
          test_discard_download_body_enforces_limit;
        Alcotest.test_case "scoped drain body limit" `Quick
          test_with_download_body_drain_enforces_limit;
      ] );
  ]
