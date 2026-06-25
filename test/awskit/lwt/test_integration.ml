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
  let drained_chunks = ref []
  let response () = Cohttp.Response.make ~status:`OK ()

  let body () =
    let chunks = ref [ !response_body ] in
    let stream =
      Lwt_stream.from (fun () ->
          match !chunks with
          | [] -> Lwt.return_none
          | chunk :: rest ->
              chunks := rest;
              drained_chunks := chunk :: !drained_chunks;
              Lwt.return_some chunk)
    in
    Cohttp_lwt.Body.of_stream stream

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

module Request_body_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let request_body = ref None
  let response () = Cohttp.Response.make ~status:`OK ()
  let empty_response_body () = Cohttp_lwt.Body.empty
  let map_context f g ?ctx = g (f ?ctx)

  let call ?ctx:_ ?headers:_ ?body ?chunked:_ _meth _uri =
    let body =
      match body with
      | None -> Lwt.return ""
      | Some body -> Cohttp_lwt.Body.to_string body
    in
    Lwt.bind body (fun request_body_value ->
        request_body := Some request_body_value;
        Lwt.return (response (), empty_response_body ()))

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
    Lwt.return (response (), empty_response_body ())

  let callv ?ctx:_ _uri _requests = Lwt.return (Lwt_stream.of_list [])
end

module RequestAws = Awskit_lwt.Make (Request_body_client)

module Failing_transport_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let map_context f g ?ctx = g (f ?ctx)

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    Lwt.fail (Failure "transport exploded")

  let head ?ctx:_ ?headers:_ _uri = Lwt.fail (Failure "transport exploded")
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
    Lwt.fail (Failure "transport exploded")

  let callv ?ctx:_ _uri _requests = Lwt.fail (Failure "transport exploded")
end

module FailingTransportAws = Awskit_lwt.Make (Failing_transport_client)

module Failing_after_request_body_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let map_context f g ?ctx = g (f ?ctx)

  let drain_request_body = function
    | None -> Lwt.return_unit
    | Some body ->
        Lwt.catch
          (fun () -> Lwt.map ignore (Cohttp_lwt.Body.to_string body))
          (fun _exn -> Lwt.return_unit)

  let call ?ctx:_ ?headers:_ ?body ?chunked:_ _meth _uri =
    Lwt.bind (drain_request_body body) (fun () ->
        Lwt.bind (Lwt.pause ()) (fun () ->
            Lwt.fail (Failure "transport exploded after request body")))

  let head ?ctx:_ ?headers:_ _uri =
    Lwt.fail (Failure "transport exploded after request body")

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
    Lwt.fail (Failure "transport exploded after request body")

  let callv ?ctx:_ _uri _requests =
    Lwt.fail (Failure "transport exploded after request body")
end

module FailingAfterRequestBodyAws =
  Awskit_lwt.Make (Failing_after_request_body_client)

module Blocking_transport_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let call_started = ref false
  let call_finalized = ref false
  let map_context f g ?ctx = g (f ?ctx)
  let response () = Cohttp.Response.make ~status:`OK ()

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    call_started := true;
    let blocked, _wake = Lwt.task () in
    Lwt.finalize
      (fun () -> blocked)
      (fun () ->
        call_finalized := true;
        Lwt.return_unit)

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
    Lwt.return (response (), Cohttp_lwt.Body.empty)

  let callv ?ctx:_ _uri _requests = Lwt.return (Lwt_stream.of_list [])
end

module BlockingTransportAws = Awskit_lwt.Make (Blocking_transport_client)

module Blocking_response_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let read_started_count = ref 0
  let read_finalized_count = ref 0
  let response () = Cohttp.Response.make ~status:`OK ()
  let map_context f g ?ctx = g (f ?ctx)

  let body () =
    let stream =
      Lwt_stream.from (fun () ->
          Int.incr read_started_count;
          let blocked, _wake = Lwt.task () in
          Lwt.finalize
            (fun () -> blocked)
            (fun () ->
              Int.incr read_finalized_count;
              Lwt.return_unit))
    in
    Cohttp_lwt.Body.of_stream stream

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

module BlockingResponseAws = Awskit_lwt.Make (Blocking_response_client)

module Early_response_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let response_body = ref ""
  let response () = Cohttp.Response.make ~status:`Forbidden ()
  let map_context f g ?ctx = g (f ?ctx)

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    Lwt.return (response (), Cohttp_lwt.Body.of_string !response_body)

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
    Lwt.return (response (), Cohttp_lwt.Body.of_string !response_body)

  let callv ?ctx:_ _uri _requests = Lwt.return (Lwt_stream.of_list [])
end

module EarlyResponseAws = Awskit_lwt.Make (Early_response_client)

module Backpressure_client = struct
  type ctx = unit

  module IO = Cohttp_lwt_unix.Client.IO

  type 'a io = 'a Lwt.t
  type 'a with_context = ?ctx:ctx -> 'a
  type body = Cohttp_lwt.Body.t

  let produced_chunks = ref 0
  let observed_before_consumption = ref None
  let response () = Cohttp.Response.make ~status:`OK ()
  let map_context f g ?ctx = g (f ?ctx)

  let call ?ctx:_ ?headers:_ ?body ?chunked:_ _meth _uri =
    Lwt.bind (Lwt.pause ()) (fun () ->
        observed_before_consumption := Some !produced_chunks;
        let request_body =
          match body with
          | None -> Lwt.return ""
          | Some body -> Cohttp_lwt.Body.to_string body
        in
        Lwt.bind request_body (fun _ ->
            Lwt.return (response (), Cohttp_lwt.Body.empty)))

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
    Lwt.return (response (), Cohttp_lwt.Body.empty)

  let callv ?ctx:_ _uri _requests = Lwt.return (Lwt_stream.of_list [])
end

module BackpressureAws = Awskit_lwt.Make (Backpressure_client)

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let tiny_span = Ptime.Span.of_float_s 0.001 |> Option.value_exn

let test_connection_roundtrip () =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = "eu-west-1" in
  let endpoint = "http://localhost:9000" in
  let conn =
    Aws.create ~region ~credentials:c ~clock ~endpoint
      ~retry_policy:Awskit.Retry.disabled ()
    |> conn_or_fail
  in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Aws.Runtime.Endpoint.region conn |> Awskit.Region.to_string);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map
       (Aws.Runtime.Endpoint.endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults () =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = "us-east-1" in
  let conn =
    Aws.create ~region ~credentials:c ~clock ~retry_policy:Awskit.Retry.disabled
      ()
    |> conn_or_fail
  in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map
       (Aws.Runtime.Endpoint.endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_generic_lwt_retry_requires_real_sleep_and_random () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let make ?retry_policy ?sleep ?random_float () =
    Aws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ?retry_policy ?sleep ?random_float ()
  in
  make () |> expect_validation "default retry without sleep/random";
  make ~sleep:(fun _ -> Lwt.return_unit) ()
  |> expect_validation "default retry without random";
  make ~random_float:(fun ~upper_bound:_ -> 0.0) ()
  |> expect_validation "default retry without sleep";
  ignore (make ~retry_policy:Awskit.Retry.disabled () |> conn_or_fail : Aws.t);
  ignore
    (make
       ~sleep:(fun _ -> Lwt.return_unit)
       ~random_float:(fun ~upper_bound -> upper_bound /. 2.0)
       ()
     |> conn_or_fail
      : Aws.t)

let test_generic_lwt_timeout_requires_real_sleep () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let timeout_policy = Awskit.Timeout.create_exn ~attempt:tiny_span () in
  Aws.create ~region:"us-east-1" ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ~timeout_policy ()
  |> expect_validation "timeout without sleep";
  ignore
    (Aws.create ~region:"us-east-1" ~credentials
       ~clock:(fun () -> Ptime.epoch)
       ~retry_policy:Awskit.Retry.disabled ~timeout_policy
       ~sleep:(fun _ -> Lwt.return_unit)
       ()
     |> conn_or_fail
      : Aws.t)

let test_runtime_bodies () =
  let body = Aws.Runtime.Request_body.of_string "hello" in
  Alcotest.(check int64)
    "content length" 5L
    (Option.value (Aws.Runtime.Request_body.descriptor body).content_length
       ~default:(-1L))

let test_provider_chain_continues_only_on_unavailable () =
  let open Awskit_lwt.Credentials.Provider in
  let valid =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
  in
  let chain =
    chain
      [
        create (fun () ->
            Lwt.return
              (Unavailable { source = `Env; reason = "not configured" }));
        static valid;
      ]
  in
  match Lwt_main.run (resolve chain) with
  | Resolved credentials ->
      Alcotest.(check string)
        "access key" "AKID"
        (Awskit.Credentials.access_key_id credentials)
  | Unavailable _ | Invalid _ | Failed _ ->
      Alcotest.fail "expected resolved credentials"

let test_static_provider_source_metadata () =
  let open Awskit_lwt.Credentials.Provider in
  let expires_at =
    Ptime.of_date_time ((2026, 4, 8), ((12, 0, 0), 0)) |> Option.value_exn
  in
  let unlabeled =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ~expires_at ()
  in
  let labeled =
    Awskit.Credentials.create_exn ~access_key_id:"AKID2"
      ~secret_access_key:"SECRET" ~source:(`Custom "lwt-static") ~expires_at ()
  in
  let check_static_source label expected credentials =
    match Lwt_main.run (resolve (static credentials)) with
    | Resolved resolved ->
        Alcotest.(check (option bool))
          label (Some true)
          (Option.map (Awskit.Credentials.source resolved) ~f:(function
            | source when Poly.equal source expected -> true
            | _ -> false));
        Alcotest.(check (option bool))
          (label ^ " expiration") (Some true)
          (Option.map
             (Awskit.Credentials.expires_at resolved)
             ~f:(Ptime.equal expires_at))
    | Unavailable _ | Invalid _ | Failed _ ->
        Alcotest.fail "static provider should resolve credentials"
  in
  check_static_source "unlabeled source" `Static unlabeled;
  check_static_source "labeled source" (`Custom "lwt-static") labeled

let test_provider_chain_stops_on_invalid_configured_credentials () =
  let open Awskit_lwt.Credentials.Provider in
  let valid =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
  in
  let chain =
    chain
      [
        create (fun () ->
            Lwt.return
              (Invalid
                 (Awskit.Error.Producer.validation
                    ~field:"AWS_SECRET_ACCESS_KEY" "missing secret")));
        static valid;
      ]
  in
  match Lwt_main.run (resolve chain) with
  | Invalid error ->
      Alcotest.(check bool) "validation" true (Awskit.Error.is_validation error)
  | Resolved _ | Unavailable _ | Failed _ ->
      Alcotest.fail "expected invalid credentials to stop chain"

let test_provider_chain_reports_all_unavailable () =
  let open Awskit_lwt.Credentials.Provider in
  let chain =
    chain
      [
        create (fun () ->
            Lwt.return
              (Unavailable { source = `Env; reason = "not configured" }));
        create (fun () ->
            Lwt.return
              (Unavailable
                 { source = `Shared_file "default"; reason = "missing" }));
      ]
  in
  match Lwt_main.run (resolve chain) with
  | Unavailable { source = `Shared_file "default"; reason } ->
      Alcotest.(check bool)
        "keeps useful unavailable context" true
        (String.is_substring reason ~substring:"missing")
  | Resolved _ -> Alcotest.fail "expected unavailable credentials"
  | Unavailable _ -> Alcotest.fail "unexpected unavailable source"
  | Invalid _ | Failed _ -> Alcotest.fail "expected unavailable outcome"

let request_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = "us-east-1" in
  RequestAws.create ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ()
  |> conn_or_fail

let early_response_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = "us-east-1" in
  EarlyResponseAws.create ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ()
  |> conn_or_fail

let backpressure_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = "us-east-1" in
  BackpressureAws.create ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ()
  |> conn_or_fail

let request_body_request =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"localhost" ~path:"/"
      ()
  in
  Awskit.Request.create_exn ~method_:`PUT ~target ()

let stream_descriptor length =
  Awskit.Body.Request.descriptor_exn ~content_length:length
    ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false ()

let rec wait_until ?(attempts = 1_000) condition =
  if condition () || attempts <= 0 then Lwt.return_unit
  else
    Lwt.bind (Lwt.pause ()) (fun () ->
        wait_until ~attempts:(attempts - 1) condition)

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let body_limit error =
  let open Awskit.Error in
  match kind error with Body { limit; _ } -> limit | _ -> None

let is_timeout_error = Awskit.Error.is_timeout

let test_stream_request_body_reaches_client () =
  Request_body_client.request_body := None;
  let body =
    RequestAws.Runtime.Request_body.of_stream (stream_descriptor 4L)
      ~write:(fun writer ->
        Lwt.bind (RequestAws.Runtime.Request_body.write_string writer "ab")
          (function
          | Error _ as error -> Lwt.return error
          | Ok () -> RequestAws.Runtime.Request_body.write_string writer "cd"))
  in
  match
    Lwt_main.run
      (RequestAws.Runtime.Transport.with_response (request_conn ())
         request_body_request ~body ~consume:(fun _ body ->
           RequestAws.Runtime.Response_body.discard body))
  with
  | Error error ->
      Alcotest.failf "unexpected request body error: %a" Awskit.Error.pp error
  | Ok () ->
      Alcotest.(check (option string))
        "request body" (Some "abcd")
        !Request_body_client.request_body

let test_stream_request_body_error_propagates () =
  Request_body_client.request_body := None;
  let stream_error = Awskit.Error.Producer.body "stream request body failed" in
  let body =
    RequestAws.Runtime.Request_body.of_stream (stream_descriptor 4L)
      ~write:(fun writer ->
        Lwt.bind (RequestAws.Runtime.Request_body.write_string writer "ab")
          (function
          | Error _ as error -> Lwt.return error
          | Ok () -> Lwt.return_error stream_error))
  in
  match
    Lwt_main.run
      (RequestAws.Runtime.Transport.with_response (request_conn ())
         request_body_request ~body ~consume:(fun _ body ->
           RequestAws.Runtime.Response_body.discard body))
  with
  | Error error when Awskit.Error.equal error stream_error -> ()
  | Error error ->
      Alcotest.failf "unexpected request body error: %a" Awskit.Error.pp error
  | Ok () -> Alcotest.fail "expected stream request body error"

let test_stream_request_body_cancellation_propagates () =
  Request_body_client.request_body := None;
  let body =
    RequestAws.Runtime.Request_body.of_stream (stream_descriptor 4L)
      ~write:(fun writer ->
        Lwt.bind (RequestAws.Runtime.Request_body.write_string writer "ab")
          (function
          | Error _ as error -> Lwt.return error
          | Ok () -> Lwt.fail Lwt.Canceled))
  in
  match
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Lwt.map
             (fun result -> `Returned result)
             (RequestAws.Runtime.Transport.with_response (request_conn ())
                request_body_request ~body ~consume:(fun _ body ->
                  RequestAws.Runtime.Response_body.discard body)))
         (fun exn -> Lwt.return (`Raised exn)))
  with
  | `Raised Lwt.Canceled -> ()
  | `Raised exn ->
      Alcotest.failf "unexpected raised exception: %s" (Exn.to_string exn)
  | `Returned (Error error) ->
      Alcotest.failf "request body cancellation became SDK error: %a"
        Awskit.Error.pp error
  | `Returned (Ok _) -> Alcotest.fail "expected request body cancellation"

let test_stream_request_body_timeout_returns_timeout_error () =
  Request_body_client.request_body := None;
  let timeout_policy = Awskit.Timeout.create_exn ~request_body:tiny_span () in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    RequestAws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled ~timeout_policy
      ~sleep:(fun _ -> Lwt.return_unit)
      ()
    |> conn_or_fail
  in
  let body =
    RequestAws.Runtime.Request_body.of_stream (stream_descriptor 4L)
      ~write:(fun _writer ->
        let forever, _wake = Lwt.wait () in
        forever)
  in
  match
    Lwt_main.run
      (RequestAws.Runtime.Transport.with_response conn request_body_request
         ~body ~consume:(fun _ body ->
           RequestAws.Runtime.Response_body.discard body))
  with
  | Error error when is_timeout_error error -> ()
  | Error error ->
      Alcotest.failf "expected timeout error, got: %a" Awskit.Error.pp error
  | Ok () -> Alcotest.fail "expected request body timeout"

let test_transport_timeout_cancels_blocked_call () =
  Blocking_transport_client.call_started := false;
  Blocking_transport_client.call_finalized := false;
  let timeout_policy = Awskit.Timeout.create_exn ~connect:tiny_span () in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    BlockingTransportAws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled ~timeout_policy
      ~sleep:(fun _ -> Lwt.return_unit)
      ()
    |> conn_or_fail
  in
  let result, call_started, call_finalized =
    Lwt_main.run
      (Lwt.bind
         (BlockingTransportAws.Runtime.Transport.with_response conn
            request_body_request
            ~body:BlockingTransportAws.Runtime.Request_body.empty
            ~consume:(fun _ body ->
              BlockingTransportAws.Runtime.Response_body.discard body))
         (fun result ->
           Lwt.bind
             (wait_until (fun () -> !Blocking_transport_client.call_finalized))
             (fun () ->
               Lwt.return
                 ( result,
                   !Blocking_transport_client.call_started,
                   !Blocking_transport_client.call_finalized ))))
  in
  (match result with
  | Error error when is_timeout_error error -> ()
  | Error error ->
      Alcotest.failf "expected timeout error, got: %a" Awskit.Error.pp error
  | Ok () -> Alcotest.fail "expected transport timeout");
  Alcotest.(check bool) "call started" true call_started;
  Alcotest.(check bool) "blocked call finalized" true call_finalized

let test_transport_timeout_cancels_stream_request_body () =
  Request_body_client.request_body := None;
  let timeout_policy = Awskit.Timeout.create_exn ~attempt:tiny_span () in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let producer_started, wake_producer_started = Lwt.wait () in
  let conn =
    RequestAws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled ~timeout_policy
      ~sleep:(fun _ -> producer_started)
      ()
    |> conn_or_fail
  in
  let producer_finalized = ref false in
  let body =
    RequestAws.Runtime.Request_body.of_stream (stream_descriptor 1024L)
      ~write:(fun writer ->
        Lwt.finalize
          (fun () ->
            Lwt.wakeup_later wake_producer_started ();
            Lwt.bind (RequestAws.Runtime.Request_body.write_string writer "ab")
              (function
              | Error _ as error -> Lwt.return error
              | Ok () -> Lwt_unix.sleep 60.0 |> Lwt.map (fun () -> Ok ())))
          (fun () ->
            producer_finalized := true;
            Lwt.return_unit))
  in
  let result, producer_finalized =
    Lwt_main.run
      (Lwt.bind
         (RequestAws.Runtime.Transport.with_response conn request_body_request
            ~body ~consume:(fun _ body ->
              RequestAws.Runtime.Response_body.discard body))
         (fun result ->
           Lwt.bind
             (wait_until (fun () -> !producer_finalized))
             (fun () -> Lwt.return (result, !producer_finalized))))
  in
  (match result with
  | Error error when is_timeout_error error -> ()
  | Error error ->
      Alcotest.failf "expected timeout error, got: %a" Awskit.Error.pp error
  | Ok () -> Alcotest.fail "expected transport timeout");
  Alcotest.(check bool) "producer finalized" true producer_finalized

let test_transport_exception_cancels_stream_request_body () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    FailingTransportAws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled ()
    |> conn_or_fail
  in
  let producer_finalized = ref false in
  let body =
    FailingTransportAws.Runtime.Request_body.of_stream (stream_descriptor 1024L)
      ~write:(fun _writer ->
        Lwt.finalize
          (fun () -> Lwt_unix.sleep 60.0 |> Lwt.map (fun () -> Ok ()))
          (fun () ->
            producer_finalized := true;
            Lwt.return_unit))
  in
  let result, producer_finalized =
    Lwt_main.run
      (Lwt.bind
         (FailingTransportAws.Runtime.Transport.with_response conn
            request_body_request ~body ~consume:(fun _ body ->
              FailingTransportAws.Runtime.Response_body.discard body))
         (fun result ->
           Lwt.bind
             (wait_until (fun () -> !producer_finalized))
             (fun () -> Lwt.return (result, !producer_finalized))))
  in
  (match result with
  | Error error when Awskit.Error.is_transport error -> ()
  | Error error ->
      Alcotest.failf "expected transport error, got: %a" Awskit.Error.pp error
  | Ok () -> Alcotest.fail "expected transport error");
  Alcotest.(check bool) "producer finalized" true producer_finalized

let test_callback_exception_is_not_transport_error () =
  let callback_exn = Failure "callback exploded" in
  let body = RequestAws.Runtime.Request_body.of_string "ok" in
  match
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Lwt.map
             (fun result -> `Returned result)
             (RequestAws.Runtime.Transport.with_response (request_conn ())
                request_body_request ~body ~consume:(fun _response _body ->
                  Lwt.fail callback_exn)))
         (fun exn -> Lwt.return (`Raised exn)))
  with
  | `Raised exn when Stdlib.( == ) exn callback_exn -> ()
  | `Raised exn ->
      Alcotest.failf "unexpected raised exception: %s" (Exn.to_string exn)
  | `Returned (Error error) ->
      Alcotest.failf "callback exception became SDK error: %a" Awskit.Error.pp
        error
  | `Returned (Ok _) -> Alcotest.fail "expected callback exception"

let test_request_body_exception_wins_over_transport_exception () =
  let request_body_exn = Failure "request body callback exploded" in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    FailingAfterRequestBodyAws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled ()
    |> conn_or_fail
  in
  let body =
    FailingAfterRequestBodyAws.Runtime.Request_body.of_stream
      (stream_descriptor 4L) ~write:(fun _writer ->
        Awskit.Body.Request.raise_escaped_exn request_body_exn)
  in
  match
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Lwt.map
             (fun result -> `Returned result)
             (FailingAfterRequestBodyAws.Runtime.Transport.with_response conn
                request_body_request ~body ~consume:(fun _response body ->
                  FailingAfterRequestBodyAws.Runtime.Response_body.discard body)))
         (fun exn -> Lwt.return (`Raised exn)))
  with
  | `Raised exn when Stdlib.( == ) exn request_body_exn -> ()
  | `Raised exn ->
      Alcotest.failf "unexpected raised exception: %s" (Exn.to_string exn)
  | `Returned (Error error) ->
      Alcotest.failf "request body exception became SDK error: %a"
        Awskit.Error.pp error
  | `Returned (Ok _) -> Alcotest.fail "expected request body exception"

let expect_request_body_error label result =
  match result with
  | Error error when is_body_error error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected request body error" label

let read_early_response_body body =
  let bytes = Bytes.create 16 in
  EarlyResponseAws.Runtime.Response_body.with_reader body
    ~consume:(fun reader ->
      let buffer = Buffer.create 128 in
      let rec loop () =
        Lwt.bind
          (EarlyResponseAws.Runtime.Response_body.read reader bytes ~off:0
             ~len:(Bytes.length bytes))
          (function
            | Error _ as error -> Lwt.return error
            | Ok 0 -> Lwt.return_ok (Buffer.contents buffer)
            | Ok n ->
                Buffer.add_subbytes buffer bytes ~pos:0 ~len:n;
                loop ())
      in
      loop ())

let rec wait_until ?(attempts = 20) condition =
  if condition () then Lwt.return true
  else if attempts <= 0 then Lwt.return false
  else
    Lwt.bind (Lwt.pause ()) (fun () ->
        wait_until ~attempts:(attempts - 1) condition)

let test_stream_request_body_early_response_preserves_body () =
  let error_body =
    "<Error><Code>AccessDenied</Code><Message>denied</Message></Error>"
  in
  Early_response_client.response_body := error_body;
  let producer_started = ref false in
  let producer_finalized = ref false in
  let body =
    EarlyResponseAws.Runtime.Request_body.of_stream (stream_descriptor 1024L)
      ~write:(fun writer ->
        producer_started := true;
        Lwt.bind
          (EarlyResponseAws.Runtime.Request_body.write_string writer "ab")
          (function
          | Error _ as error -> Lwt.return error
          | Ok () ->
              Lwt.finalize
                (fun () -> Lwt_unix.sleep 60.0)
                (fun () ->
                  producer_finalized := true;
                  Lwt.return_unit)
              |> Lwt.map (fun () -> Ok ())))
  in
  let outcome =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Lwt_unix.with_timeout 0.5 (fun () ->
               Lwt.bind
                 (EarlyResponseAws.Runtime.Transport.with_response
                    (early_response_conn ()) request_body_request ~body
                    ~consume:(fun response response_body ->
                      Lwt.bind (read_early_response_body response_body)
                        (function
                        | Error _ as error -> Lwt.return error
                        | Ok body -> Lwt.return_ok (response, body))))
                 (fun result ->
                   Lwt.bind
                     (wait_until (fun () -> !producer_finalized))
                     (fun producer_finalized ->
                       Lwt.return (`Result (result, producer_finalized))))))
         (function
           | Lwt_unix.Timeout -> Lwt.return `Timeout | exn -> Lwt.fail exn))
  in
  Alcotest.(check bool) "producer started" true !producer_started;
  match outcome with
  | `Timeout ->
      Alcotest.fail
        "Runtime.with_response did not return after early service response"
  | `Result (Error error, _) ->
      Alcotest.failf "unexpected runtime error: %a" Awskit.Error.pp error
  | `Result (Ok (response, body), producer_finalized) ->
      Alcotest.(check int) "status" 403 (Awskit.Response.status response);
      Alcotest.(check string) "error body" error_body body;
      Alcotest.(check bool) "producer finalized" true producer_finalized

let test_stream_request_body_backpressure_limits_read_ahead () =
  Backpressure_client.produced_chunks := 0;
  Backpressure_client.observed_before_consumption := None;
  let chunk = "aa" in
  let chunk_count = 32 in
  let body =
    BackpressureAws.Runtime.Request_body.of_stream
      (stream_descriptor (Int64.of_int (chunk_count * String.length chunk)))
      ~write:(fun writer ->
        let rec loop remaining =
          if remaining = 0 then Lwt.return_ok ()
          else
            Lwt.bind
              (BackpressureAws.Runtime.Request_body.write_string writer chunk)
              (function
              | Error _ as error -> Lwt.return error
              | Ok () ->
                  Int.incr Backpressure_client.produced_chunks;
                  loop (remaining - 1))
        in
        loop chunk_count)
  in
  (match
     Lwt_main.run
       (BackpressureAws.Runtime.Transport.with_response (backpressure_conn ())
          request_body_request ~body ~consume:(fun _ body ->
            BackpressureAws.Runtime.Response_body.discard body))
   with
  | Error error ->
      Alcotest.failf "unexpected request body error: %a" Awskit.Error.pp error
  | Ok () -> ());
  Alcotest.(check int)
    "all chunks produced" chunk_count
    !Backpressure_client.produced_chunks;
  match !Backpressure_client.observed_before_consumption with
  | None -> Alcotest.fail "client did not observe producer progress"
  | Some produced ->
      Alcotest.(check bool)
        "bounded request body read ahead" true (produced <= 16)

let test_stream_request_body_rejects_short_body () =
  Request_body_client.request_body := None;
  let body =
    RequestAws.Runtime.Request_body.of_stream (stream_descriptor 4L)
      ~write:(fun writer ->
        RequestAws.Runtime.Request_body.write_string writer "ab")
  in
  Lwt_main.run
    (RequestAws.Runtime.Transport.with_response (request_conn ())
       request_body_request ~body ~consume:(fun _ body ->
         RequestAws.Runtime.Response_body.discard body))
  |> expect_request_body_error "short request body"

let test_stream_request_body_rejects_long_body () =
  Request_body_client.request_body := None;
  let body =
    RequestAws.Runtime.Request_body.of_stream (stream_descriptor 4L)
      ~write:(fun writer ->
        Lwt.bind (RequestAws.Runtime.Request_body.write_string writer "abcd")
          (function
          | Error _ as error -> Lwt.return error
          | Ok () -> RequestAws.Runtime.Request_body.write_string writer "e"))
  in
  Lwt_main.run
    (RequestAws.Runtime.Transport.with_response (request_conn ())
       request_body_request ~body ~consume:(fun _ body ->
         RequestAws.Runtime.Response_body.discard body))
  |> expect_request_body_error "long request body"

let limited_conn ~max_response_drain_bytes =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = "us-east-1" in
  LimitedAws.create ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ~max_response_drain_bytes ()
  |> conn_or_fail

let test_create_rejects_invalid_region_string () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Aws.create ~region:"" ~credentials ~clock:(fun () -> Ptime.epoch) ()
  |> expect_validation "invalid region"

let test_create_rejects_invalid_endpoint_string () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Aws.create ~region:"us-east-1" ~endpoint:"http://localhost:9000/path"
    ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ()
  |> expect_validation "invalid endpoint"

let test_create_with_credentials_provider_rejects_invalid_endpoint_string () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let credentials_provider =
    Awskit_lwt.Credentials.Provider.static credentials
  in
  Aws.create_with_credentials_provider ~region:"us-east-1"
    ~endpoint:"http://localhost:9000/path" ~credentials_provider
    ~clock:(fun () -> Ptime.epoch)
    ()
  |> expect_validation "invalid provider endpoint"

let test_create_rejects_invalid_response_drain_limit () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Aws.create ~region:"us-east-1" ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~max_response_drain_bytes:0 ()
  |> expect_validation "invalid response drain limit"

let limited_request =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"localhost" ~path:"/"
      ()
  in
  Awskit.Request.create_exn ~method_:`GET ~target ()

let with_limited_response ~max_response_drain_bytes body ~f =
  Limited_body_client.response_body := body;
  Limited_body_client.drained_chunks := [];
  let conn = limited_conn ~max_response_drain_bytes in
  Lwt_main.run
    (LimitedAws.Runtime.Transport.with_response conn limited_request
       ~body:LimitedAws.Runtime.Request_body.empty ~consume:(fun _ body ->
         f body))

let drained_limited_body () =
  List.rev !Limited_body_client.drained_chunks |> String.concat ~sep:""

let expect_body_limit label expected = function
  | Error error when Option.equal Int64.equal (body_limit error) (Some expected)
    ->
      let limit = Option.value_exn (body_limit error) in
      Alcotest.(check int64) label expected limit
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body limit error" label

let test_response_read_timeout_interrupts_drain_cleanup () =
  Blocking_response_client.read_started_count := 0;
  Blocking_response_client.read_finalized_count := 0;
  let timeout_policy = Awskit.Timeout.create_exn ~response_body:tiny_span () in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    BlockingResponseAws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled ~timeout_policy
      ~sleep:(fun _ -> Lwt.return_unit)
      ()
    |> conn_or_fail
  in
  let result, started_count =
    Lwt_main.run
      (Lwt.map
         (fun result -> (result, !Blocking_response_client.read_started_count))
         (BlockingResponseAws.Runtime.Transport.with_response conn
            request_body_request
            ~body:BlockingResponseAws.Runtime.Request_body.empty
            ~consume:(fun _ body ->
              BlockingResponseAws.Runtime.Response_body.with_reader body
                ~consume:(fun reader ->
                  let bytes = Bytes.create 1 in
                  BlockingResponseAws.Runtime.Response_body.read reader bytes
                    ~off:0 ~len:1))))
  in
  (match result with
  | Error error when is_timeout_error error -> ()
  | Error error ->
      Alcotest.failf "expected timeout error, got: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected response body timeout");
  Alcotest.(check int) "single blocked read started" 1 started_count

let test_response_read_cancellation_skips_drain_cleanup () =
  Blocking_response_client.read_started_count := 0;
  Blocking_response_client.read_finalized_count := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    BlockingResponseAws.create ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled ()
    |> conn_or_fail
  in
  let escaped_reader = ref None in
  let observed =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Lwt_unix.with_timeout 0.5 (fun () ->
               let consume body =
                 BlockingResponseAws.Runtime.Response_body.with_reader body
                   ~consume:(fun reader ->
                     escaped_reader := Some reader;
                     let bytes = Bytes.create 1 in
                     let read =
                       BlockingResponseAws.Runtime.Response_body.read reader
                         bytes ~off:0 ~len:1
                     in
                     Lwt.bind
                       (wait_until (fun () ->
                            !Blocking_response_client.read_started_count = 1))
                       (fun started ->
                         if not started then
                           Alcotest.fail
                             "blocked response body read did not start";
                         Lwt.cancel read;
                         read))
               in
               BlockingResponseAws.Runtime.Transport.with_response conn
                 request_body_request
                 ~body:BlockingResponseAws.Runtime.Request_body.empty
                 ~consume:(fun _ body -> consume body)
               |> Lwt.map (fun result -> `Returned result)))
         (function
           | Lwt_unix.Timeout -> Lwt.return `Timed_out
           | exn -> Lwt.return (`Raised exn)))
  in
  (match observed with
  | `Raised Lwt.Canceled -> ()
  | `Raised exn ->
      Alcotest.failf "unexpected raised exception: %s" (Exn.to_string exn)
  | `Timed_out ->
      Alcotest.fail "response body cancellation waited for drain cleanup"
  | `Returned (Error error) ->
      Alcotest.failf "response body cancellation became SDK error: %a"
        Awskit.Error.pp error
  | `Returned (Ok _) -> Alcotest.fail "expected response body cancellation");
  Alcotest.(check int)
    "single blocked read started" 1
    !Blocking_response_client.read_started_count;
  if !Blocking_response_client.read_finalized_count = 0 then
    let reader =
      match !escaped_reader with
      | Some reader -> reader
      | None -> Alcotest.fail "expected escaped response body reader"
    in
    let bytes = Bytes.create 1 in
    match
      Lwt_main.run
        (BlockingResponseAws.Runtime.Response_body.read reader bytes ~off:0
           ~len:1)
    with
    | Error error when is_body_error error ->
        Alcotest.(check int)
          "closed reader did not start drain read" 1
          !Blocking_response_client.read_started_count
    | Error error ->
        Alcotest.failf "unexpected closed reader error: %a" Awskit.Error.pp
          error
    | Ok _ -> Alcotest.fail "canceled response body reader stayed active"
  else
    Alcotest.(check int)
      "blocked read finalized" 1
      !Blocking_response_client.read_finalized_count

let test_discard_response_body_enforces_limit () =
  with_limited_response ~max_response_drain_bytes:3 "abcdef"
    ~f:LimitedAws.Runtime.Response_body.discard
  |> expect_body_limit "discard limit" 3L

let test_with_response_body_drain_enforces_limit () =
  with_limited_response ~max_response_drain_bytes:3 "abcdef" ~f:(fun body ->
      LimitedAws.Runtime.Response_body.with_reader body ~consume:(fun _ ->
          Lwt.return_ok ()))
  |> expect_body_limit "scoped drain limit" 3L

let test_with_response_body_preserves_consumer_error () =
  let consumer_error = Awskit.Error.Producer.body "consumer failed" in
  with_limited_response ~max_response_drain_bytes:3 "abcdef" ~f:(fun body ->
      LimitedAws.Runtime.Response_body.with_reader body ~consume:(fun _ ->
          Lwt.return_error consumer_error))
  |> function
  | Error error when Awskit.Error.equal error consumer_error -> ()
  | Error error ->
      Alcotest.failf "expected consumer error, got: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected consumer error"

let test_with_response_body_drains_after_consumer_exception () =
  let exception Consumer_failed in
  let observed =
    try
      ignore
        (with_limited_response ~max_response_drain_bytes:64 "abcdef"
           ~f:(fun body ->
             LimitedAws.Runtime.Response_body.with_reader body
               ~consume:(fun _reader -> Lwt.fail Consumer_failed))
          : (unit, Awskit.Error.t) Result.t);
      `Returned
    with exn -> `Raised exn
  in
  match observed with
  | `Raised exn when Stdlib.( == ) exn Consumer_failed ->
      Alcotest.(check string) "drained body" "abcdef" (drained_limited_body ())
  | `Raised exn -> Alcotest.failf "unexpected exception: %s" (Exn.to_string exn)
  | `Returned -> Alcotest.fail "expected consumer exception"

let test_response_body_reader_cannot_escape_scope () =
  let escaped = ref None in
  let result =
    with_limited_response ~max_response_drain_bytes:64 "abcdef" ~f:(fun body ->
        LimitedAws.Runtime.Response_body.with_reader body
          ~consume:(fun reader ->
            escaped := Some reader;
            Lwt.return_ok ()))
  in
  (match result with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "unexpected with_reader error: %a" Awskit.Error.pp error);
  let reader =
    match !escaped with
    | Some reader -> reader
    | None -> Alcotest.fail "expected escaped reader"
  in
  let bytes = Bytes.create 1 in
  match
    Lwt_main.run
      (LimitedAws.Runtime.Response_body.read reader bytes ~off:0 ~len:1)
  with
  | Error error when is_body_error error -> ()
  | Error error ->
      Alcotest.failf "unexpected read error: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "escaped reader read succeeded"

let suite =
  [
    ( "integration:awskit-lwt:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
        Alcotest.test_case "retry requires real sleep and random" `Quick
          test_generic_lwt_retry_requires_real_sleep_and_random;
        Alcotest.test_case "timeout requires real sleep" `Quick
          test_generic_lwt_timeout_requires_real_sleep;
        Alcotest.test_case "rejects invalid region string" `Quick
          test_create_rejects_invalid_region_string;
        Alcotest.test_case "rejects invalid endpoint string" `Quick
          test_create_rejects_invalid_endpoint_string;
        Alcotest.test_case
          "credentials provider rejects invalid endpoint string" `Quick
          test_create_with_credentials_provider_rejects_invalid_endpoint_string;
        Alcotest.test_case "rejects invalid response drain limit" `Quick
          test_create_rejects_invalid_response_drain_limit;
        Alcotest.test_case "runtime bodies" `Quick test_runtime_bodies;
        Alcotest.test_case "provider chain continues on unavailable" `Quick
          test_provider_chain_continues_only_on_unavailable;
        Alcotest.test_case "static provider source metadata" `Quick
          test_static_provider_source_metadata;
        Alcotest.test_case "provider chain stops on invalid" `Quick
          test_provider_chain_stops_on_invalid_configured_credentials;
        Alcotest.test_case "provider chain reports unavailable" `Quick
          test_provider_chain_reports_all_unavailable;
        Alcotest.test_case "stream request body reaches client" `Quick
          test_stream_request_body_reaches_client;
        Alcotest.test_case "stream request body error propagates" `Quick
          test_stream_request_body_error_propagates;
        Alcotest.test_case "stream request body cancellation propagates" `Quick
          test_stream_request_body_cancellation_propagates;
        Alcotest.test_case "stream request body timeout" `Quick
          test_stream_request_body_timeout_returns_timeout_error;
        Alcotest.test_case "transport timeout cancels blocked call" `Quick
          test_transport_timeout_cancels_blocked_call;
        Alcotest.test_case "transport timeout cancels stream request body"
          `Quick test_transport_timeout_cancels_stream_request_body;
        Alcotest.test_case "transport exception cancels stream request body"
          `Quick test_transport_exception_cancels_stream_request_body;
        Alcotest.test_case "callback exception is not transport error" `Quick
          test_callback_exception_is_not_transport_error;
        Alcotest.test_case
          "request body exception is not masked by transport error" `Quick
          test_request_body_exception_wins_over_transport_exception;
        Alcotest.test_case "stream request body early response preserves body"
          `Quick test_stream_request_body_early_response_preserves_body;
        Alcotest.test_case "stream request body backpressure limits read ahead"
          `Quick test_stream_request_body_backpressure_limits_read_ahead;
        Alcotest.test_case "stream request body rejects short body" `Quick
          test_stream_request_body_rejects_short_body;
        Alcotest.test_case "stream request body rejects long body" `Quick
          test_stream_request_body_rejects_long_body;
        Alcotest.test_case "response read timeout interrupts drain cleanup"
          `Quick test_response_read_timeout_interrupts_drain_cleanup;
        Alcotest.test_case "response read cancellation skips drain cleanup"
          `Quick test_response_read_cancellation_skips_drain_cleanup;
        Alcotest.test_case "discard body limit" `Quick
          test_discard_response_body_enforces_limit;
        Alcotest.test_case "scoped drain body limit" `Quick
          test_with_response_body_drain_enforces_limit;
        Alcotest.test_case "consumer error wins over drain error" `Quick
          test_with_response_body_preserves_consumer_error;
        Alcotest.test_case "consumer exception still drains body" `Quick
          test_with_response_body_drains_after_consumer_exception;
        Alcotest.test_case "response body reader cannot escape scope" `Quick
          test_response_body_reader_cannot_escape_scope;
      ] );
  ]
