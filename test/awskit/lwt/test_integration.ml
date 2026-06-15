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
  let body = Aws.Runtime.Request_body.of_string "hello" in
  Alcotest.(check int64)
    "content length" 5L
    (Option.value (Aws.Runtime.Request_body.descriptor body).content_length
       ~default:(-1L))

let test_provider_chain_uses_multiple_errors () =
  let first =
    Awskit_lwt.Credentials.Provider.create (fun () ->
        Lwt.return_error
          (Awskit.Error.validation ~field:"env" "missing env credentials"))
  in
  let second =
    Awskit_lwt.Credentials.Provider.create (fun () ->
        Lwt.return_error
          (Awskit.Error.validation ~field:"profile"
             "missing profile credentials"))
  in
  match
    Lwt_main.run
      (Awskit_lwt.Credentials.Provider.resolve
         (Awskit_lwt.Credentials.Provider.chain [ first; second ]))
  with
  | Ok _ -> Alcotest.fail "expected provider chain failure"
  | Error error -> (
      match Awskit.Error.kind error with
      | Awskit.Error.Multiple [ env_error; profile_error ] ->
          Alcotest.(check (option string))
            "first provider field" (Some "env")
            (Awskit.Error.validation_field env_error);
          Alcotest.(check (option string))
            "second provider field" (Some "profile")
            (Awskit.Error.validation_field profile_error)
      | Awskit.Error.Multiple errors ->
          Alcotest.failf "expected two provider errors, got %d"
            (List.length errors)
      | _ ->
          Alcotest.failf "expected Multiple, got %s"
            (Awskit.Error.to_string_hum error))

let request_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  RequestAws.create ~region ~credentials ~clock:(fun () -> Ptime.epoch) ()

let early_response_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  EarlyResponseAws.create ~region ~credentials ~clock:(fun () -> Ptime.epoch) ()

let backpressure_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  BackpressureAws.create ~region ~credentials ~clock:(fun () -> Ptime.epoch) ()

let request_body_request =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"localhost" ~path:"/"
      ()
  in
  Awskit.Request.create_exn ~method_:`PUT ~target ()

let stream_descriptor length =
  {
    Awskit.Body.Request.content_length = Some length;
    payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
    replayable = false;
  }

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let body_limit error =
  let open Awskit.Error in
  match kind error with Body { limit; _ } -> limit | _ -> None

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
      (RequestAws.Runtime.with_response (request_conn ()) request_body_request
         body ~f:(fun _ body -> RequestAws.Runtime.Response_body.discard body))
  with
  | Error error ->
      Alcotest.failf "unexpected request body error: %a" Awskit.Error.pp error
  | Ok () ->
      Alcotest.(check (option string))
        "request body" (Some "abcd")
        !Request_body_client.request_body

let test_stream_request_body_error_propagates () =
  Request_body_client.request_body := None;
  let stream_error = Awskit.Error.body "stream request body failed" in
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
      (RequestAws.Runtime.with_response (request_conn ()) request_body_request
         body ~f:(fun _ body -> RequestAws.Runtime.Response_body.discard body))
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
             (RequestAws.Runtime.with_response (request_conn ())
                request_body_request body ~f:(fun _ body ->
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

let test_callback_exception_is_not_transport_error () =
  let callback_exn = Failure "callback exploded" in
  let body = RequestAws.Runtime.Request_body.of_string "ok" in
  match
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           Lwt.map
             (fun result -> `Returned result)
             (RequestAws.Runtime.with_response (request_conn ())
                request_body_request body ~f:(fun _response _body ->
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
                 (EarlyResponseAws.Runtime.with_response
                    (early_response_conn ()) request_body_request body
                    ~f:(fun response response_body ->
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
       (BackpressureAws.Runtime.with_response (backpressure_conn ())
          request_body_request body ~f:(fun _ body ->
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
    (RequestAws.Runtime.with_response (request_conn ()) request_body_request
       body ~f:(fun _ body -> RequestAws.Runtime.Response_body.discard body))
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
    (RequestAws.Runtime.with_response (request_conn ()) request_body_request
       body ~f:(fun _ body -> RequestAws.Runtime.Response_body.discard body))
  |> expect_request_body_error "long request body"

let limited_conn ~max_response_drain_bytes =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = Awskit.Region.of_string_exn "us-east-1" in
  LimitedAws.create ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~max_response_drain_bytes ()

let limited_request =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"localhost" ~path:"/"
      ()
  in
  Awskit.Request.create_exn ~method_:`GET ~target ()

let with_limited_response ~max_response_drain_bytes body ~f =
  Limited_body_client.response_body := body;
  let conn = limited_conn ~max_response_drain_bytes in
  Lwt_main.run
    (LimitedAws.Runtime.with_response conn limited_request
       LimitedAws.Runtime.Request_body.empty ~f:(fun _ body -> f body))

let expect_body_limit label expected = function
  | Error error when Option.equal Int64.equal (body_limit error) (Some expected)
    ->
      let limit = Option.value_exn (body_limit error) in
      Alcotest.(check int64) label expected limit
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body limit error" label

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
  let consumer_error = Awskit.Error.body "consumer failed" in
  with_limited_response ~max_response_drain_bytes:3 "abcdef" ~f:(fun body ->
      LimitedAws.Runtime.Response_body.with_reader body ~consume:(fun _ ->
          Lwt.return_error consumer_error))
  |> function
  | Error error when Awskit.Error.equal error consumer_error -> ()
  | Error error ->
      Alcotest.failf "expected consumer error, got: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected consumer error"

let suite =
  [
    ( "integration:connection",
      [
        Alcotest.test_case "roundtrip" `Quick test_connection_roundtrip;
        Alcotest.test_case "defaults" `Quick test_connection_defaults;
        Alcotest.test_case "runtime bodies" `Quick test_runtime_bodies;
        Alcotest.test_case "provider chain uses multiple errors" `Quick
          test_provider_chain_uses_multiple_errors;
        Alcotest.test_case "stream request body reaches client" `Quick
          test_stream_request_body_reaches_client;
        Alcotest.test_case "stream request body error propagates" `Quick
          test_stream_request_body_error_propagates;
        Alcotest.test_case "stream request body cancellation propagates" `Quick
          test_stream_request_body_cancellation_propagates;
        Alcotest.test_case "callback exception is not transport error" `Quick
          test_callback_exception_is_not_transport_error;
        Alcotest.test_case "stream request body early response preserves body"
          `Quick test_stream_request_body_early_response_preserves_body;
        Alcotest.test_case "stream request body backpressure limits read ahead"
          `Quick test_stream_request_body_backpressure_limits_read_ahead;
        Alcotest.test_case "stream request body rejects short body" `Quick
          test_stream_request_body_rejects_short_body;
        Alcotest.test_case "stream request body rejects long body" `Quick
          test_stream_request_body_rejects_long_body;
        Alcotest.test_case "discard body limit" `Quick
          test_discard_response_body_enforces_limit;
        Alcotest.test_case "scoped drain body limit" `Quick
          test_with_response_body_drain_enforces_limit;
        Alcotest.test_case "consumer error wins over drain error" `Quick
          test_with_response_body_preserves_consumer_error;
      ] );
  ]
