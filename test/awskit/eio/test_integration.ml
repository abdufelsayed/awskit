(** Integration tests for Eio runtime: connection creation. *)

open Base

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let expect_validation label = function
  | Error error when Awskit.Error.is_validation error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let test_connection_roundtrip env =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = "eu-west-1" in
  let endpoint = "http://localhost:9000" in
  let conn =
    Eio.Switch.run @@ fun sw ->
    Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region
      ~credentials:c ~clock ~endpoint ()
    |> conn_or_fail
  in
  Alcotest.(check string)
    "region" "eu-west-1"
    (Awskit_eio.Runtime.Endpoint.region conn |> Awskit.Region.to_string);
  Alcotest.(check (option string))
    "endpoint" (Some "http://localhost:9000")
    (Option.map
       (Awskit_eio.Runtime.Endpoint.endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let test_connection_defaults env =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let clock () = Ptime.epoch in
  let region = "us-east-1" in
  let conn =
    Eio.Switch.run @@ fun sw ->
    Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region
      ~credentials:c ~clock ()
    |> conn_or_fail
  in
  Alcotest.(check (option string))
    "no endpoint" None
    (Option.map
       (Awskit_eio.Runtime.Endpoint.endpoint conn)
       ~f:Awskit.Endpoint.to_url_prefix)

let ptime_of_eio_clock env =
  Eio.Time.now env#clock |> Ptime.of_float_s |> Option.value_exn

let test_connection_uses_env_clock_by_default env =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = "us-east-1" in
  let conn =
    Eio.Switch.run @@ fun sw ->
    Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region
      ~credentials:c ()
    |> conn_or_fail
  in
  let before = ptime_of_eio_clock env in
  let actual = Awskit_eio.Runtime.Clock.now conn in
  let after = ptime_of_eio_clock env in
  Alcotest.(check bool)
    "clock between call bounds" true
    (Ptime.compare before actual <= 0 && Ptime.compare actual after <= 0)

let test_runtime_bodies env =
  let c =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = "us-east-1" in
  Eio.Switch.run @@ fun sw ->
  let conn =
    Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region
      ~credentials:c ()
    |> conn_or_fail
  in
  ignore (Awskit_eio.Runtime.Endpoint.region conn : Awskit.Region.t);
  let body = Awskit_eio.Runtime.Request_body.of_string "hello" in
  Alcotest.(check int64)
    "content length" 5L
    (Option.value
       (Awskit_eio.Runtime.Request_body.descriptor body).content_length
       ~default:(-1L))

let stream_descriptor length =
  Awskit.Body.Request.descriptor_exn ~content_length:length
    ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:false ()

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let body_limit error =
  let open Awskit.Error in
  match kind error with Body { limit; _ } -> limit | _ -> None

let is_timeout_error = Awskit.Error.is_timeout
let tiny_span = Ptime.Span.of_float_s 0.001 |> Option.value_exn

let is_transport_error_with_message ~substring error =
  let open Awskit.Error in
  match kind error with
  | Transport { message; _ } -> String.is_substring message ~substring
  | _ -> false

let request_conn ?timeout_policy ?max_response_drain_bytes env sw =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let region = "us-east-1" in
  Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ?timeout_policy ?max_response_drain_bytes ()
  |> conn_or_fail

let test_create_rejects_invalid_region_string env =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Eio.Switch.run @@ fun sw ->
  Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"" ~credentials
    ()
  |> expect_validation "invalid region"

let test_create_rejects_invalid_endpoint_string env =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Eio.Switch.run @@ fun sw ->
  Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"us-east-1"
    ~endpoint:"http://localhost:9000/path" ~credentials ()
  |> expect_validation "invalid endpoint"

let test_create_rejects_invalid_response_drain_limit env =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Eio.Switch.run @@ fun sw ->
  Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"us-east-1"
    ~credentials ~max_response_drain_bytes:0 ()
  |> expect_validation "invalid response drain limit"

let rec read_all body buffer =
  let chunk = Bytes.create 2 in
  match
    Awskit_eio.Runtime.Response_body.read body chunk ~off:0
      ~len:(Bytes.length chunk)
  with
  | Error error ->
      Alcotest.failf "unexpected body read error: %a" Awskit.Error.pp error
  | Ok 0 -> Buffer.contents buffer
  | Ok n ->
      Buffer.add_substring buffer (Bytes.to_string chunk) ~pos:0 ~len:n;
      read_all body buffer

let listener_bind_denied_by_sandbox = function
  (* opam-repository macOS CI can deny local TCP listeners under sandbox.sh. *)
  | Unix.Unix_error (Unix.EPERM, "bind", _) -> true
  | _ -> false

let test_listener_bind_denied_by_sandbox () =
  Alcotest.(check bool)
    "EPERM bind" true
    (listener_bind_denied_by_sandbox (Unix.Unix_error (Unix.EPERM, "bind", "")));
  Alcotest.(check bool)
    "other bind error" false
    (listener_bind_denied_by_sandbox
       (Unix.Unix_error (Unix.EADDRINUSE, "bind", "")));
  Alcotest.(check bool)
    "other exception" false
    (listener_bind_denied_by_sandbox (Failure "bind"))

let with_eio_early_response_server env ?(on_request_body = fun _ -> ())
    ?(on_response_sent = fun () -> ()) ?(ignore_connection_errors = false)
    ~status ~response_body ~read_request_body test =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listening_socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn -> Alcotest.skip ()
  in
  let port =
    match Eio.Net.listening_addr listening_socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listening socket"
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun exn ->
          if ignore_connection_errors then () else raise exn)
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          let rec read_headers () =
            match Eio.Buf_read.line input with
            | "" -> []
            | line -> line :: read_headers ()
          in
          let headers = read_headers () in
          let header_value header_name =
            List.find_map headers ~f:(fun header ->
                match String.lsplit2 header ~on:':' with
                | Some (name, value)
                  when String.Caseless.equal (String.strip name) header_name ->
                    Some (String.strip value)
                | _ -> None)
          in
          let content_length =
            match header_value "content-length" with
            | None -> None
            | Some value -> (
                match Int.of_string_opt value with
                | Some length when length >= 0 -> Some length
                | _ -> Alcotest.failf "invalid Content-Length header: %s" value)
          in
          let transfer_chunked =
            match header_value "transfer-encoding" with
            | None -> false
            | Some value ->
                String.is_substring (String.lowercase value)
                  ~substring:"chunked"
          in
          let read_chunked_body () =
            let rec loop chunks =
              let size_line = Eio.Buf_read.line input |> String.strip in
              let size_text =
                match String.lsplit2 size_line ~on:';' with
                | Some (size, _) -> size
                | None -> size_line
              in
              match Int.of_string_opt ("0x" ^ size_text) with
              | None -> Alcotest.failf "invalid chunk size: %s" size_line
              | Some 0 ->
                  ignore (Eio.Buf_read.line input : string);
                  String.concat ~sep:"" (List.rev chunks)
              | Some length ->
                  let chunk = Eio.Buf_read.take length input in
                  ignore (Eio.Buf_read.line input : string);
                  loop (chunk :: chunks)
            in
            loop []
          in
          let () =
            if read_request_body then
              match (content_length, transfer_chunked) with
              | Some length, _ ->
                  Eio.Buf_read.take length input |> on_request_body
              | None, true -> read_chunked_body () |> on_request_body
              | None, false ->
                  Alcotest.fail
                    "read_request_body requires a Content-Length or chunked \
                     request body"
          in
          Eio.Buf_write.with_flow flow (fun output ->
              Eio.Buf_write.string output
                (Fmt.str
                   "HTTP/1.1 %d test\r\n\
                    Content-Length: %d\r\n\
                    Connection: close\r\n\
                    \r\n\
                    %s"
                   status
                   (String.length response_body)
                   response_body);
              Eio.Buf_write.flush output;
              on_response_sent ())));
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  test endpoint

let with_eio_stalled_response_server env test =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listening_socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn -> Alcotest.skip ()
  in
  let port =
    match Eio.Net.listening_addr listening_socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listening socket"
  in
  let client_closed, wake_client_closed = Eio.Promise.create () in
  let resolve_client_closed () =
    ignore (Eio.Promise.try_resolve wake_client_closed () : bool)
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun _ -> ())
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          let rec read_headers () =
            match Eio.Buf_read.line input with "" -> () | _ -> read_headers ()
          in
          read_headers ();
          Eio.Buf_write.with_flow flow (fun output ->
              Eio.Buf_write.string output
                "HTTP/1.1 200 test\r\n\
                 Content-Length: 1\r\n\
                 Connection: close\r\n\
                 \r\n";
              Eio.Buf_write.flush output);
          let buffer = Cstruct.create 1 in
          match Eio.Flow.single_read flow buffer with
          | _ -> resolve_client_closed ()
          | exception End_of_file -> resolve_client_closed ()
          | exception _ -> resolve_client_closed ()));
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  test ~client_closed endpoint

let request_body_request_for_endpoint endpoint =
  let target =
    Awskit.Request.Target.create_exn
      ~scheme:(Awskit.Endpoint.scheme endpoint)
      ~host:(Awskit.Endpoint.host endpoint)
      ?port:(Awskit.Endpoint.port endpoint)
      ~path:"/" ()
  in
  Awskit.Request.create_exn ~method_:`PUT ~target ()

let with_eio_chunked_keep_alive_response_server env ~response_body test =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listening_socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn -> Alcotest.skip ()
  in
  let port =
    match Eio.Net.listening_addr listening_socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listening socket"
  in
  let hold_open, resolve_hold_open = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun _ -> ())
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          let rec read_headers () =
            match Eio.Buf_read.line input with "" -> () | _ -> read_headers ()
          in
          read_headers ();
          Eio.Buf_write.with_flow flow (fun output ->
              Eio.Buf_write.string output
                (Fmt.str
                   "HTTP/1.1 200 test\r\n\
                    Transfer-Encoding: chunked\r\n\
                    Connection: keep-alive\r\n\
                    \r\n\
                    %x\r\n\
                    %s\r\n\
                    0\r\n\
                    \r\n"
                   (String.length response_body)
                   response_body);
              Eio.Buf_write.flush output;
              Eio.Promise.await hold_open)));
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  let release_server () =
    ignore (Eio.Promise.try_resolve resolve_hold_open () : bool)
  in
  match test endpoint with
  | result ->
      release_server ();
      result
  | exception exn ->
      release_server ();
      raise exn

let test_https_request_requires_connector env =
  Eio.Switch.run @@ fun sw ->
  let conn = request_conn env sw in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Https ~host:"example.com"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`GET ~target () in
  let body = Awskit_eio.Runtime.Request_body.empty in
  match
    Awskit_eio.Runtime.Transport.with_response conn request ~body
      ~consume:(fun _ _ -> Ok ())
  with
  | Error error
    when is_transport_error_with_message
           ~substring:"HTTPS endpoint requires an HTTPS connector" error ->
      ()
  | Error error -> Alcotest.failf "unexpected error: %a" Awskit.Error.pp error
  | Ok () -> Alcotest.fail "expected missing HTTPS connector error"

let read_response_body_to_string body =
  Awskit_eio.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      Ok (read_all reader (Buffer.create 128)))

let test_no_read_past_response_eof env =
  let timeout_policy = Awskit.Timeout.create_exn ~drain:tiny_span () in
  with_eio_chunked_keep_alive_response_server env ~response_body:"hello"
    (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn ~timeout_policy env sw in
      let request = request_body_request_for_endpoint endpoint in
      match
        Awskit_eio.Runtime.Transport.with_response conn request
          ~body:Awskit_eio.Runtime.Request_body.empty
          ~consume:(fun _ response_body ->
            read_response_body_to_string response_body)
      with
      | Ok payload -> Alcotest.(check string) "consumed body" "hello" payload
      | Error error ->
          Alcotest.failf "unexpected body read error: %a" Awskit.Error.pp error)

(* A server that answers like a real S3/Ceph HeadObject: a 200 advertising a
   Content-Length but sending NO message body, then holding the keep-alive
   connection open. A client that tries to read/drain that advertised body
   blocks until the connection closes. *)
let with_eio_head_no_body_server env ~content_length test =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let listening_socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn -> Alcotest.skip ()
  in
  let port =
    match Eio.Net.listening_addr listening_socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listening socket"
  in
  let hold_open, resolve_hold_open = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun _ -> ())
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          let rec read_headers () =
            match Eio.Buf_read.line input with "" -> () | _ -> read_headers ()
          in
          read_headers ();
          Eio.Buf_write.with_flow flow (fun output ->
              Eio.Buf_write.string output
                (Fmt.str
                   "HTTP/1.1 200 test\r\n\
                    Content-Length: %d\r\n\
                    Connection: keep-alive\r\n\
                    \r\n"
                   content_length);
              Eio.Buf_write.flush output;
              Eio.Promise.await hold_open)));
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  let release_server () =
    ignore (Eio.Promise.try_resolve resolve_hold_open () : bool)
  in
  match test endpoint with
  | result ->
      release_server ();
      result
  | exception exn ->
      release_server ();
      raise exn

let head_request_for_endpoint endpoint =
  let target =
    Awskit.Request.Target.create_exn
      ~scheme:(Awskit.Endpoint.scheme endpoint)
      ~host:(Awskit.Endpoint.host endpoint)
      ?port:(Awskit.Endpoint.port endpoint)
      ~path:"/" ()
  in
  Awskit.Request.create_exn ~method_:`HEAD ~target ()

(* Regression: a HEAD response has no message body even when Content-Length is
   present (RFC 7230 §3.3.3), so the runtime must treat it as empty and never
   read the body flow. A tiny drain timeout makes a regression (reading the
   advertised body) fail fast instead of hanging. *)
let test_head_response_is_bodiless env =
  let timeout_policy = Awskit.Timeout.create_exn ~drain:tiny_span () in
  with_eio_head_no_body_server env ~content_length:5 (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn ~timeout_policy env sw in
      let request = head_request_for_endpoint endpoint in
      match
        Awskit_eio.Runtime.Transport.with_response conn request
          ~body:Awskit_eio.Runtime.Request_body.empty
          ~consume:(fun _ response_body ->
            read_response_body_to_string response_body)
      with
      | Ok payload -> Alcotest.(check string) "head body is empty" "" payload
      | Error error ->
          Alcotest.failf "unexpected HEAD body error: %a" Awskit.Error.pp error)

let test_stream_request_body_emits_multiple_chunks env =
  let request_body = ref None in
  with_eio_early_response_server env ~status:200 ~response_body:"ok"
    ~read_request_body:true
    ~on_request_body:(fun body -> request_body := Some body)
    (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn env sw in
      let request = request_body_request_for_endpoint endpoint in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 6L)
          ~write:(fun writer ->
            match Awskit_eio.Runtime.Request_body.write_string writer "ab" with
            | Error _ as error -> error
            | Ok () -> (
                match
                  Awskit_eio.Runtime.Request_body.write_string writer "cd"
                with
                | Error _ as error -> error
                | Ok () ->
                    Awskit_eio.Runtime.Request_body.write_string writer "ef"))
      in
      match
        Awskit_eio.Runtime.Transport.with_response conn request ~body
          ~consume:(fun _ response_body ->
            Awskit_eio.Runtime.Response_body.discard response_body)
      with
      | Error error ->
          Alcotest.failf "unexpected request body error: %a" Awskit.Error.pp
            error
      | Ok () ->
          Alcotest.(check (option string))
            "request body" (Some "abcdef") !request_body)

let test_stream_request_body_error_propagates env =
  let stream_error = Awskit.Error.Producer.body "stream request body failed" in
  with_eio_early_response_server env ~status:200 ~response_body:"ok"
    ~read_request_body:false ~ignore_connection_errors:true (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn env sw in
      let request = request_body_request_for_endpoint endpoint in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 4L)
          ~write:(fun writer ->
            match Awskit_eio.Runtime.Request_body.write_string writer "ab" with
            | Error _ as error -> error
            | Ok () -> Error stream_error)
      in
      match
        Awskit_eio.Runtime.Transport.with_response conn request ~body
          ~consume:(fun _ response_body ->
            Awskit_eio.Runtime.Response_body.discard response_body)
      with
      | Error error when Awskit.Error.equal error stream_error -> ()
      | Error error ->
          Alcotest.failf "unexpected request body error: %a" Awskit.Error.pp
            error
      | Ok () -> Alcotest.fail "expected stream request body error")

let test_stream_request_body_escaped_exception_propagates env =
  let callback_exn = Failure "request body callback exploded" in
  with_eio_early_response_server env ~status:200 ~response_body:"ok"
    ~read_request_body:false ~ignore_connection_errors:true (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn env sw in
      let request = request_body_request_for_endpoint endpoint in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 4L)
          ~write:(fun writer ->
            match Awskit_eio.Runtime.Request_body.write_string writer "ab" with
            | Error _ as error -> error
            | Ok () -> Awskit.Body.Request.raise_escaped_exn callback_exn)
      in
      try
        ignore
          (Awskit_eio.Runtime.Transport.with_response conn request ~body
             ~consume:(fun _ response_body ->
               Awskit_eio.Runtime.Response_body.discard response_body)
            : (unit, Awskit.Error.t) Result.t);
        Alcotest.fail "expected escaped request body exception"
      with
      | exn when phys_equal exn callback_exn -> ()
      | exn -> Alcotest.failf "unexpected exception: %s" (Exn.to_string exn))

let test_stream_request_body_timeout_returns_timeout_error env =
  let timeout_policy = Awskit.Timeout.create_exn ~request_body:tiny_span () in
  with_eio_early_response_server env ~status:200 ~response_body:"ok"
    ~read_request_body:false ~ignore_connection_errors:true (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn =
        Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only
          ~region:"us-east-1"
          ~credentials:
            (Awskit.Credentials.create_exn ~access_key_id:"AK"
               ~secret_access_key:"SK" ())
          ~clock:(fun () -> Ptime.epoch)
          ~timeout_policy ()
        |> conn_or_fail
      in
      let request = request_body_request_for_endpoint endpoint in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 4L)
          ~write:(fun _writer -> Eio.Fiber.await_cancel ())
      in
      match
        Awskit_eio.Runtime.Transport.with_response conn request ~body
          ~consume:(fun _ response_body ->
            Awskit_eio.Runtime.Response_body.discard response_body)
      with
      | Error error when is_timeout_error error -> ()
      | Error error ->
          Alcotest.failf "expected timeout error, got: %a" Awskit.Error.pp error
      | Ok () -> Alcotest.fail "expected request body timeout")

let expect_request_body_error label = function
  | Error error when is_body_error error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok () -> Alcotest.failf "%s: expected request body error" label

let test_stream_request_body_rejects_short_body env =
  with_eio_early_response_server env ~status:200 ~response_body:"ok"
    ~read_request_body:false ~ignore_connection_errors:true (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn env sw in
      let request = request_body_request_for_endpoint endpoint in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 4L)
          ~write:(fun writer ->
            Awskit_eio.Runtime.Request_body.write_string writer "ab")
      in
      Awskit_eio.Runtime.Transport.with_response conn request ~body
        ~consume:(fun _ response_body ->
          Awskit_eio.Runtime.Response_body.discard response_body)
      |> expect_request_body_error "short request body")

let test_stream_request_body_rejects_long_body env =
  with_eio_early_response_server env ~status:200 ~response_body:"ok"
    ~read_request_body:false ~ignore_connection_errors:true (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn env sw in
      let request = request_body_request_for_endpoint endpoint in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 4L)
          ~write:(fun writer ->
            match
              Awskit_eio.Runtime.Request_body.write_string writer "abcd"
            with
            | Error _ as error -> error
            | Ok () -> Awskit_eio.Runtime.Request_body.write_string writer "e")
      in
      Awskit_eio.Runtime.Transport.with_response conn request ~body
        ~consume:(fun _ response_body ->
          Awskit_eio.Runtime.Response_body.discard response_body)
      |> expect_request_body_error "long request body")

let test_stream_request_body_early_response_cancels_producer_and_preserves_body
    env =
  let error_body =
    "<Error><Code>AccessDenied</Code><Message>denied</Message></Error>"
  in
  with_eio_early_response_server env ~status:403 ~response_body:error_body
    ~read_request_body:false (fun endpoint ->
      Eio.Switch.run @@ fun conn_sw ->
      let clock = Eio.Stdenv.clock env in
      let lifecycle_timeout = 1.0 in
      let conn = request_conn env conn_sw in
      let request = request_body_request_for_endpoint endpoint in
      let producer_started = ref false in
      let producer_finished, wake_producer_finished = Eio.Promise.create () in
      let finish_producer () =
        ignore (Eio.Promise.try_resolve wake_producer_finished () : bool)
      in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 1024L)
          ~write:(fun writer ->
            producer_started := true;
            match Awskit_eio.Runtime.Request_body.write_string writer "ab" with
            | Error _ as error -> error
            | Ok () ->
                Exn.protect ~finally:finish_producer ~f:(fun () ->
                    Eio.Fiber.await_cancel ()))
      in
      let result =
        try
          Eio.Time.with_timeout_exn clock lifecycle_timeout (fun () ->
              Awskit_eio.Runtime.Transport.with_response conn request ~body
                ~consume:(fun response response_body ->
                  match read_response_body_to_string response_body with
                  | Ok body -> Ok (response, body)
                  | Error _ as error -> error))
        with Eio.Time.Timeout ->
          Alcotest.fail
            "Runtime.with_response did not return after early service response"
      in
      Alcotest.(check bool) "producer started" true !producer_started;
      (try
         Eio.Time.with_timeout_exn clock lifecycle_timeout (fun () ->
             Eio.Promise.await producer_finished)
       with Eio.Time.Timeout ->
         Alcotest.fail
           "producer was still alive after Runtime.with_response returned");
      match result with
      | Error error ->
          Alcotest.failf "unexpected runtime error: %a" Awskit.Error.pp error
      | Ok (response, body) ->
          Alcotest.(check int) "status" 403 (Awskit.Response.status response);
          Alcotest.(check string) "error body" error_body body)

let test_stream_request_body_success_response_body_is_scoped env =
  let response_body = String.make (128 * 1024) 'r' in
  with_eio_early_response_server env ~status:200 ~response_body
    ~read_request_body:false (fun endpoint ->
      Eio.Switch.run @@ fun conn_sw ->
      let conn = request_conn env conn_sw in
      let request = request_body_request_for_endpoint endpoint in
      let body =
        Awskit_eio.Runtime.Request_body.of_stream (stream_descriptor 2L)
          ~write:(fun writer ->
            Awskit_eio.Runtime.Request_body.write_string writer "ok")
      in
      let result =
        Awskit_eio.Runtime.Transport.with_response conn request ~body
          ~consume:(fun response response_body ->
            match read_response_body_to_string response_body with
            | Ok body -> Ok (response, body)
            | Error _ as error -> error)
      in
      match result with
      | Error error ->
          Alcotest.failf "unexpected runtime error: %a" Awskit.Error.pp error
      | Ok (response, body) ->
          Alcotest.(check int) "status" 200 (Awskit.Response.status response);
          Alcotest.(check int)
            "response length"
            (String.length response_body)
            (String.length body);
          Alcotest.(check string) "response body" response_body body)

let test_source_request_body_attempt_closes_after_callback_exception env =
  let callback_exn = Failure "source body callback exploded" in
  with_eio_stalled_response_server env (fun ~client_closed endpoint ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let conn = request_conn env sw in
      let request = request_body_request_for_endpoint endpoint in
      let observed =
        try
          ignore
            (Awskit_eio.Runtime.Transport.with_response conn request
               ~body:Awskit_eio.Runtime.Request_body.empty
               ~consume:(fun _response _body -> raise callback_exn)
              : (unit, Awskit.Error.t) Result.t);
          `Returned
        with exn -> `Raised exn
      in
      (match observed with
      | `Raised exn when Stdlib.( == ) exn callback_exn -> ()
      | `Raised exn ->
          Alcotest.failf "unexpected exception: %s" (Exn.to_string exn)
      | `Returned -> Alcotest.fail "expected callback exception");
      try
        Eio.Time.with_timeout_exn clock 0.5 (fun () ->
            Eio.Promise.await client_closed)
      with Eio.Time.Timeout ->
        Alcotest.fail
          "source-body attempt kept response connection alive after callback \
           exception")

let test_callback_exception_is_not_transport_error env =
  let callback_exn = Failure "callback exploded" in
  with_eio_early_response_server env ~status:200 ~response_body:"ok"
    ~read_request_body:false (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn env sw in
      let request = request_body_request_for_endpoint endpoint in
      let body = Awskit_eio.Runtime.Request_body.of_string "ok" in
      try
        ignore
          (Awskit_eio.Runtime.Transport.with_response conn request ~body
             ~consume:(fun _response _body -> raise callback_exn)
            : (unit, Awskit.Error.t) Result.t);
        Alcotest.fail "expected callback exception"
      with
      | exn when Stdlib.( == ) exn callback_exn -> ()
      | exn -> Alcotest.failf "unexpected exception: %s" (Exn.to_string exn))

let expect_body_limit label expected = function
  | Error error when Option.equal Int64.equal (body_limit error) (Some expected)
    ->
      let limit = Option.value_exn (body_limit error) in
      Alcotest.(check int64) label expected limit
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected body limit error" label

let with_eio_response_body env ~max_response_drain_bytes response_body ~f =
  with_eio_early_response_server env ~status:200 ~response_body
    ~read_request_body:false (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let conn = request_conn ~max_response_drain_bytes env sw in
      let request = request_body_request_for_endpoint endpoint in
      Awskit_eio.Runtime.Transport.with_response conn request
        ~body:Awskit_eio.Runtime.Request_body.empty ~consume:(fun _ body ->
          f body))

let test_discard_response_body_enforces_limit env =
  with_eio_response_body env ~max_response_drain_bytes:3 "abcdef"
    ~f:Awskit_eio.Runtime.Response_body.discard
  |> expect_body_limit "discard limit" 3L

let test_with_response_body_drain_enforces_limit env =
  with_eio_response_body env ~max_response_drain_bytes:3 "abcdef"
    ~f:(fun body ->
      Awskit_eio.Runtime.Response_body.with_reader body ~consume:(fun _ ->
          Ok ()))
  |> expect_body_limit "scoped drain limit" 3L

let test_with_response_body_preserves_consumer_error env =
  let consumer_error = Awskit.Error.Producer.body "consumer failed" in
  match
    with_eio_response_body env ~max_response_drain_bytes:3 "abcdef"
      ~f:(fun body ->
        Awskit_eio.Runtime.Response_body.with_reader body ~consume:(fun _ ->
            Error consumer_error))
  with
  | Error error when Awskit.Error.equal error consumer_error -> ()
  | Error error ->
      Alcotest.failf "expected consumer error, got: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "expected consumer error"

let test_with_response_body_drains_after_consumer_exception env =
  let exception Consumer_failed in
  let response_body = String.make (8 * 1024 * 1024) 'd' in
  let response_sent, wake_response_sent = Eio.Promise.create () in
  let resolve_response_sent () =
    ignore (Eio.Promise.try_resolve wake_response_sent () : bool)
  in
  with_eio_early_response_server env ~status:200 ~response_body
    ~read_request_body:false ~on_response_sent:resolve_response_sent
    (fun endpoint ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let conn =
        request_conn
          ~max_response_drain_bytes:(String.length response_body)
          env sw
      in
      let request = request_body_request_for_endpoint endpoint in
      let observed =
        try
          ignore
            (Awskit_eio.Runtime.Transport.with_response conn request
               ~body:Awskit_eio.Runtime.Request_body.empty
               ~consume:(fun _ body ->
                 Awskit_eio.Runtime.Response_body.with_reader body
                   ~consume:(fun _reader -> raise Consumer_failed))
              : (unit, Awskit.Error.t) Result.t);
          `Returned
        with exn -> `Raised exn
      in
      match observed with
      | `Raised exn when Stdlib.( == ) exn Consumer_failed -> (
          try
            Eio.Time.with_timeout_exn clock 1.0 (fun () ->
                Eio.Promise.await response_sent)
          with Eio.Time.Timeout ->
            Alcotest.fail "response body was not drained after exception")
      | `Raised exn ->
          Alcotest.failf "unexpected exception: %s" (Exn.to_string exn)
      | `Returned -> Alcotest.fail "expected consumer exception")

let test_response_body_reader_cannot_escape_scope env =
  let escaped = ref None in
  (match
     with_eio_response_body env ~max_response_drain_bytes:64 "abcdef"
       ~f:(fun body ->
         Awskit_eio.Runtime.Response_body.with_reader body
           ~consume:(fun reader ->
             escaped := Some reader;
             Ok ()))
   with
  | Ok () -> ()
  | Error error ->
      Alcotest.failf "unexpected with_reader error: %a" Awskit.Error.pp error);
  let reader =
    match !escaped with
    | Some reader -> reader
    | None -> Alcotest.fail "expected escaped reader"
  in
  let bytes = Bytes.create 1 in
  match Awskit_eio.Runtime.Response_body.read reader bytes ~off:0 ~len:1 with
  | Error error when is_body_error error -> ()
  | Error error ->
      Alcotest.failf "unexpected read error: %a" Awskit.Error.pp error
  | Ok _ -> Alcotest.fail "escaped reader read succeeded"

let test_response_read_timeout_interrupts_drain_cleanup env =
  let drain_span = Ptime.Span.of_float_s 0.5 |> Option.value_exn in
  let timeout_policy =
    Awskit.Timeout.create_exn ~response_body:tiny_span ~drain:drain_span ()
  in
  with_eio_stalled_response_server env (fun ~client_closed:_ endpoint ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let conn = request_conn ~timeout_policy env sw in
      let request = request_body_request_for_endpoint endpoint in
      let observed =
        try
          Eio.Time.with_timeout_exn clock 0.2 (fun () ->
              Awskit_eio.Runtime.Transport.with_response conn request
                ~body:Awskit_eio.Runtime.Request_body.empty
                ~consume:(fun _ body ->
                  Awskit_eio.Runtime.Response_body.with_reader body
                    ~consume:(fun reader ->
                      let bytes = Bytes.create 1 in
                      Awskit_eio.Runtime.Response_body.read reader bytes ~off:0
                        ~len:1)))
          |> fun result -> `Returned result
        with
        | Eio.Time.Timeout -> `Timed_out
        | exn -> `Raised exn
      in
      match observed with
      | `Returned (Error error) when is_timeout_error error -> ()
      | `Returned (Error error) ->
          Alcotest.failf "expected timeout error, got: %a" Awskit.Error.pp error
      | `Returned (Ok _) -> Alcotest.fail "expected response body timeout"
      | `Raised exn ->
          Alcotest.failf "unexpected exception: %s" (Exn.to_string exn)
      | `Timed_out ->
          Alcotest.fail "response body timeout waited for drain cleanup")

let test_response_read_cancellation_skips_drain_cleanup env =
  let drain_span = Ptime.Span.of_float_s 0.5 |> Option.value_exn in
  let timeout_policy = Awskit.Timeout.create_exn ~drain:drain_span () in
  with_eio_stalled_response_server env (fun ~client_closed:_ endpoint ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let conn = request_conn ~timeout_policy env sw in
      let request = request_body_request_for_endpoint endpoint in
      let started = Eio.Time.now clock in
      let observed =
        try
          Eio.Time.with_timeout_exn clock 1.0 (fun () ->
              ignore
                (Awskit_eio.Runtime.Transport.with_response conn request
                   ~body:Awskit_eio.Runtime.Request_body.empty
                   ~consume:(fun _ body ->
                     Awskit_eio.Runtime.Response_body.with_reader body
                       ~consume:(fun _reader ->
                         raise (Eio.Cancel.Cancelled Stdlib.Exit)))
                  : (unit, Awskit.Error.t) Result.t));
          `Returned
        with
        | Eio.Cancel.Cancelled _ -> `Canceled
        | Eio.Time.Timeout -> `Timed_out
        | exn -> `Raised exn
      in
      let elapsed = Eio.Time.now clock -. started in
      match observed with
      | `Canceled ->
          Alcotest.(check bool)
            "cancellation returned before drain timeout" true
            Float.(elapsed < 0.2)
      | `Raised exn ->
          Alcotest.failf "unexpected exception: %s" (Exn.to_string exn)
      | `Timed_out ->
          Alcotest.fail "response body cancellation waited for drain cleanup"
      | `Returned -> Alcotest.fail "expected response body cancellation")

let suite env =
  [
    ( "integration:awskit-eio:connection",
      [
        Alcotest.test_case "roundtrip" `Quick (fun () ->
            test_connection_roundtrip env);
        Alcotest.test_case "defaults" `Quick (fun () ->
            test_connection_defaults env);
        Alcotest.test_case "rejects invalid region string" `Quick (fun () ->
            test_create_rejects_invalid_region_string env);
        Alcotest.test_case "rejects invalid endpoint string" `Quick (fun () ->
            test_create_rejects_invalid_endpoint_string env);
        Alcotest.test_case "rejects invalid response drain limit" `Quick
          (fun () -> test_create_rejects_invalid_response_drain_limit env);
        Alcotest.test_case "uses env clock by default" `Quick (fun () ->
            test_connection_uses_env_clock_by_default env);
        Alcotest.test_case "runtime bodies" `Quick (fun () ->
            test_runtime_bodies env);
        Alcotest.test_case "HTTPS request requires connector" `Quick (fun () ->
            test_https_request_requires_connector env);
        Alcotest.test_case "listener bind sandbox errors are skippable" `Quick
          test_listener_bind_denied_by_sandbox;
        Alcotest.test_case "stream request body emits multiple chunks" `Quick
          (fun () -> test_stream_request_body_emits_multiple_chunks env);
        Alcotest.test_case "stream request body error propagates" `Quick
          (fun () -> test_stream_request_body_error_propagates env);
        Alcotest.test_case "stream request body escaped exception propagates"
          `Quick (fun () ->
            test_stream_request_body_escaped_exception_propagates env);
        Alcotest.test_case "stream request body timeout" `Quick (fun () ->
            test_stream_request_body_timeout_returns_timeout_error env);
        Alcotest.test_case "stream request body rejects short body" `Quick
          (fun () -> test_stream_request_body_rejects_short_body env);
        Alcotest.test_case "stream request body rejects long body" `Quick
          (fun () -> test_stream_request_body_rejects_long_body env);
        Alcotest.test_case
          "stream request body early response cancels producer and preserves \
           body"
          `Quick (fun () ->
            test_stream_request_body_early_response_cancels_producer_and_preserves_body
              env);
        Alcotest.test_case "stream request body success response body is scoped"
          `Quick (fun () ->
            test_stream_request_body_success_response_body_is_scoped env);
        Alcotest.test_case
          "source request body attempt closes after callback exception" `Quick
          (fun () ->
            test_source_request_body_attempt_closes_after_callback_exception env);
        Alcotest.test_case "callback exception is not transport error" `Quick
          (fun () -> test_callback_exception_is_not_transport_error env);
        Alcotest.test_case "response read timeout interrupts drain cleanup"
          `Quick (fun () ->
            test_response_read_timeout_interrupts_drain_cleanup env);
        Alcotest.test_case "response read cancellation skips drain cleanup"
          `Quick (fun () ->
            test_response_read_cancellation_skips_drain_cleanup env);
        Alcotest.test_case "does not read response body past EOF" `Quick
          (fun () -> test_no_read_past_response_eof env);
        Alcotest.test_case "head response is treated as bodiless" `Quick
          (fun () -> test_head_response_is_bodiless env);
        Alcotest.test_case "discard body limit" `Quick (fun () ->
            test_discard_response_body_enforces_limit env);
        Alcotest.test_case "scoped drain body limit" `Quick (fun () ->
            test_with_response_body_drain_enforces_limit env);
        Alcotest.test_case "consumer error wins over drain error" `Quick
          (fun () -> test_with_response_body_preserves_consumer_error env);
        Alcotest.test_case "consumer exception still drains body" `Quick
          (fun () ->
            test_with_response_body_drains_after_consumer_exception env);
        Alcotest.test_case "response body reader cannot escape scope" `Quick
          (fun () -> test_response_body_reader_cannot_escape_scope env);
      ] );
  ]
