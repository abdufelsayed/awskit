open Base

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let listener_bind_denied_by_sandbox = function
  (* opam-repository macOS CI can deny local TCP listeners under sandbox.sh. *)
  | Unix.Unix_error (Unix.EPERM, "bind", _) -> true
  | _ -> false

let rec read_headers input =
  match Eio.Buf_read.line input with "" -> () | _ -> read_headers input

let write_response output scenario =
  let connection =
    if scenario.Runtime_http_contract.keep_connection_open then "keep-alive"
    else "close"
  in
  Eio.Buf_write.string output
    (Fmt.str "HTTP/1.1 %d test\r\n" scenario.response_status);
  List.iter scenario.response_headers ~f:(fun (name, value) ->
      Eio.Buf_write.string output (Fmt.str "%s: %s\r\n" name value));
  Eio.Buf_write.string output (Fmt.str "Connection: %s\r\n\r\n" connection);
  Eio.Buf_write.string output scenario.response_bytes;
  Eio.Buf_write.flush output

let with_loopback_server env scenario test =
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
  let request_method, resolve_request_method = Eio.Promise.create () in
  let release_server () =
    ignore (Eio.Promise.try_resolve resolve_hold_open () : bool)
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Net.accept_fork listening_socket ~sw
        ~on_error:(fun _ -> ())
        (fun flow _addr ->
          let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
          let request_line = Eio.Buf_read.line input in
          let observed_method =
            match String.split request_line ~on:' ' with
            | method_ :: _ -> method_
            | [] -> Alcotest.fail "missing HTTP request line"
          in
          ignore
            (Eio.Promise.try_resolve resolve_request_method observed_method
              : bool);
          read_headers input;
          Eio.Buf_write.with_flow flow (fun output ->
              write_response output scenario;
              if scenario.keep_connection_open then Eio.Promise.await hold_open)));
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  match test endpoint with
  | result ->
      release_server ();
      Alcotest.(check string)
        "request method"
        (Awskit.Request.Method.to_string scenario.request_method)
        (Eio.Promise.await request_method);
      result
  | exception exn ->
      release_server ();
      raise exn

let request_for_endpoint scenario endpoint =
  let target =
    Awskit.Request.Target.create_exn
      ~scheme:(Awskit.Endpoint.scheme endpoint)
      ~host:(Awskit.Endpoint.host endpoint)
      ?port:(Awskit.Endpoint.port endpoint)
      ~path:"/" ()
  in
  Awskit.Request.create_exn
    ~method_:scenario.Runtime_http_contract.request_method ~target ()

let request_conn env sw =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"us-east-1"
    ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ()
  |> conn_or_fail

let rec read_all reader buffer =
  let chunk = Bytes.create 2 in
  match
    Awskit_eio.Runtime.Response_body.read reader chunk ~off:0
      ~len:(Bytes.length chunk)
  with
  | Error error ->
      Alcotest.failf "unexpected response body read error: %a" Awskit.Error.pp
        error
  | Ok 0 -> Buffer.contents buffer
  | Ok n ->
      Buffer.add_substring buffer (Bytes.to_string chunk) ~pos:0 ~len:n;
      read_all reader buffer

let read_body_to_string body =
  Awskit_eio.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      Ok (read_all reader (Buffer.create 16)))

let run_with_guard env scenario consume =
  let clock = Eio.Stdenv.clock env in
  with_loopback_server env scenario (fun endpoint ->
      Eio.Time.with_timeout_exn clock 1.0 (fun () ->
          Eio.Switch.run @@ fun sw ->
          let conn = request_conn env sw in
          let request = request_for_endpoint scenario endpoint in
          Awskit_eio.Runtime.Transport.with_response conn request
            ~body:Awskit_eio.Runtime.Request_body.empty
            ~consume:(fun response response_body ->
              Alcotest.(check int)
                "response status" scenario.response_status
                (Awskit.Response.status response);
              consume response_body)))

let expect_ok label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let test_read_scenario env scenario =
  match
    try run_with_guard env scenario read_body_to_string
    with Eio.Time.Timeout -> Alcotest.fail "response body read timed out"
  with
  | result ->
      let body = expect_ok "response body read failed" result in
      Alcotest.(check string) "response body" scenario.expected_body body

let test_discard_scenario env scenario =
  match
    try run_with_guard env scenario Awskit_eio.Runtime.Response_body.discard
    with Eio.Time.Timeout -> Alcotest.fail "response body discard timed out"
  with
  | result -> ignore (expect_ok "response body discard failed" result : unit)

let suite env =
  [
    ( "runtime-http-contract",
      List.concat
        [
          List.map Runtime_http_contract.scenarios ~f:(fun scenario ->
              Alcotest.test_case (scenario.name ^ " read") `Quick (fun () ->
                  test_read_scenario env scenario));
          List.map Runtime_http_contract.bodiless_scenarios ~f:(fun scenario ->
              Alcotest.test_case (scenario.name ^ " discard") `Quick (fun () ->
                  test_discard_scenario env scenario));
        ] );
  ]
