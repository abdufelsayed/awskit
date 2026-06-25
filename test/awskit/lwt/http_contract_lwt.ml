open Base
module Aws = Awskit_lwt.Make (Cohttp_lwt_unix.Client)

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let close_socket fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
    (function
      | Unix.Unix_error (Unix.EBADF, _, _) | Lwt.Canceled -> Lwt.return_unit
      | exn -> Lwt.fail exn)

let listener_bind_denied_by_sandbox = function
  (* opam-repository macOS CI can deny local TCP listeners under sandbox.sh. *)
  | Unix.Unix_error (Unix.EPERM, "bind", _) -> true
  | _ -> false

let read_line fd =
  let buffer = Buffer.create 64 in
  let byte = Stdlib.Bytes.create 1 in
  let rec loop previous =
    Lwt.bind (Lwt_unix.read fd byte 0 1) (fun read ->
        if read = 0 then Lwt.fail End_of_file
        else
          let ch = Stdlib.Bytes.get byte 0 in
          if Char.equal previous '\r' && Char.equal ch '\n' then
            let line = Buffer.contents buffer in
            let length = String.length line in
            Lwt.return (String.sub line ~pos:0 ~len:(length - 1))
          else (
            Buffer.add_char buffer ch;
            loop ch))
  in
  loop '\000'

let rec read_headers fd =
  Lwt.bind (read_line fd) (function
    | "" -> Lwt.return_unit
    | _ -> read_headers fd)

let write_all fd value =
  let bytes = Stdlib.Bytes.of_string value in
  let length = Stdlib.Bytes.length bytes in
  let rec loop offset =
    if offset = length then Lwt.return_unit
    else
      Lwt.bind
        (Lwt_unix.write fd bytes offset (length - offset))
        (fun written ->
          if written = 0 then Lwt.fail End_of_file else loop (offset + written))
  in
  loop 0

let response_wire scenario =
  let connection =
    if scenario.Runtime_http_contract.keep_connection_open then "keep-alive"
    else "close"
  in
  let headers =
    List.map scenario.response_headers ~f:(fun (name, value) ->
        Printf.sprintf "%s: %s\r\n" name value)
    |> String.concat ~sep:""
  in
  Printf.sprintf "HTTP/1.1 %d test\r\n%sConnection: %s\r\n\r\n%s"
    scenario.response_status headers connection scenario.response_bytes

let with_loopback_server scenario f =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  Lwt.catch
    (fun () ->
      Lwt.bind
        (Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)))
        (fun () ->
          Lwt_unix.listen socket 1;
          let endpoint =
            match Lwt_unix.getsockname socket with
            | Unix.ADDR_INET (_, port) ->
                Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port ()
            | Unix.ADDR_UNIX _ -> Alcotest.fail "unexpected Unix socket"
          in
          let hold_open, resolve_hold_open = Lwt.task () in
          let request_method, resolve_request_method = Lwt.task () in
          let release_server () = Lwt.wakeup_later resolve_hold_open () in
          let server =
            Lwt.catch
              (fun () ->
                Lwt.bind (Lwt_unix.accept socket) (fun (client, _) ->
                    Lwt.finalize
                      (fun () ->
                        Lwt.bind (read_line client) (fun request_line ->
                            let observed_method =
                              match String.split request_line ~on:' ' with
                              | method_ :: _ -> method_
                              | [] -> Alcotest.fail "missing HTTP request line"
                            in
                            Lwt.wakeup_later resolve_request_method
                              observed_method;
                            Lwt.bind (read_headers client) (fun () ->
                                Lwt.bind
                                  (write_all client (response_wire scenario))
                                  (fun () ->
                                    if scenario.keep_connection_open then
                                      hold_open
                                    else Lwt.return_unit))))
                      (fun () -> close_socket client)))
              (function
                | Unix.Unix_error (Unix.EBADF, _, _) | Lwt.Canceled ->
                    Lwt.return_unit
                | exn -> Lwt.fail exn)
          in
          Lwt.finalize
            (fun () ->
              Lwt.finalize
                (fun () ->
                  Lwt.bind (f endpoint) (fun result ->
                      Lwt.bind request_method (fun observed_method ->
                          Alcotest.(check string)
                            "request method"
                            (Awskit.Request.Method.to_string
                               scenario.request_method)
                            observed_method;
                          Lwt.return result)))
                (fun () ->
                  release_server ();
                  Lwt.return_unit))
            (fun () ->
              Lwt.cancel server;
              Lwt.bind (close_socket socket) (fun () ->
                  Lwt.catch
                    (fun () -> server)
                    (function
                      | Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn)))))
    (fun exn ->
      Lwt.bind (close_socket socket) (fun () ->
          if listener_bind_denied_by_sandbox exn then Alcotest.skip ()
          else Lwt.fail exn))

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

let request_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Aws.create ~region:"us-east-1" ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ()
  |> conn_or_fail

let rec read_all reader buffer =
  let chunk = Bytes.create 2 in
  Lwt.bind
    (Aws.Runtime.Response_body.read reader chunk ~off:0
       ~len:(Bytes.length chunk))
    (function
      | Error error ->
          Alcotest.failf "unexpected response body read error: %a"
            Awskit.Error.pp error
      | Ok 0 -> Lwt.return (Buffer.contents buffer)
      | Ok n ->
          Buffer.add_substring buffer (Bytes.to_string chunk) ~pos:0 ~len:n;
          read_all reader buffer)

let read_body_to_string body =
  Aws.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      Lwt.map Result.return (read_all reader (Buffer.create 16)))

let run_with_guard scenario consume =
  Lwt_unix.with_timeout 1.0 (fun () ->
      with_loopback_server scenario (fun endpoint ->
          let conn = request_conn () in
          let request = request_for_endpoint scenario endpoint in
          Aws.Runtime.Transport.with_response conn request
            ~body:Aws.Runtime.Request_body.empty ~consume:(fun response body ->
              Alcotest.(check int)
                "response status" scenario.response_status
                (Awskit.Response.status response);
              consume body)))

let expect_ok label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let test_read_scenario scenario =
  let result =
    try Lwt_main.run (run_with_guard scenario read_body_to_string)
    with Lwt_unix.Timeout -> Alcotest.fail "response body read timed out"
  in
  let body = expect_ok "response body read failed" result in
  Alcotest.(check string) "response body" scenario.expected_body body

let test_discard_scenario scenario =
  let result =
    try Lwt_main.run (run_with_guard scenario Aws.Runtime.Response_body.discard)
    with Lwt_unix.Timeout -> Alcotest.fail "response body discard timed out"
  in
  ignore (expect_ok "response body discard failed" result : unit)

let suite =
  [
    ( "runtime-http-contract",
      List.concat
        [
          List.map Runtime_http_contract.scenarios ~f:(fun scenario ->
              Alcotest.test_case (scenario.name ^ " read") `Quick (fun () ->
                  test_read_scenario scenario));
          List.map Runtime_http_contract.bodiless_scenarios ~f:(fun scenario ->
              Alcotest.test_case (scenario.name ^ " discard") `Quick (fun () ->
                  test_discard_scenario scenario));
        ] );
  ]
