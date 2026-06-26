open Base
module Model = Runtime_http_model
module Aws = Awskit_lwt.Make (Cohttp_lwt_unix.Client)

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let workload_timeout = Ptime.Span.of_float_s 0.25 |> Option.value_exn

let timeout_policy =
  Awskit.Timeout.create_exn ~operation:workload_timeout
    ~response_body:workload_timeout ~drain:workload_timeout ()

let timeout_error message =
  Awskit.Error.Producer.timeout ~operation:"runtime http workload" message

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
  Printf.sprintf "HTTP/1.1 %d test\r\n%s\r\n%s" scenario.Model.status
    (Model.response_header_block scenario)
    (Model.body_for_framing scenario.framing)

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
          let observed_request_method = ref None in
          let release_server () = Lwt.wakeup_later resolve_hold_open () in
          let check_observed_request_method observed_method =
            Alcotest.(check string)
              "request method"
              (Model.method_to_string scenario.Model.method_)
              observed_method
          in
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
                            observed_request_method := Some observed_method;
                            Lwt.bind (read_headers client) (fun () ->
                                Lwt.bind
                                  (write_all client (response_wire scenario))
                                  (fun () ->
                                    match scenario.connection with
                                    | Close -> Lwt.return_unit
                                    | Keep_alive -> hold_open))))
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
                  Lwt.catch
                    (fun () ->
                      Lwt.bind (f endpoint) (fun result ->
                          (match (result, !observed_request_method) with
                          | Ok _, Some observed_method
                          | Error _, Some observed_method ->
                              check_observed_request_method observed_method
                          | Ok _, None ->
                              Alcotest.failf
                                "%s succeeded without observed request method"
                                scenario.Model.name
                          | Error _, None -> ());
                          Lwt.return result))
                    (fun exn ->
                      (match !observed_request_method with
                      | Some observed_method ->
                          check_observed_request_method observed_method
                      | None -> ());
                      Lwt.fail exn))
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
          if listener_bind_denied_by_sandbox exn then
            Loopback_policy.handle_bind_denied ()
          else Lwt.fail exn))

let request_for_endpoint scenario endpoint =
  let target =
    Awskit.Request.Target.create_exn
      ~scheme:(Awskit.Endpoint.scheme endpoint)
      ~host:(Awskit.Endpoint.host endpoint)
      ?port:(Awskit.Endpoint.port endpoint)
      ~path:"/" ()
  in
  Awskit.Request.create_exn ~method_:scenario.Model.method_ ~target ()

let request_conn () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Aws.create ~region:"us-east-1" ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled
    ~sleep:(fun span -> Lwt_unix.sleep (Ptime.Span.to_float_s span))
    ~random_float:(fun ~upper_bound:_ -> 0.0)
    ~timeout_policy ()
  |> conn_or_fail

let rec read_all reader buffer =
  let chunk = Bytes.create 3 in
  Lwt.bind
    (Aws.Runtime.Response_body.read reader chunk ~off:0
       ~len:(Bytes.length chunk))
    (function
      | Error _ as error -> Lwt.return error
      | Ok 0 -> Lwt.return_ok (Buffer.contents buffer)
      | Ok n ->
          Buffer.add_substring buffer (Bytes.to_string chunk) ~pos:0 ~len:n;
          read_all reader buffer)

let read_body_to_string body =
  Aws.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      read_all reader (Buffer.create 16))

let read_body_once n body =
  let n = Int.max 0 n in
  Aws.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      let bytes = Bytes.create n in
      Lwt.bind (Aws.Runtime.Response_body.read reader bytes ~off:0 ~len:n)
        (function
        | Error _ as error -> Lwt.return error
        | Ok read -> Lwt.return_ok (Stdlib.Bytes.sub_string bytes 0 read)))

let rec read_until_error reader =
  let bytes = Bytes.create 3 in
  Lwt.bind
    (Aws.Runtime.Response_body.read reader bytes ~off:0
       ~len:(Bytes.length bytes))
    (function
      | Error error -> Lwt.return_ok error
      | Ok 0 ->
          Lwt.return_error
            (Awskit.Error.Producer.body "expected response body read error")
      | Ok _ -> read_until_error reader)

let assert_reader_invalidated reader =
  let bytes = Bytes.create 1 in
  Lwt.bind (Aws.Runtime.Response_body.read reader bytes ~off:0 ~len:1) (function
    | Error error ->
        Alcotest.(check bool)
          "reader invalidated" true
          (String.is_substring
             (Awskit.Error.to_string_hum error)
             ~substring:"outside its scope");
        Lwt.return_ok ()
    | Ok _ ->
        Lwt.return_error
          (Awskit.Error.Producer.body
             "response body reader remained usable after read error"))

let is_content_length_underflow_error error =
  String.is_substring
    (Awskit.Error.to_string_hum error)
    ~substring:"ended before declared Content-Length"

let consume_body scenario body =
  match scenario.Model.consume with
  | Model.Read_all -> read_body_to_string body
  | Model.Read_once n -> read_body_once n body
  | Model.Drop_without_read -> Lwt.return_ok ""
  | Model.Raise_in_consume -> raise Stdlib.Exit

let run_with_guard scenario =
  Lwt_unix.with_timeout 0.75 (fun () ->
      with_loopback_server scenario (fun endpoint ->
          let conn = request_conn () in
          let request = request_for_endpoint scenario endpoint in
          Aws.Runtime.Transport.with_response conn request
            ~body:Aws.Runtime.Request_body.empty ~consume:(fun response body ->
              Alcotest.(check int)
                "response status" scenario.status
                (Awskit.Response.status response);
              consume_body scenario body)))

let run_scenario scenario =
  try
    match Lwt_main.run (run_with_guard scenario) with
    | Ok body -> Model.Observed_body body
    | Error error -> Model.Observed_error (Awskit.Error.to_string_hum error)
  with
  | Lwt_unix.Timeout ->
      Model.Observed_error
        (Awskit.Error.to_string_hum
           (timeout_error "runtime HTTP scenario timed out"))
  | Stdlib.Exit -> Model.Observed_exception

let reader_invalidation_scenario =
  Model.scenario ~name:"lwt-reader-invalidated-after-read-error" ~method_:`GET
    ~status:200
    ~framing:(Content_length { declared = 6; actual = "hello" })
    ~connection:Close ()

let run_reader_invalidation_check () =
  Lwt_unix.with_timeout 0.75 (fun () ->
      with_loopback_server reader_invalidation_scenario (fun endpoint ->
          let conn = request_conn () in
          let request =
            request_for_endpoint reader_invalidation_scenario endpoint
          in
          Lwt.bind
            (Aws.Runtime.Transport.with_response conn request
               ~body:Aws.Runtime.Request_body.empty
               ~consume:(fun _response response_body ->
                 Aws.Runtime.Response_body.with_reader response_body
                   ~consume:(fun reader ->
                     Lwt.bind (read_until_error reader) (function
                       | Error _ as error -> Lwt.return error
                       | Ok first_error ->
                           Lwt.bind (assert_reader_invalidated reader) (function
                             | Error _ as error -> Lwt.return error
                             | Ok () -> Lwt.return_error first_error)))))
            (function
              | Error error when is_content_length_underflow_error error ->
                  Lwt.return_ok ()
              | Error _ as error -> Lwt.return error
              | Ok () ->
                  Lwt.return_error
                    (Awskit.Error.Producer.body
                       "expected response body read error"))))

let test_reader_invalidated_after_read_error () =
  match Lwt_main.run (run_reader_invalidation_check ()) with
  | Ok () -> ()
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
  | exception Lwt_unix.Timeout ->
      Alcotest.fail "reader invalidation scenario timed out"

module Target = struct
  let name = "awskit-lwt"
  let run_scenario = run_scenario
end

module Workload = Runtime_http_workload.Make (Target)

let suite =
  List.map Workload.suite ~f:(fun (name, cases) ->
      if String.equal name "workload:awskit-lwt:runtime-http" then
        ( name,
          cases
          @ [
              Alcotest.test_case "read error invalidates reader" `Quick
                test_reader_invalidated_after_read_error;
            ] )
      else (name, cases))

let () = Alcotest.run "awskit-lwt" suite
