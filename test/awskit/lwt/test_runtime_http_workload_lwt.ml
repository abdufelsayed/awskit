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

let ensure_loopback_listener_available () =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  let bind_result =
    Lwt_main.run
      (Lwt.finalize
         (fun () ->
           Lwt.catch
             (fun () ->
               Lwt.bind
                 (Lwt_unix.bind socket
                    (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)))
                 (fun () -> Lwt.return (Ok ())))
             (fun exn -> Lwt.return (Error exn)))
         (fun () -> close_socket socket))
  in
  match bind_result with
  | Ok () -> ()
  | Error exn when listener_bind_denied_by_sandbox exn ->
      Loopback_policy.handle_bind_denied ()
  | Error exn -> raise exn

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

let request_conn ?observability () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Aws.create ~region:"us-east-1" ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled
    ~sleep:(fun span -> Lwt_unix.sleep (Ptime.Span.to_float_s span))
    ~random_float:(fun ~upper_bound:_ -> 0.0)
    ~timeout_policy ?observability ()
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
  let ensure_loopback_available = ensure_loopback_listener_available
  let run_scenario = run_scenario
end

module Workload = Runtime_http_workload.Make (Target)

let sensitive_transport_exception =
  "Authorization: AWS4-HMAC-SHA256 X-Amz-Signature=secret"

module Failing_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    Lwt.fail (Failure sensitive_transport_exception)
end

module Failing_aws = Awskit_lwt.Make (Failing_client)

exception Synchronous_producer_failure
exception Response_consumer_failure

module Synchronous_producer_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    Lwt.return (Cohttp.Response.make ~status:`OK (), Cohttp_lwt.Body.empty)
end

module Synchronous_producer_aws = Awskit_lwt.Make (Synchronous_producer_client)

let captured_static_body : Cohttp_lwt.Body.t option ref = ref None

module Static_body_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body ?chunked:_ _meth _uri =
    captured_static_body := body;
    Lwt.return (Cohttp.Response.make ~status:`OK (), Cohttp_lwt.Body.empty)
end

module Static_body_aws = Awskit_lwt.Make (Static_body_client)

let captured_early_request_body : Cohttp_lwt.Body.t option ref = ref None

module Early_response_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body ?chunked:_ _meth _uri =
    captured_early_request_body := body;
    Lwt.return
      ( Cohttp.Response.make ~status:`Too_many_requests
          ~encoding:(Cohttp.Transfer.Fixed 0L) (),
        Cohttp_lwt.Body.empty )
end

module Early_response_aws = Awskit_lwt.Make (Early_response_client)

let producer_failure_response_chunks = ref 0

let response_stream ~chunks ~chunks_counter =
  let chunks = ref chunks in
  Lwt_stream.from (fun () ->
      match !chunks with
      | [] -> Lwt.return_none
      | chunk :: remaining ->
          (* Count connector-delivered data chunks, not the stream's terminal
             [None] sentinel. The latter is framing bookkeeping, not another
             physical response pull. *)
          Int.incr chunks_counter;
          chunks := remaining;
          Lwt.return_some chunk)
  |> Cohttp_lwt.Body.of_stream

module Producer_failure_response_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    let response =
      Cohttp.Response.make ~status:`OK ~encoding:(Cohttp.Transfer.Fixed 5L)
        ~headers:(Cohttp.Header.of_list [ ("Connection", "keep-alive") ])
        ()
    in
    Lwt.return
      ( response,
        response_stream ~chunks:[ "hello" ]
          ~chunks_counter:producer_failure_response_chunks )
end

module Producer_failure_response_aws =
  Awskit_lwt.Make (Producer_failure_response_client)

let malformed_framing_response_chunks = ref 0
let consumer_failure_response_chunks = ref 0
let canceled_response_chunks = ref 0
let canceled_response_abort_calls = ref 0

module Canceled_response_connector = struct
  module Client = Cohttp_lwt_unix.Client

  type call = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  let call ?ctx:_ ~headers:_ ~body:_ _meth _uri =
    let response =
      Cohttp.Response.make ~status:`OK ~encoding:(Cohttp.Transfer.Fixed 5L) ()
    in
    let delivered = ref false in
    let stream =
      Lwt_stream.from (fun () ->
          if not !delivered then (
            delivered := true;
            Int.incr canceled_response_chunks;
            Lwt.return_some "hello")
          else Lwt.fail Lwt.Canceled)
    in
    Lwt.return (response, Cohttp_lwt.Body.of_stream stream)

  let response call = call

  let abort _call =
    Int.incr canceled_response_abort_calls;
    Lwt.return_unit
end

module Canceled_response_aws =
  Awskit_lwt.For_connector.Make (Canceled_response_connector)

module Consumer_failure_response_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    let response =
      Cohttp.Response.make ~status:`OK ~encoding:(Cohttp.Transfer.Fixed 5L)
        ~headers:(Cohttp.Header.of_list [ ("Connection", "keep-alive") ])
        ()
    in
    Lwt.return
      ( response,
        response_stream ~chunks:[ "hello" ]
          ~chunks_counter:consumer_failure_response_chunks )
end

module Consumer_failure_response_aws =
  Awskit_lwt.Make (Consumer_failure_response_client)

let connector_abort_calls = ref 0
let connector_abort_completed = ref false

module Abort_connector = struct
  module Client = Cohttp_lwt_unix.Client

  type call = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  let call ?ctx ~headers ~body meth uri =
    Consumer_failure_response_client.call ?ctx ~headers ~body meth uri

  let response call = call

  let abort _call =
    Int.incr connector_abort_calls;
    connector_abort_completed := true;
    Lwt.return_unit
end

module Abort_aws = Awskit_lwt.For_connector.Make (Abort_connector)

let delayed_abort_started = ref false
let delayed_abort_completed = ref false
let delayed_abort_promise : unit Lwt.t ref = ref Lwt.return_unit

module Delayed_abort_connector = struct
  module Client = Cohttp_lwt_unix.Client

  type call = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  let call ?ctx ~headers ~body meth uri =
    Abort_connector.call ?ctx ~headers ~body meth uri

  let response call = call

  let abort _call =
    Int.incr connector_abort_calls;
    delayed_abort_started := true;
    Lwt.bind !delayed_abort_promise (fun () ->
        delayed_abort_completed := true;
        Lwt.return_unit)
end

module Delayed_abort_aws =
  Awskit_lwt.For_connector.Make (Delayed_abort_connector)

let incremental_chunked : bool option option ref = ref None
let incremental_request_chunks = ref 0

module Incremental_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body ?chunked _meth _uri =
    incremental_chunked := Some chunked;
    let stream =
      Option.value body ~default:Cohttp_lwt.Body.empty
      |> Cohttp_lwt.Body.to_stream
    in
    Lwt.bind (Lwt_stream.get stream) (fun _ ->
        Int.incr incremental_request_chunks;
        Lwt.return (Cohttp.Response.make ~status:`OK (), Cohttp_lwt.Body.empty))
end

module Incremental_aws = Awskit_lwt.Make (Incremental_client)

let exact_length_eof_closes = ref 0
let exact_length_abort_calls = ref 0

module Exact_length_connector = struct
  module Client = Cohttp_lwt_unix.Client

  type call = (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  let call ?ctx:_ ~headers:_ ~body:_ _meth _uri =
    let chunks = ref [ "abc" ] in
    let closed = ref false in
    let close_on_eof () =
      if not !closed then (
        closed := true;
        Int.incr exact_length_eof_closes)
    in
    let stream =
      Lwt_stream.from (fun () ->
          match !chunks with
          | [] ->
              close_on_eof ();
              Lwt.return_none
          | chunk :: remaining ->
              chunks := remaining;
              Lwt.return_some chunk)
    in
    let response =
      Cohttp.Response.make ~status:`OK ~encoding:(Cohttp.Transfer.Fixed 3L) ()
    in
    Lwt.return (response, Cohttp_lwt.Body.of_stream stream)

  let response call = call

  let abort _call =
    Int.incr exact_length_abort_calls;
    Lwt.return_unit
end

module Exact_length_aws = Awskit_lwt.For_connector.Make (Exact_length_connector)

let late_connect_started = ref false
let late_connect_closes = ref 0

module Late_connect_connector = struct
  module Client = Cohttp_lwt_unix.Client

  type connection = { mutable closed : bool }

  type call = {
    mutable response_promise :
      (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t option;
    connection : connection option ref;
    mutable abort_requested : bool;
  }

  let close connection =
    if not connection.closed then (
      connection.closed <- true;
      Int.incr late_connect_closes)

  let call ?ctx:_ ~headers:_ ~body:_ _meth _uri =
    late_connect_started := true;
    let owner =
      {
        response_promise = None;
        connection = ref None;
        abort_requested = false;
      }
    in
    let response_promise =
      Lwt.no_cancel
        (Lwt.bind (Lwt_unix.sleep 0.02) (fun () ->
             let connection = { closed = false } in
             owner.connection := Some connection;
             if owner.abort_requested then (
               close connection;
               Lwt.fail Lwt.Canceled)
             else
               Lwt.return
                 (Cohttp.Response.make ~status:`OK (), Cohttp_lwt.Body.empty)))
    in
    owner.response_promise <- Some response_promise;
    owner

  let response owner =
    match owner.response_promise with
    | Some response -> response
    | None -> Lwt.fail (Failure "late connector response requested too early")

  let abort owner =
    owner.abort_requested <- true;
    Option.iter !(owner.connection) ~f:close;
    Option.iter owner.response_promise ~f:Lwt.cancel;
    match owner.response_promise with
    | None -> Lwt.return_unit
    | Some response ->
        Lwt.catch
          (fun () -> Lwt.bind response (fun _ -> Lwt.return_unit))
          (fun _ -> Lwt.return_unit)
end

module Late_connect_aws = Awskit_lwt.For_connector.Make (Late_connect_connector)

module Malformed_framing_response_client = struct
  include Cohttp_lwt_unix.Client

  let call ?ctx:_ ?headers:_ ?body:_ ?chunked:_ _meth _uri =
    let response =
      Cohttp.Response.make ~status:`OK
        ~headers:
          (Cohttp.Header.of_list
             [
               ("Content-Length", "5");
               ("Transfer-Encoding", "chunked");
               ("Connection", "keep-alive");
             ])
        ()
    in
    Lwt.return
      ( response,
        response_stream ~chunks:[ "hello" ]
          ~chunks_counter:malformed_framing_response_chunks )
end

module Malformed_framing_response_aws =
  Awskit_lwt.Make (Malformed_framing_response_client)

let awskit_http_log_src () =
  match
    List.find (Logs.Src.list ()) ~f:(fun src ->
        String.equal (Logs.Src.name src) "awskit.http")
  with
  | Some src -> src
  | None -> Alcotest.fail "missing awskit.http log source"

let capture_awskit_http_completions f =
  let awskit_http_src = awskit_http_log_src () in
  let previous_reporter = Logs.reporter () in
  let previous_level = Logs.Src.level awskit_http_src in
  let messages = ref [] in
  let completions = ref [] in
  let reporter =
    {
      Logs.report =
        (fun src _level ~over k msgf ->
          let capture ?header:_ ?tags fmt =
            Stdlib.Format.kasprintf
              (fun message ->
                if String.equal (Logs.Src.name src) "awskit.http" then (
                  messages := message :: !messages;
                  Option.iter tags ~f:(fun tags ->
                      Option.iter
                        (Logs.Tag.find
                           Awskit.Observability.Logs_tags.operation_completion
                           tags) ~f:(fun completion ->
                          completions := completion :: !completions)));
                over ();
                k ())
              fmt
          in
          msgf capture);
    }
  in
  Exn.protect
    ~f:(fun () ->
      Logs.Src.set_level awskit_http_src (Some Logs.Debug);
      Logs.set_reporter reporter;
      let result = f () in
      (result, List.rev !messages, List.rev !completions))
    ~finally:(fun () ->
      Logs.set_reporter previous_reporter;
      Logs.Src.set_level awskit_http_src previous_level)

let test_transport_failure_log_is_redacted () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Failing_aws.create ~endpoint:"http://127.0.0.1:9000" ~region:"us-east-1"
      ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun span -> Lwt_unix.sleep (Ptime.Span.to_float_s span))
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy ()
    |> conn_or_fail
  in
  let request =
    let target =
      Awskit.Request.Target.create_exn ~scheme:`Http ~host:"127.0.0.1"
        ~port:9000 ~path:"/" ()
    in
    Awskit.Request.create_exn ~method_:`GET ~target ()
  in
  let result, messages, completions =
    capture_awskit_http_completions (fun () ->
        Lwt_main.run
          (Failing_aws.Runtime.Transport.with_response conn request
             ~body:Failing_aws.Runtime.Request_body.empty
             ~consume:(fun _response _body -> Lwt.return_ok ())))
  in
  (match result with
  | Error error ->
      Alcotest.(check bool)
        "transport error redacts public diagnostic" false
        (String.is_substring
           (Awskit.Error.to_string_hum error)
           ~substring:sensitive_transport_exception)
  | Ok () -> Alcotest.fail "transport exception should fail request");
  Alcotest.(check bool)
    "attempt completion captured" true
    (not (List.is_empty completions));
  List.iter messages ~f:(fun message ->
      Alcotest.(check bool)
        "log message redacts transport exception" false
        (String.is_substring message ~substring:sensitive_transport_exception));
  List.iter completions ~f:(fun completion ->
      let rendered =
        Fmt.str "%a" Awskit.Observability.For_projection.Operation.Completion.pp
          completion
      in
      Alcotest.(check bool)
        "typed log tag redacts transport exception" false
        (String.is_substring rendered ~substring:sensitive_transport_exception))

let test_synchronous_request_producer_is_observed () =
  let completions = ref [] in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"producer-capture"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.http.request_body.production"
          (Awskit.Observability.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_lwt.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observability =
    Awskit_lwt.Observability.create ~logs:false ~trace_sinks:[ sink ] ()
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Synchronous_producer_aws.create ~endpoint:"http://example.test"
      ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy ~observability ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`POST ~target () in
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:3L
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "abc")
      ~replayable:true ()
  in
  let result =
    Lwt_main.run
      (Synchronous_producer_aws.Runtime.Transport.with_response conn request
         ~body:
           (Synchronous_producer_aws.Runtime.Request_body.of_stream descriptor
              ~write:(fun _ -> raise Synchronous_producer_failure))
         ~consume:(fun _ _ -> Lwt.return_ok ()))
  in
  Alcotest.(check bool)
    "producer failure remains an SDK error" true (Result.is_error result);
  Alcotest.(check int)
    "production phase completed once" 1 (List.length !completions);
  Alcotest.(check string)
    "production phase outcome" "exception"
    (!completions
    |> List.hd_exn
    |> Awskit.Observability.For_projection.Operation.Completion.outcome
    |> Awskit.Observability.Outcome.to_string)

let measurement name completion =
  completion
  |> Awskit.Observability.For_projection.Operation.Completion.measurements
  |> List.find_map ~f:(fun value ->
      if
        String.equal name
          (Awskit.Observability.For_projection.Measurement.name value)
      then
        match Awskit.Observability.For_projection.Measurement.value value with
        | Int64 value -> Some value
        | Int value -> Some (Int64.of_int value)
        | Float _ -> None
      else None)

let streaming_snapshot_values observability direction =
  Awskit_lwt.Observability.instrument_snapshot observability
  |> List.filter_map ~f:(fun observation ->
      let module Metric = Awskit.Observability.For_projection.Metric in
      match
        ( List.map
            (Metric.Observation.labels observation)
            ~f:Metric.Label.encoded,
          Metric.Observation.value observation )
      with
      | [ observed_direction ], Int64 value
        when String.equal direction observed_direction ->
          Some value
      | _ -> None)

let test_large_early_response_stream_does_not_update_released_lease () =
  captured_early_request_body := None;
  let sink_calls = ref 0 in
  let metric_sink =
    Awskit.Observability.Metric_sink.create ~name:"early-response-streaming"
      ~needs_clock:false
      ~enabled:(fun family ->
        String.equal "awskit.http.streaming_bytes_in_flight"
          (Awskit.Observability.For_projection.Metric.Family.name family))
      ~observe:(fun _ -> Int.incr sink_calls)
  in
  let observability =
    Awskit_lwt.Observability.create ~logs:false ~metric_sinks:[ metric_sink ] ()
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Early_response_aws.create ~endpoint:"http://example.test"
      ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy ~observability ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`POST ~target () in
  let chunk = String.make 4096 'x' in
  let chunks = 512 in
  let descriptor =
    Awskit.Body.Request.descriptor_exn
      ~content_length:(Int64.of_int (String.length chunk * chunks))
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "")
      ~replayable:true ()
  in
  let rec write_many writer remaining =
    if remaining = 0 then Lwt.return_ok ()
    else
      Lwt.bind
        (Early_response_aws.Runtime.Request_body.write_string writer chunk)
        (fun result ->
          match result with
          | Error _ as error -> Lwt.return error
          | Ok () -> write_many writer (remaining - 1))
  in
  let result =
    Lwt_main.run
      (Lwt_unix.with_timeout 0.75 (fun () ->
           Early_response_aws.Runtime.Transport.with_response conn request
             ~body:
               (Early_response_aws.Runtime.Request_body.of_stream descriptor
                  ~write:(fun writer -> write_many writer chunks))
             ~consume:(fun _response _body -> Lwt.return_ok ())))
  in
  (match result with
  | Ok () -> ()
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error);
  let body =
    match !captured_early_request_body with
    | Some body -> body
    | None -> Alcotest.fail "early response client did not capture body"
  in
  let late_pull =
    Lwt_main.run
      (Lwt_unix.with_timeout 0.75 (fun () ->
           Lwt.catch
             (fun () ->
               Lwt.bind
                 (Lwt_stream.iter_s
                    (fun _ -> Lwt.return_unit)
                    (Cohttp_lwt.Body.to_stream body))
                 (fun () -> Lwt.return_ok ()))
             (fun exn -> Lwt.return_error exn)))
  in
  (match late_pull with
  | Ok () -> ()
  | Error exn ->
      Alcotest.failf "late connector pull raised after lease release: %s"
        (Exn.to_string exn));
  Alcotest.(check int) "streaming sink stayed off chunk path" 0 !sink_calls;
  List.iter [ "request"; "response" ] ~f:(fun direction ->
      let values = streaming_snapshot_values observability direction in
      Alcotest.(check bool)
        (direction ^ " streaming gauge has a snapshot")
        true
        (not (List.is_empty values));
      Alcotest.(check int64)
        (direction ^ " streaming gauge released at zero")
        0L (List.last_exn values))

let test_request_producer_failure_drains_non_empty_keep_alive_response () =
  producer_failure_response_chunks := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Producer_failure_response_aws.create ~endpoint:"http://example.test"
      ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`POST ~target () in
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:3L
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "abc")
      ~replayable:true ()
  in
  let result =
    Lwt_main.run
      (Lwt_unix.with_timeout 0.75 (fun () ->
           Producer_failure_response_aws.Runtime.Transport.with_response conn
             request
             ~body:
               (Producer_failure_response_aws.Runtime.Request_body.of_stream
                  descriptor ~write:(fun _ ->
                    raise Synchronous_producer_failure))
             ~consume:(fun _ _ ->
               Lwt.return_error
                 (Awskit.Error.Producer.body
                    "response consumer should not run after producer failure"))))
  in
  Alcotest.(check bool)
    "producer failure remains an SDK error" true (Result.is_error result);
  Alcotest.(check bool)
    "producer failure drains one data chunk from the keep-alive response" true
    (!producer_failure_response_chunks = 1)

let test_framing_validation_drains_non_empty_keep_alive_response () =
  malformed_framing_response_chunks := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Malformed_framing_response_aws.create ~endpoint:"http://example.test"
      ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`GET ~target () in
  let result =
    Lwt_main.run
      (Lwt_unix.with_timeout 0.75 (fun () ->
           Malformed_framing_response_aws.Runtime.Transport.with_response conn
             request
             ~body:Malformed_framing_response_aws.Runtime.Request_body.empty
             ~consume:(fun _ _ ->
               Lwt.return_error
                 (Awskit.Error.Producer.body
                    "response consumer should not run after framing failure"))))
  in
  Alcotest.(check bool)
    "framing validation remains an SDK error" true (Result.is_error result);
  Alcotest.(check bool)
    "framing validation drains one data chunk from the keep-alive response" true
    (!malformed_framing_response_chunks = 1)

let test_response_consumer_exception_drains_keep_alive_response () =
  consumer_failure_response_chunks := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Consumer_failure_response_aws.create ~endpoint:"http://example.test"
      ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`GET ~target () in
  let result =
    try
      Lwt_main.run
        (Lwt_unix.with_timeout 0.75 (fun () ->
             Consumer_failure_response_aws.Runtime.Transport.with_response conn
               request
               ~body:Consumer_failure_response_aws.Runtime.Request_body.empty
               ~consume:(fun _ body ->
                 Consumer_failure_response_aws.Runtime.Response_body.with_reader
                   body ~consume:(fun _ -> raise Response_consumer_failure))))
      |> fun _ -> `Returned
    with
    | Response_consumer_failure -> `Raised
    | Lwt_unix.Timeout -> `Timed_out
    | _ -> `Other
  in
  Alcotest.(check string)
    "consumer exception is preserved" "raised"
    (match result with
    | `Raised -> "raised"
    | `Returned -> "returned"
    | `Timed_out -> "timed out"
    | `Other -> "other");
  Alcotest.(check bool)
    "consumer exception drains one data chunk from the keep-alive response" true
    (!consumer_failure_response_chunks = 1)

let test_callback_exception_after_successful_reader_cleanup () =
  consumer_failure_response_chunks := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Consumer_failure_response_aws.create ~endpoint:"http://example.test"
      ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`GET ~target () in
  let result =
    try
      Lwt_main.run
        (Lwt_unix.with_timeout 0.75 (fun () ->
             Consumer_failure_response_aws.Runtime.Transport.with_response conn
               request
               ~body:Consumer_failure_response_aws.Runtime.Request_body.empty
               ~consume:(fun _ body ->
                 Lwt.bind
                   (Consumer_failure_response_aws.Runtime.Response_body
                    .with_reader body ~consume:(fun _ -> Lwt.return_ok ()))
                   (fun _ -> raise Response_consumer_failure))))
      |> fun _ -> `Returned
    with
    | Response_consumer_failure -> `Raised
    | Lwt_unix.Timeout -> `Timed_out
    | _ -> `Other
  in
  Alcotest.(check string)
    "post-reader callback exception is preserved" "raised"
    (match result with
    | `Raised -> "raised"
    | `Returned -> "returned"
    | `Timed_out -> "timed out"
    | `Other -> "other");
  Alcotest.(check int)
    "post-reader callback cleanup data chunks exactly once" 1
    !consumer_failure_response_chunks

let test_cancellation_during_body_cleanup_aborts_once () =
  canceled_response_chunks := 0;
  canceled_response_abort_calls := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Canceled_response_aws.create ~endpoint:"http://example.test"
      ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy:Awskit.Timeout.disabled ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`GET ~target () in
  let completion =
    Canceled_response_aws.Runtime.Transport.with_response conn request
      ~body:Canceled_response_aws.Runtime.Request_body.empty
      ~consume:(fun _ body ->
        Canceled_response_aws.Runtime.Response_body.with_reader body
          ~consume:(fun _ -> Lwt.return_ok ()))
  in
  let outcome =
    try `Returned (Lwt_main.run completion) with Lwt.Canceled -> `Canceled
  in
  let canceled =
    match outcome with `Canceled -> true | `Returned _ -> false
  in
  if not canceled then
    Alcotest.failf "body cancellation returned: %s"
      (match outcome with
      | `Canceled -> "canceled"
      | `Returned (Ok ()) -> "ok"
      | `Returned (Error error) -> Awskit.Error.to_string_hum error);
  Alcotest.(check bool) "body cancellation remains primary" true canceled;
  Alcotest.(check int)
    "canceled body data chunks exactly once before cancellation" 1
    !canceled_response_chunks;
  Alcotest.(check int)
    "canceled body aborts once" 1
    !canceled_response_abort_calls

let abort_connection () =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Abort_aws.create ~endpoint:"http://example.test" ~region:"us-east-1"
    ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled
    ~sleep:(fun _ -> Lwt.return_unit)
    ~random_float:(fun ~upper_bound:_ -> 0.0)
    ~timeout_policy:Awskit.Timeout.disabled ~max_response_drain_bytes:3 ()
  |> conn_or_fail

let abort_request () =
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  Awskit.Request.create_exn ~method_:`GET ~target ()

let test_discard_limit_aborts_before_return () =
  consumer_failure_response_chunks := 0;
  connector_abort_calls := 0;
  connector_abort_completed := false;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Abort_aws.create ~endpoint:"http://example.test" ~region:"us-east-1"
      ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy:Awskit.Timeout.disabled ~max_response_drain_bytes:3 ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`GET ~target () in
  let result =
    Lwt_main.run
      (Lwt_unix.with_timeout 0.75 (fun () ->
           Abort_aws.Runtime.Transport.with_response conn request
             ~body:Abort_aws.Runtime.Request_body.empty ~consume:(fun _ body ->
               Abort_aws.Runtime.Response_body.discard body)))
  in
  (match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "discard should report the configured body limit");
  Alcotest.(check bool)
    "discard limit awaits connector abort" true !connector_abort_completed;
  Alcotest.(check int) "connector abort is idempotent" 1 !connector_abort_calls;
  Alcotest.(check int)
    "bounded cleanup stops at the configured limit" 1
    !consumer_failure_response_chunks

let test_streaming_request_uses_incremental_client_call () =
  incremental_chunked := None;
  incremental_request_chunks := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Incremental_aws.create ~endpoint:"http://example.test" ~region:"us-east-1"
      ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy:Awskit.Timeout.disabled ()
    |> conn_or_fail
  in
  let request = abort_request () in
  let descriptor =
    Awskit.Body.Request.descriptor_exn ~content_length:3L
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string "abc")
      ~replayable:true ()
  in
  let result =
    Lwt_main.run
      (Incremental_aws.Runtime.Transport.with_response conn request
         ~body:
           (Incremental_aws.Runtime.Request_body.of_stream descriptor
              ~write:(fun writer ->
                Incremental_aws.Runtime.Request_body.write_string writer "abc"))
         ~consume:(fun _ _ -> Lwt.return_ok ()))
  in
  Alcotest.(check bool)
    "incremental request succeeds" true (Result.is_ok result);
  Alcotest.(check (option (option bool)))
    "generic client receives no chunked override" (Some None)
    !incremental_chunked;
  Alcotest.(check int)
    "client received one request chunk" 1
    !incremental_request_chunks

let test_primary_consumer_error_survives_abort () =
  consumer_failure_response_chunks := 0;
  connector_abort_calls := 0;
  connector_abort_completed := false;
  let conn = abort_connection () in
  let request = abort_request () in
  let primary = Awskit.Error.Producer.body "consumer-primary" in
  let result =
    Lwt_main.run
      (Abort_aws.Runtime.Transport.with_response conn request
         ~body:Abort_aws.Runtime.Request_body.empty ~consume:(fun _ body ->
           Abort_aws.Runtime.Response_body.with_reader body ~consume:(fun _ ->
               Lwt.return_error primary)))
  in
  (match result with
  | Error error ->
      Alcotest.(check string)
        "primary consumer error" "body: consumer-primary"
        (Awskit.Error.to_string_hum error)
  | Ok () -> Alcotest.fail "consumer error unexpectedly succeeded");
  Alcotest.(check bool)
    "consumer cleanup aborts before return" true !connector_abort_completed;
  Alcotest.(check int) "consumer cleanup abort count" 1 !connector_abort_calls

let test_callback_error_before_reader_drains_and_preserves_primary () =
  consumer_failure_response_chunks := 0;
  connector_abort_calls := 0;
  connector_abort_completed := false;
  let conn = abort_connection () in
  let request = abort_request () in
  let primary = Awskit.Error.Producer.body "callback-primary" in
  let result =
    Lwt_main.run
      (Abort_aws.Runtime.Transport.with_response conn request
         ~body:Abort_aws.Runtime.Request_body.empty ~consume:(fun _ _ ->
           Lwt.return_error primary))
  in
  (match result with
  | Error error ->
      Alcotest.(check string)
        "primary callback error" "body: callback-primary"
        (Awskit.Error.to_string_hum error)
  | Ok () -> Alcotest.fail "callback error unexpectedly succeeded");
  Alcotest.(check int)
    "callback-error cleanup pulls one data chunk" 1
    !consumer_failure_response_chunks;
  Alcotest.(check bool)
    "callback-error cleanup awaits connector abort" true
    !connector_abort_completed;
  Alcotest.(check int)
    "callback-error cleanup abort count" 1 !connector_abort_calls

let test_exact_length_response_releases_owner_at_eof () =
  exact_length_eof_closes := 0;
  exact_length_abort_calls := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Exact_length_aws.create ~endpoint:"http://example.test" ~region:"us-east-1"
      ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy:Awskit.Timeout.disabled ()
    |> conn_or_fail
  in
  let result =
    Lwt_main.run
      (Exact_length_aws.Runtime.Transport.with_response conn (abort_request ())
         ~body:Exact_length_aws.Runtime.Request_body.empty
         ~consume:(fun _ body ->
           Exact_length_aws.Runtime.Response_body.with_reader body
             ~consume:(fun _ -> Lwt.return_ok ())))
  in
  Alcotest.(check bool)
    "exact-length response succeeds" true (Result.is_ok result);
  Alcotest.(check int)
    "exact-length body reaches EOF before return" 1 !exact_length_eof_closes;
  Alcotest.(check int)
    "normal EOF does not use abort" 0 !exact_length_abort_calls

let test_abort_before_connect_closes_late_connection () =
  late_connect_started := false;
  late_connect_closes := 0;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Late_connect_aws.create ~endpoint:"http://example.test" ~region:"us-east-1"
      ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy:Awskit.Timeout.disabled ()
    |> conn_or_fail
  in
  let completion =
    Late_connect_aws.Runtime.Transport.with_response conn (abort_request ())
      ~body:Late_connect_aws.Runtime.Request_body.empty ~consume:(fun _ _ ->
        Lwt.return_ok ())
  in
  Lwt_main.run (Lwt_unix.sleep 0.001);
  Alcotest.(check bool) "in-flight call started" true !late_connect_started;
  Lwt.cancel completion;
  let cancelled =
    try
      ignore (Lwt_main.run completion);
      false
    with Lwt.Canceled -> true
  in
  Alcotest.(check bool) "native cancellation is preserved" true cancelled;
  Alcotest.(check int)
    "late connection closed before cancellation returns" 1 !late_connect_closes

let test_cancellation_survives_abort () =
  consumer_failure_response_chunks := 0;
  connector_abort_calls := 0;
  connector_abort_completed := false;
  let conn = abort_connection () in
  let request = abort_request () in
  let cancelled =
    try
      ignore
        (Lwt_main.run
           (Abort_aws.Runtime.Transport.with_response conn request
              ~body:Abort_aws.Runtime.Request_body.empty ~consume:(fun _ _ ->
                Lwt.fail Lwt.Canceled)));
      false
    with Lwt.Canceled -> true
  in
  Alcotest.(check bool) "native cancellation is preserved" true cancelled;
  Alcotest.(check bool)
    "cancellation cleanup aborts before return" true !connector_abort_completed;
  Alcotest.(check int)
    "cancellation cleanup abort count" 1 !connector_abort_calls

let test_cancellation_waits_for_delayed_abort () =
  consumer_failure_response_chunks := 0;
  connector_abort_calls := 0;
  delayed_abort_started := false;
  delayed_abort_completed := false;
  let delayed, resolve_delayed = Lwt.wait () in
  delayed_abort_promise := delayed;
  let conn =
    let credentials =
      Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK"
        ()
    in
    Delayed_abort_aws.create ~endpoint:"http://example.test" ~region:"us-east-1"
      ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy:Awskit.Timeout.disabled ~max_response_drain_bytes:3 ()
    |> conn_or_fail
  in
  let completion =
    Delayed_abort_aws.Runtime.Transport.with_response conn (abort_request ())
      ~body:Delayed_abort_aws.Runtime.Request_body.empty ~consume:(fun _ body ->
        Delayed_abort_aws.Runtime.Response_body.with_reader body
          ~consume:(fun _ -> Lwt.fail Lwt.Canceled))
  in
  let settled, wake_settled = Lwt.wait () in
  Lwt.on_any completion
    (fun value -> Lwt.wakeup_later wake_settled (`Returned value))
    (fun exn -> Lwt.wakeup_later_exn wake_settled exn);
  Lwt_main.run (Lwt_unix.sleep 0.01);
  Alcotest.(check bool) "delayed abort started" true !delayed_abort_started;
  Alcotest.(check bool)
    "cancellation waits for abort" true (Lwt.is_sleeping settled);
  Alcotest.(check bool) "abort has not completed" false !delayed_abort_completed;
  Lwt.wakeup_later resolve_delayed ();
  let cancelled =
    try
      ignore (Lwt_main.run settled);
      false
    with Lwt.Canceled -> true
  in
  Alcotest.(check bool) "native cancellation remains primary" true cancelled;
  Alcotest.(check bool)
    "delayed abort completed before return" true !delayed_abort_completed;
  Alcotest.(check int) "delayed abort count" 1 !connector_abort_calls

let test_static_request_body_preserves_representation () =
  captured_static_body := None;
  let completions = ref [] in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"static-body-capture"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.http.attempt"
          (Awskit.Observability.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_lwt.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  let conn =
    Static_body_aws.create ~endpoint:"http://example.test" ~region:"us-east-1"
      ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~sleep:(fun _ -> Lwt.return_unit)
      ~random_float:(fun ~upper_bound:_ -> 0.0)
      ~timeout_policy:Awskit.Timeout.disabled
      ~observability:
        (Awskit_lwt.Observability.create ~logs:false ~trace_sinks:[ sink ] ())
      ()
    |> conn_or_fail
  in
  let target =
    Awskit.Request.Target.create_exn ~scheme:`Http ~host:"example.test"
      ~path:"/" ()
  in
  let request = Awskit.Request.create_exn ~method_:`POST ~target () in
  let result =
    Lwt_main.run
      (Static_body_aws.Runtime.Transport.with_response conn request
         ~body:(Static_body_aws.Runtime.Request_body.of_string "static-body")
         ~consume:(fun _ _ -> Lwt.return_ok ()))
  in
  (match result with
  | Ok () -> ()
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error);
  (match !captured_static_body with
  | Some (`String body) ->
      Alcotest.(check string) "static body" "static-body" body
  | Some (`Strings bodies) ->
      Alcotest.failf "static body was converted to string list: %d chunks"
        (List.length bodies)
  | Some (`Stream _) ->
      Alcotest.fail "static body was converted to a streaming Cohttp body"
  | Some `Empty -> Alcotest.fail "client received an empty request body"
  | None -> Alcotest.fail "client did not receive a request body");
  let completion =
    match !completions with
    | [ completion ] -> completion
    | completions ->
        Alcotest.failf "expected one HTTP attempt completion, got %d"
          (List.length completions)
  in
  Alcotest.(check (option int64))
    "static connector request bytes are unmeasured" None
    (measurement "http.connector_request_bytes" completion)

let test_attempt_body_and_drain_bytes () =
  let completions = ref [] in
  let observed_operations =
    [
      "awskit.http.attempt";
      "awskit.http.request_body.production";
      "awskit.http.response_headers.wait";
      "awskit.http.response_body.consumption";
      "awskit.http.response_body.drain";
    ]
  in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"attempt-capture"
      ~needs_clock:true
      ~enabled:(fun info ->
        List.mem observed_operations
          (Awskit.Observability.For_projection.Operation.Info.name info)
          ~equal:String.equal)
      ~start:(fun _start ->
        {
          Awskit_lwt.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish = (fun completion -> completions := completion :: !completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let metric_sink =
    Awskit.Observability.Metric_sink.create ~name:"streaming-capture"
      ~needs_clock:false
      ~enabled:(fun family ->
        String.equal "awskit.http.streaming_bytes_in_flight"
          (Awskit.Observability.For_projection.Metric.Family.name family))
      ~observe:(fun _ -> Alcotest.fail "instrument sink ran on the body path")
  in
  let ticks = ref 0L in
  let observability =
    Awskit_lwt.Observability.create
      ~clock:(fun () ->
        let value = !ticks in
        ticks := Int64.succ value;
        value)
      ~metric_sinks:[ metric_sink ] ~trace_sinks:[ sink ] ()
  in
  let scenario =
    Model.scenario ~name:"observed-attempt-bytes" ~method_:`POST ~status:429
      ~framing:(Content_length { declared = 6; actual = "abcdef" })
      ~connection:Close ~consume:(Read_once 2) ()
  in
  let result =
    Lwt_main.run
      (Lwt_unix.with_timeout 0.75 (fun () ->
           with_loopback_server scenario (fun endpoint ->
               let conn = request_conn ~observability () in
               let request = request_for_endpoint scenario endpoint in
               let descriptor =
                 Awskit.Body.Request.descriptor_exn ~content_length:3L
                   ~payload_hash:
                     (Awskit.Body.Payload_hash.sha256_of_string "abc")
                   ~replayable:true ()
               in
               Aws.Runtime.Transport.with_response conn request
                 ~body:
                   (Aws.Runtime.Request_body.of_stream descriptor
                      ~write:(fun writer ->
                        Aws.Runtime.Request_body.write_string writer "abc"))
                 ~consume:(fun _ body -> read_body_once 2 body))))
  in
  (match result with
  | Ok "ab" -> ()
  | Ok value -> Alcotest.failf "unexpected response %S" value
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error);
  let completion_for name =
    List.find_exn !completions ~f:(fun completion ->
        String.equal name
          (completion
          |> Awskit.Observability.For_projection.Operation.Completion.info
          |> Awskit.Observability.For_projection.Operation.Info.name))
  in
  List.iter observed_operations ~f:(fun name ->
      Alcotest.(check int)
        (name ^ " completed once") 1
        (List.count !completions ~f:(fun completion ->
             String.equal name
               (completion
               |> Awskit.Observability.For_projection.Operation.Completion.info
               |> Awskit.Observability.For_projection.Operation.Info.name))));
  List.iter (List.tl_exn observed_operations) ~f:(fun name ->
      Alcotest.(check string)
        (name ^ " outcome") "ok"
        (completion_for name
        |> Awskit.Observability.For_projection.Operation.Completion.outcome
        |> Awskit.Observability.Outcome.to_string));
  let completion = completion_for "awskit.http.attempt" in
  Alcotest.(check string)
    "physical attempt outcome" "throttled"
    (completion
    |> Awskit.Observability.For_projection.Operation.Completion.outcome
    |> Awskit.Observability.Outcome.to_string);
  Alcotest.(check (option int64))
    "connector request bytes" (Some 3L)
    (measurement "http.connector_request_bytes" completion);
  Alcotest.(check (option int64))
    "connector response bytes" (Some 2L)
    (measurement "http.connector_response_bytes" completion);
  Alcotest.(check (option int64))
    "connector drained bytes" (Some 4L)
    (measurement "http.connector_drained_bytes" completion);
  let streaming_values direction =
    Awskit_lwt.Observability.instrument_snapshot observability
    |> List.filter_map ~f:(fun observation ->
        let module Metric = Awskit.Observability.For_projection.Metric in
        match
          ( List.map
              (Metric.Observation.labels observation)
              ~f:Metric.Label.encoded,
            Metric.Observation.value observation )
        with
        | [ observed_direction ], Int64 value
          when String.equal direction observed_direction ->
            Some value
        | _ -> None)
  in
  List.iter [ "request"; "response" ] ~f:(fun direction ->
      let values = streaming_values direction in
      Alcotest.(check bool)
        (direction ^ " streaming gauge is present")
        true
        (not (List.is_empty values));
      Alcotest.(check bool)
        (direction ^ " streaming gauge stayed non-negative")
        true
        (List.for_all values ~f:Int64.is_non_negative);
      Alcotest.(check int64)
        (direction ^ " streaming gauge returned to zero")
        0L (List.last_exn values))

let suite =
  List.map Workload.suite ~f:(fun (name, cases) ->
      if String.equal name "workload:awskit-lwt:runtime-http" then
        ( name,
          cases
          @ [
              Alcotest.test_case "read error invalidates reader" `Quick
                test_reader_invalidated_after_read_error;
              Alcotest.test_case "transport failure log is redacted" `Quick
                test_transport_failure_log_is_redacted;
              Alcotest.test_case "synchronous producer is observed" `Quick
                test_synchronous_request_producer_is_observed;
              Alcotest.test_case
                "large early response stream releases request lease" `Quick
                test_large_early_response_stream_does_not_update_released_lease;
              Alcotest.test_case
                "producer failure drains non-empty keep-alive response" `Quick
                test_request_producer_failure_drains_non_empty_keep_alive_response;
              Alcotest.test_case
                "framing validation drains non-empty keep-alive response" `Quick
                test_framing_validation_drains_non_empty_keep_alive_response;
              Alcotest.test_case
                "response consumer exception drains keep-alive response" `Quick
                test_response_consumer_exception_drains_keep_alive_response;
              Alcotest.test_case "post-reader callback exception drains once"
                `Quick test_callback_exception_after_successful_reader_cleanup;
              Alcotest.test_case "cancellation during body cleanup aborts once"
                `Quick test_cancellation_during_body_cleanup_aborts_once;
              Alcotest.test_case
                "streaming request uses incremental client call" `Quick
                test_streaming_request_uses_incremental_client_call;
              Alcotest.test_case "exact-length response releases owner at EOF"
                `Quick test_exact_length_response_releases_owner_at_eof;
              Alcotest.test_case "abort before connect closes late connection"
                `Quick test_abort_before_connect_closes_late_connection;
              Alcotest.test_case "discard limit aborts before return" `Quick
                test_discard_limit_aborts_before_return;
              Alcotest.test_case "primary consumer error survives abort" `Quick
                test_primary_consumer_error_survives_abort;
              Alcotest.test_case
                "callback error before reader drains and preserves primary"
                `Quick
                test_callback_error_before_reader_drains_and_preserves_primary;
              Alcotest.test_case "cancellation survives abort" `Quick
                test_cancellation_survives_abort;
              Alcotest.test_case "cancellation waits for delayed abort" `Quick
                test_cancellation_waits_for_delayed_abort;
              Alcotest.test_case "static request body preserves representation"
                `Quick test_static_request_body_preserves_representation;
              Alcotest.test_case "attempt body and drain byte accounting" `Quick
                test_attempt_body_and_drain_bytes;
            ] )
      else (name, cases))

let () = Alcotest.run "awskit-lwt" suite
