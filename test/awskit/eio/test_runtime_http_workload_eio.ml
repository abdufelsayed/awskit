open Base
module Model = Runtime_http_model

let conn_or_fail = function
  | Ok conn -> conn
  | Error error -> Alcotest.failf "%a" Awskit.Error.pp error

let workload_timeout = Ptime.Span.of_float_s 0.25 |> Option.value_exn

let timeout_policy =
  Awskit.Timeout.create_exn ~operation:workload_timeout
    ~response_body:workload_timeout ~drain:workload_timeout ()

let timeout_error message =
  Awskit.Error.Producer.timeout ~operation:"runtime http workload" message

let listener_bind_denied_by_sandbox = function
  (* opam-repository macOS CI can deny local TCP listeners under sandbox.sh. *)
  | Unix.Unix_error (Unix.EPERM, "bind", _) -> true
  | _ -> false

let rec read_headers input =
  match Eio.Buf_read.line input with "" -> () | _ -> read_headers input

let write_response output scenario =
  Eio.Buf_write.string output
    (Printf.sprintf "HTTP/1.1 %d test\r\n" scenario.Model.status);
  List.iter (Model.response_headers scenario) ~f:(fun (name, value) ->
      Eio.Buf_write.string output (Printf.sprintf "%s: %s\r\n" name value));
  Eio.Buf_write.string output "\r\n";
  Eio.Buf_write.string output (Model.body_for_framing scenario.framing);
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
  let release_server () =
    ignore (Eio.Promise.try_resolve resolve_hold_open () : bool)
  in
  let observed_request_method = ref None in
  let check_observed_request_method observed_method =
    Alcotest.(check string)
      "request method"
      (Model.method_to_string scenario.Model.method_)
      observed_method
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
          observed_request_method := Some observed_method;
          read_headers input;
          Eio.Buf_write.with_flow flow (fun output ->
              write_response output scenario;
              match scenario.connection with
              | Close -> ()
              | Keep_alive -> Eio.Promise.await hold_open)));
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  match test endpoint with
  | result ->
      release_server ();
      (match (result, !observed_request_method) with
      | Ok _, Some observed_method | Error _, Some observed_method ->
          check_observed_request_method observed_method
      | Ok _, None ->
          Alcotest.failf "%s succeeded without observed request method"
            scenario.Model.name
      | Error _, None -> ());
      result
  | exception exn ->
      release_server ();
      (match !observed_request_method with
      | Some observed_method -> check_observed_request_method observed_method
      | None -> ());
      raise exn

let request_for_endpoint scenario endpoint =
  let target =
    Awskit.Request.Target.create_exn
      ~scheme:(Awskit.Endpoint.scheme endpoint)
      ~host:(Awskit.Endpoint.host endpoint)
      ?port:(Awskit.Endpoint.port endpoint)
      ~path:"/" ()
  in
  Awskit.Request.create_exn ~method_:scenario.Model.method_ ~target ()

let request_conn env sw =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK" ()
  in
  Awskit_eio.create ~env ~sw ~https:Awskit_eio.http_only ~region:"us-east-1"
    ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ~timeout_policy ()
  |> conn_or_fail

let rec read_all reader buffer =
  let chunk = Bytes.create 3 in
  match
    Awskit_eio.Runtime.Response_body.read reader chunk ~off:0
      ~len:(Bytes.length chunk)
  with
  | Error _ as error -> error
  | Ok 0 -> Ok (Buffer.contents buffer)
  | Ok n ->
      Buffer.add_substring buffer (Bytes.to_string chunk) ~pos:0 ~len:n;
      read_all reader buffer

let read_body_to_string body =
  Awskit_eio.Runtime.Response_body.with_reader body ~consume:(fun reader ->
      read_all reader (Buffer.create 16))

let run_with_guard env scenario =
  let clock = Eio.Stdenv.clock env in
  with_loopback_server env scenario (fun endpoint ->
      try
        Eio.Time.with_timeout_exn clock 0.75 (fun () ->
            Eio.Switch.run @@ fun sw ->
            let conn = request_conn env sw in
            let request = request_for_endpoint scenario endpoint in
            Awskit_eio.Runtime.Transport.with_response conn request
              ~body:Awskit_eio.Runtime.Request_body.empty
              ~consume:(fun response response_body ->
                Alcotest.(check int)
                  "response status" scenario.status
                  (Awskit.Response.status response);
                read_body_to_string response_body))
      with Eio.Time.Timeout ->
        Error (timeout_error "runtime HTTP scenario timed out"))

let suite env =
  let module Target = struct
    let name = "awskit-eio"
    let run_scenario scenario = run_with_guard env scenario
  end in
  let module Workload = Runtime_http_workload.Make (Target) in
  Workload.suite

let () = Eio_main.run @@ fun env -> Alcotest.run "awskit-eio" (suite env)
