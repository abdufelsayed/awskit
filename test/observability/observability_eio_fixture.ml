open Base

let rec read_request_headers input acc =
  match Eio.Buf_read.line input with
  | "" -> List.rev acc
  | line -> (
      match String.lsplit2 line ~on:':' with
      | Some (name, value) ->
          read_request_headers input
            ((String.lowercase (String.strip name), String.strip value) :: acc)
      | None -> read_request_headers input acc)

let header_value name headers =
  List.find_map headers ~f:(fun (candidate, value) ->
      if String.equal name candidate then Some value else None)

let read_request_body input headers =
  match header_value "content-length" headers with
  | None -> ""
  | Some value -> Eio.Buf_read.take (Int.of_string value) input

let write_response output ~status ~headers body =
  Eio.Buf_write.string output (Printf.sprintf "HTTP/1.1 %d test\r\n" status);
  let has_content_length =
    List.exists headers ~f:(fun (name, _) ->
        String.equal "content-length" (String.lowercase name))
  in
  if not has_content_length then
    Eio.Buf_write.string output
      (Printf.sprintf "content-length: %d\r\n" (String.length body));
  List.iter headers ~f:(fun (name, value) ->
      Eio.Buf_write.string output (Printf.sprintf "%s: %s\r\n" name value));
  Eio.Buf_write.string output "connection: close\r\n\r\n";
  Eio.Buf_write.string output body;
  Eio.Buf_write.flush output

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let with_connection env ?(status = 200) ?(headers = []) ?(body = "payload")
    ?(responses = []) ?response_delay ?on_request ?observability callback =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:1
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> failwith "expected TCP listener"
  in
  let calls = Atomic.make 0 in
  let remaining_responses = ref responses in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec accept_loop () =
        Eio.Net.accept_fork socket ~sw
          ~on_error:(fun _ -> ())
          (fun flow _ ->
            ignore (Atomic.fetch_and_add calls 1 : int);
            let input = Eio.Buf_read.of_flow ~max_size:Int.max_value flow in
            let request_line = Eio.Buf_read.line input in
            let method_ =
              match String.split request_line ~on:' ' with
              | method_ :: _ -> method_
              | [] -> failwith "missing HTTP request method"
            in
            let request_headers = read_request_headers input [] in
            let request_body = read_request_body input request_headers in
            Option.iter on_request ~f:(fun callback ->
                callback method_ request_headers request_body);
            Option.iter response_delay ~f:(fun seconds ->
                Eio.Time.sleep (Eio.Stdenv.clock env) seconds);
            let response_status, response_headers, response_body =
              match !remaining_responses with
              | (status, headers, body) :: rest ->
                  remaining_responses := rest;
                  (status, headers, body)
              | [] -> (status, headers, body)
            in
            let response_body =
              if String.equal method_ "HEAD" then "" else response_body
            in
            Eio.Buf_write.with_flow flow (fun output ->
                write_response output ~status:response_status
                  ~headers:response_headers response_body));
        accept_loop ()
      in
      accept_loop ());
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  let endpoint_config =
    Awskit_s3.Endpoint_config.local_plaintext ~endpoint
      ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
      ~addressing_style:`Path ()
    |> Result.map_error ~f:Awskit.Error.to_string_hum
    |> Result.ok_or_failwith
  in
  let connection =
    Awskit_s3_eio.create ~sw ~env ~https:Awskit_eio.http_only
      ~region:"us-east-1" ~credentials ~retry_policy:Awskit.Retry.disabled
      ~timeout_policy:Awskit.Timeout.disabled ~endpoint_config ?observability ()
    |> Result.map_error ~f:Awskit.Error.to_string_hum
    |> Result.ok_or_failwith
  in
  callback connection ~calls

let with_connection_without_server env ?observability callback =
  Eio.Switch.run @@ fun sw ->
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:1 () in
  let endpoint_config =
    Awskit_s3.Endpoint_config.local_plaintext ~endpoint
      ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
      ~addressing_style:`Path ()
    |> Result.map_error ~f:Awskit.Error.to_string_hum
    |> Result.ok_or_failwith
  in
  let connection =
    Awskit_s3_eio.create ~sw ~env ~https:Awskit_eio.http_only
      ~region:"us-east-1" ~credentials ~retry_policy:Awskit.Retry.disabled
      ~timeout_policy:Awskit.Timeout.disabled ~endpoint_config ?observability ()
    |> Result.map_error ~f:Awskit.Error.to_string_hum
    |> Result.ok_or_failwith
  in
  callback connection ~calls:(Atomic.make 0)

let get connection =
  Awskit_s3_eio.Object.get_string connection
    ~bucket:(Awskit_s3.Bucket_name.of_string_exn "observability-bucket")
    ~key:(Awskit_s3.Object_key.of_string_exn "object")
    ~max_bytes:1024L ()

let get_value connection =
  match get connection with
  | Ok result -> Ok result.Awskit_s3.Object.Get.value
  | Error _ as error -> error
