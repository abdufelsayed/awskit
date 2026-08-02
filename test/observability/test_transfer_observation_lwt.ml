open Base
open Lwt.Infix
module O = Awskit.Observability

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

let bucket = Awskit_s3.Bucket_name.of_string_exn "observability-bucket"
let key = Awskit_s3.Object_key.of_string_exn "transfer-object"

let close_socket fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
    (function
      | Unix.Unix_error (Unix.EBADF, _, _) | Lwt.Canceled -> Lwt.return_unit
      | exn -> Lwt.fail exn)

let rec read_line fd =
  let buffer = Buffer.create 64 in
  let byte = Bytes.create 1 in
  let rec loop previous =
    Lwt.bind (Lwt_unix.read fd byte 0 1) (fun read ->
        if read = 0 then Lwt.fail End_of_file
        else
          let ch = Bytes.get byte 0 in
          if Char.equal previous '\r' && Char.equal ch '\n' then
            let value = Buffer.contents buffer in
            Lwt.return (String.drop_suffix value 1)
          else (
            Buffer.add_char buffer ch;
            loop ch))
  in
  loop '\000'

let rec read_headers fd headers =
  Lwt.bind (read_line fd) (fun line ->
      if String.is_empty line then Lwt.return (List.rev headers)
      else
        match String.lsplit2 line ~on:':' with
        | None -> read_headers fd headers
        | Some (name, value) ->
            read_headers fd
              ((String.lowercase (String.strip name), String.strip value)
              :: headers))

let header name headers =
  List.find_map headers ~f:(fun (key, value) ->
      if String.equal name key then Some value else None)

let rec read_exact fd bytes offset remaining =
  if remaining = 0 then Lwt.return_unit
  else
    Lwt.bind (Lwt_unix.read fd bytes offset remaining) (fun read ->
        if read = 0 then Lwt.fail End_of_file
        else read_exact fd bytes (offset + read) (remaining - read))

let consume_request_body fd headers =
  match header "content-length" headers with
  | None -> Lwt.return ""
  | Some value -> (
      match Int.of_string_opt value with
      | None -> Lwt.fail (Failure "invalid request content length")
      | Some length ->
          let bytes = Bytes.create length in
          Lwt.bind (read_exact fd bytes 0 length) (fun () ->
              Lwt.return (Bytes.to_string bytes)))

let write_all fd value =
  let bytes = Bytes.of_string value in
  let rec loop offset =
    if offset = Bytes.length bytes then Lwt.return_unit
    else
      Lwt.bind
        (Lwt_unix.write fd bytes offset (Bytes.length bytes - offset))
        (fun written ->
          if written = 0 then Lwt.fail End_of_file else loop (offset + written))
  in
  loop 0

let response_wire response =
  let headers =
    let has_content_length =
      List.exists response.headers ~f:(fun (name, _) ->
          String.equal "content-length" (String.lowercase name))
    in
    (if has_content_length then []
     else [ ("Content-Length", Int.to_string (String.length response.body)) ])
    @ [ ("Connection", "close") ]
    @ response.headers
  in
  let header_block =
    headers
    |> List.map ~f:(fun (name, value) -> Fmt.str "%s: %s\r\n" name value)
    |> String.concat
  in
  Fmt.str "HTTP/1.1 %d test\r\n%s\r\n%s" response.status header_block
    response.body

let with_loopback_server
    ?(on_request = fun _method_ _headers _body -> Lwt.return_unit)
    ?(before_response = fun _method_ _headers _body -> Lwt.return_unit)
    ?(allow_server_error = false) ?(wait_for_server = true) responses callback =
  let listener = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt listener Unix.SO_REUSEADDR true;
  Lwt.catch
    (fun () ->
      Lwt.bind
        (Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)))
        (fun () ->
          Lwt_unix.listen listener 8;
          let endpoint =
            match Lwt_unix.getsockname listener with
            | Unix.ADDR_INET (_, port) ->
                Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port ()
            | Unix.ADDR_UNIX _ -> failwith "expected TCP listener"
          in
          let remaining = ref responses in
          let server_done, resolve_server_done = Lwt.task () in
          let rec serve () =
            match !remaining with
            | [] ->
                Lwt.wakeup_later resolve_server_done ();
                Lwt.return_unit
            | response :: rest ->
                Lwt.bind (Lwt_unix.accept listener) (fun (client, _) ->
                    remaining := rest;
                    Lwt.finalize
                      (fun () ->
                        Lwt.bind (read_line client) (fun request_line ->
                            let method_ =
                              match String.split request_line ~on:' ' with
                              | method_ :: _ -> method_
                              | [] -> ""
                            in
                            Lwt.bind (read_headers client []) (fun headers ->
                                Lwt.bind (consume_request_body client headers)
                                  (fun body ->
                                    Lwt.bind (on_request method_ headers body)
                                      (fun () ->
                                        Lwt.bind
                                          (before_response method_ headers body)
                                          (fun () ->
                                            let response =
                                              if String.equal method_ "HEAD"
                                              then { response with body = "" }
                                              else response
                                            in
                                            write_all client
                                              (response_wire response)))))))
                      (fun () -> close_socket client)
                    >>= fun () -> serve ())
          in
          let server =
            Lwt.catch serve (fun exn ->
                if Lwt.is_sleeping server_done then
                  Lwt.wakeup_later_exn resolve_server_done exn;
                Lwt.fail exn)
          in
          Lwt.finalize
            (fun () ->
              Lwt.bind (callback endpoint) (fun result ->
                  if not wait_for_server then Lwt.return result
                  else
                    Lwt.catch
                      (fun () ->
                        Lwt.bind server_done (fun () -> Lwt.return result))
                      (fun exn ->
                        if allow_server_error then Lwt.return result
                        else Lwt.fail exn)))
            (fun () ->
              Lwt.cancel server;
              Lwt.bind (close_socket listener) (fun () ->
                  Lwt.catch
                    (fun () -> server)
                    (function
                      | Lwt.Canceled -> Lwt.return_unit
                      | _exn when allow_server_error -> Lwt.return_unit
                      | exn -> Lwt.fail exn)))))
    (function
      | Unix.Unix_error (Unix.EPERM, "bind", _) ->
          Loopback_policy.handle_bind_denied ()
      | exn -> Lwt.fail exn)

let write_file path contents =
  let channel = Stdlib.open_out_bin path in
  Exn.protect
    ~f:(fun () -> Stdlib.output_string channel contents)
    ~finally:(fun () -> Stdlib.close_out_noerr channel)

let read_file path =
  let channel = Stdlib.open_in_bin path in
  Exn.protect
    ~f:(fun () ->
      let length = Stdlib.in_channel_length channel in
      Stdlib.really_input_string channel length)
    ~finally:(fun () -> Stdlib.close_in_noerr channel)

let unlink_if_present path =
  try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let with_temp_file ~suffix f =
  let path = Stdlib.Filename.temp_file "awskit-transfer-observation" suffix in
  Exn.protect ~f:(fun () -> f path) ~finally:(fun () -> unlink_if_present path)

let transfer_name completion =
  completion
  |> O.For_projection.Operation.Completion.info
  |> O.For_projection.Operation.Info.name

let measurement name completion =
  completion
  |> O.For_projection.Operation.Completion.measurements
  |> List.find_map ~f:(fun value ->
      if String.equal name (O.For_projection.Measurement.name value) then
        match O.For_projection.Measurement.value value with
        | Int value -> Some (Int64.of_int value)
        | Int64 value -> Some value
        | Float _ -> None
      else None)

let dimension name completion =
  completion
  |> O.For_projection.Operation.Completion.dimensions
  |> List.find_map ~f:(fun value ->
      if String.equal name (O.For_projection.Dimension.name value) then
        Some (O.For_projection.Dimension.value value)
      else None)

let metric_observer ?trace_sinks () =
  let transfer_observations = ref [] in
  let sink =
    O.Metric_sink.create ~name:"transfer-capture" ~needs_clock:true
      ~enabled:(fun family ->
        List.mem
          [
            "awskit.s3.transfers";
            "awskit.s3.transfer.logical_bytes";
            "awskit.s3.transfer.parts";
            "awskit.s3.transfers_in_flight";
          ]
          (O.For_projection.Metric.Family.name family)
          ~equal:String.equal)
      ~observe:(fun observation ->
        transfer_observations := observation :: !transfer_observations)
  in
  let ticks = ref 0L in
  let observer =
    Awskit_lwt.Observability.create ~logs:false ~metric_sinks:[ sink ]
      ?trace_sinks
      ~clock:(fun () ->
        let value = !ticks in
        ticks := Int64.succ value;
        value)
      ()
  in
  (observer, transfer_observations)

let gauge_value observer direction =
  let module Metric = O.For_projection.Metric in
  Awskit_lwt.Observability.instrument_snapshot observer
  |> List.find_map ~f:(fun observation ->
      let family = Metric.Observation.family observation in
      let labels = Metric.Observation.labels observation in
      if
        String.equal "awskit.s3.transfers_in_flight" (Metric.Family.name family)
        && List.equal String.equal
             (List.map labels ~f:Metric.Label.encoded)
             [ direction ]
      then
        match Metric.Observation.value observation with
        | Int64 value -> Some value
        | Int _ | Float _ -> None
      else None)
  |> Option.value ~default:0L

let transfer_completions completions =
  List.filter !completions ~f:(fun completion ->
      String.equal "awskit.s3.transfer" (transfer_name completion))

let metric_values name observations =
  let module Metric = O.For_projection.Metric in
  List.filter_map !observations ~f:(fun observation ->
      let family = Metric.Observation.family observation in
      if String.equal name (Metric.Family.name family) then
        Some
          ( List.map
              (Metric.Observation.labels observation)
              ~f:Metric.Label.encoded,
            Metric.Observation.value observation )
      else None)

let all_transfer_and_operation_sink completions =
  Awskit_lwt.Observability.Trace_sink.create ~name:"transfer-topology"
    ~needs_clock:false
    ~enabled:(fun info ->
      let name = O.For_projection.Operation.Info.name info in
      String.equal "awskit.s3.transfer" name
      || String.equal "awskit.s3.operation" name)
    ~start:(fun _ ->
      {
        Awskit_lwt.Observability.Trace_sink.within =
          (fun callback -> callback ());
        correlation = [];
        finish = (fun completion -> completions := completion :: !completions);
      })
    ~event_enabled:(fun _ -> false)
    ~event:(fun _ -> ())

let operation_completions completions =
  List.filter !completions ~f:(fun completion ->
      String.equal "awskit.s3.operation" (transfer_name completion))

let sorted_operation_names completions =
  operation_completions completions
  |> List.filter_map ~f:(dimension "aws.operation")
  |> List.sort ~compare:String.compare

let multipart_part_size = Awskit_s3.Transfer.min_part_size
let multipart_length = multipart_part_size + 1

let make_lwt_connection observer endpoint =
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
  in
  let endpoint_config =
    Awskit_s3.Endpoint_config.local_plaintext ~endpoint
      ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
      ~addressing_style:`Path ()
    |> ok_or_fail "endpoint config"
  in
  Awskit_s3_lwt_unix.create ~endpoint_config ~region:"us-east-1" ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ~timeout_policy:Awskit.Timeout.disabled
    ~observability:observer ()
  |> ok_or_fail "connection"

let test_successful_upload_and_download () =
  with_temp_file ~suffix:".source" (fun source ->
      write_file source "upload!";
      with_temp_file ~suffix:".target" (fun target ->
          Unix.unlink target;
          let completions = ref [] in
          let sink =
            Awskit_lwt.Observability.Trace_sink.create ~name:"transfer-trace"
              ~needs_clock:false
              ~enabled:(fun info ->
                String.equal "awskit.s3.transfer"
                  (O.For_projection.Operation.Info.name info))
              ~start:(fun _ ->
                {
                  Awskit_lwt.Observability.Trace_sink.within =
                    (fun callback -> callback ());
                  correlation = [];
                  finish =
                    (fun completion ->
                      completions := completion :: !completions);
                })
              ~event_enabled:(fun _ -> false)
              ~event:(fun _ -> ())
          in
          let observer, metric_observations =
            metric_observer ~trace_sinks:[ sink ] ()
          in
          let upload_progress_gauges = ref [] in
          let download_progress_gauges = ref [] in
          let credentials =
            Awskit.Credentials.create_exn ~access_key_id:"AKID"
              ~secret_access_key:"SECRET" ()
          in
          let make_connection endpoint =
            let endpoint_config =
              Awskit_s3.Endpoint_config.local_plaintext ~endpoint
                ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
                ~addressing_style:`Path ()
              |> ok_or_fail "endpoint config"
            in
            Awskit_s3_lwt_unix.create ~endpoint_config ~region:"us-east-1"
              ~credentials
              ~clock:(fun () -> Ptime.epoch)
              ~retry_policy:Awskit.Retry.disabled
              ~timeout_policy:Awskit.Timeout.disabled ~observability:observer ()
            |> ok_or_fail "connection"
          in
          let responses =
            [
              { status = 200; headers = [ ("etag", "\"upload\"") ]; body = "" };
              {
                status = 200;
                headers = [ ("etag", "\"head\"") ];
                body = "payload";
              };
              {
                status = 200;
                headers = [ ("etag", "\"download\"") ];
                body = "payload";
              };
            ]
          in
          let requests = ref [] in
          let result =
            Lwt_main.run
              (with_loopback_server
                 ~on_request:(fun method_ _headers body ->
                   requests := (method_, body) :: !requests;
                   Lwt.return_unit)
                 responses
                 (fun endpoint ->
                   let connection = make_connection endpoint in
                   let upload =
                     Awskit_s3_lwt_unix.Object.Transfer.upload_file connection
                       ~bucket ~key
                       ~on_progress:(fun _ ->
                         upload_progress_gauges :=
                           gauge_value observer "upload"
                           :: !upload_progress_gauges)
                       ~path:source ()
                   in
                   Lwt.bind upload (function
                     | Error error -> Lwt.return (Error error)
                     | Ok upload ->
                         let download =
                           Awskit_s3_lwt_unix.Object.Transfer.download_file
                             connection ~bucket ~key
                             ~on_progress:(fun _ ->
                               download_progress_gauges :=
                                 gauge_value observer "download"
                                 :: !download_progress_gauges)
                             ~path:target ()
                         in
                         Lwt.map
                           (Result.map ~f:(fun download -> (upload, download)))
                           download)))
          in
          let upload, download =
            match result with
            | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
            | Ok result -> result
          in
          ignore upload;
          ignore download;
          Alcotest.(check (list (pair string string)))
            "PUT, HEAD, and GET requests"
            [ ("PUT", "upload!"); ("HEAD", ""); ("GET", "") ]
            (List.rev !requests);
          Alcotest.(check string) "download body" "payload" (read_file target);
          let transfers = transfer_completions completions in
          Alcotest.(check int)
            "two transfer completions" 2 (List.length transfers);
          let uploads, downloads =
            List.partition_tf transfers ~f:(fun completion ->
                String.equal "upload"
                  (Option.value_exn (dimension "transfer.direction" completion)))
          in
          Alcotest.(check int) "one upload completion" 1 (List.length uploads);
          Alcotest.(check int)
            "one download completion" 1 (List.length downloads);
          let check_success direction completion =
            Alcotest.(check string)
              (direction ^ " outcome") "ok"
              (O.For_projection.Operation.Completion.outcome completion
              |> O.Outcome.to_string);
            Alcotest.(check (option int64))
              (direction ^ " logical bytes")
              (Some 7L)
              (measurement "transfer.logical_bytes" completion);
            Alcotest.(check (option int64))
              (direction ^ " parts") (Some 1L)
              (measurement "transfer.parts" completion)
          in
          check_success "upload" (List.hd_exn uploads);
          check_success "download" (List.hd_exn downloads);
          Alcotest.(check int64)
            "upload gauge released" 0L
            (gauge_value observer "upload");
          Alcotest.(check int64)
            "download gauge released" 0L
            (gauge_value observer "download");
          Alcotest.(check bool)
            "upload gauge was held" true
            (List.exists !upload_progress_gauges ~f:(Int64.equal 1L));
          Alcotest.(check bool)
            "download gauge was held" true
            (List.exists !download_progress_gauges ~f:(Int64.equal 1L));
          let transfer_samples =
            metric_values "awskit.s3.transfers" metric_observations
          in
          Alcotest.(check int)
            "transfer counter samples" 2
            (List.length transfer_samples);
          List.iter transfer_samples ~f:(fun (labels, value) ->
              Alcotest.(check int)
                "counter labels are exact" 2 (List.length labels);
              Alcotest.(check bool)
                "counter direction is bounded" true
                (List.equal String.equal labels [ "upload"; "ok" ]
                || List.equal String.equal labels [ "download"; "ok" ]);
              match value with
              | O.For_projection.Metric.Value.Int64 1L -> ()
              | _ -> Alcotest.fail "transfer counter value was not one");
          let logical_samples =
            metric_values "awskit.s3.transfer.logical_bytes" metric_observations
          in
          Alcotest.(check int)
            "logical byte samples" 2
            (List.length logical_samples);
          List.iter logical_samples ~f:(fun (labels, value) ->
              Alcotest.(check bool)
                "logical byte labels are bounded" true
                (List.equal String.equal labels [ "upload" ]
                || List.equal String.equal labels [ "download" ]);
              match value with
              | O.For_projection.Metric.Value.Int64 7L -> ()
              | _ -> Alcotest.fail "logical byte value was not seven");
          let part_samples =
            metric_values "awskit.s3.transfer.parts" metric_observations
          in
          Alcotest.(check int) "part samples" 2 (List.length part_samples);
          List.iter part_samples ~f:(fun (labels, value) ->
              Alcotest.(check bool)
                "part labels are bounded" true
                (List.equal String.equal labels [ "upload" ]
                || List.equal String.equal labels [ "download" ]);
              match value with
              | O.For_projection.Metric.Value.Int64 1L -> ()
              | _ -> Alcotest.fail "part value was not one")))

let test_multipart_upload_topology () =
  with_temp_file ~suffix:".source" (fun source ->
      write_file source (String.make multipart_length 'u');
      let completions = ref [] in
      let sink = all_transfer_and_operation_sink completions in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let create_body =
        "<InitiateMultipartUploadResult><UploadId>upload-id</UploadId>"
        ^ "</InitiateMultipartUploadResult>"
      in
      let complete_body =
        "<CompleteMultipartUploadResult><ETag>\"complete\"</ETag>"
        ^ "</CompleteMultipartUploadResult>"
      in
      let responses =
        [
          { status = 200; headers = []; body = create_body };
          { status = 200; headers = [ ("etag", "\"part-1\"") ]; body = "" };
          { status = 200; headers = [ ("etag", "\"part-2\"") ]; body = "" };
          { status = 200; headers = []; body = complete_body };
        ]
      in
      let requests = ref [] in
      let result =
        Lwt_main.run
          (with_loopback_server
             ~on_request:(fun method_ _headers body ->
               requests := (method_, String.length body) :: !requests;
               Lwt.return_unit)
             responses
             (fun endpoint ->
               let connection = make_lwt_connection observer endpoint in
               let options =
                 Awskit_s3.Transfer.upload_options_exn ~multipart_threshold:1L
                   ~part_size:multipart_part_size ~concurrency:1 ()
               in
               Awskit_s3_lwt_unix.Object.Transfer.upload_file connection ~bucket
                 ~key ~options ~path:source ()))
      in
      let upload =
        match result with
        | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
        | Ok upload -> upload
      in
      (match Awskit_s3.Transfer.upload_strategy upload with
      | `Multipart -> ()
      | `Put -> Alcotest.fail "multipart upload selected PutObject");
      Alcotest.(check int64)
        "multipart upload logical bytes"
        (Int64.of_int multipart_length)
        (Awskit_s3.Transfer.upload_bytes_transferred upload);
      let requests = List.rev !requests in
      Alcotest.(check (list string))
        "multipart request methods"
        [ "POST"; "PUT"; "PUT"; "POST" ]
        (List.map requests ~f:fst);
      let request_body_lengths = List.map requests ~f:snd in
      Alcotest.(check int)
        "create request body length" 0
        (List.nth_exn request_body_lengths 0);
      Alcotest.(check int)
        "first upload part body length" multipart_part_size
        (List.nth_exn request_body_lengths 1);
      Alcotest.(check int)
        "final upload part body length" 1
        (List.nth_exn request_body_lengths 2);
      Alcotest.(check bool)
        "complete request has an XML body" true
        (Int.compare (List.nth_exn request_body_lengths 3) 0 > 0);
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "one multipart transfer parent" 1 (List.length transfers);
      let transfer = List.hd_exn transfers in
      Alcotest.(check string)
        "multipart transfer direction" "upload"
        (Option.value_exn (dimension "transfer.direction" transfer));
      Alcotest.(check string)
        "multipart transfer outcome" "ok"
        (O.For_projection.Operation.Completion.outcome transfer
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "multipart transfer logical bytes"
        (Some (Int64.of_int multipart_length))
        (measurement "transfer.logical_bytes" transfer);
      Alcotest.(check (option int64))
        "multipart transfer parts" (Some 2L)
        (measurement "transfer.parts" transfer);
      Alcotest.(check (list string))
        "multipart logical child operations"
        [
          "CompleteMultipartUpload";
          "CreateMultipartUpload";
          "UploadPart";
          "UploadPart";
        ]
        (sorted_operation_names completions);
      List.iter (operation_completions completions) ~f:(fun completion ->
          Alcotest.(check string)
            "multipart child outcome" "ok"
            (O.For_projection.Operation.Completion.outcome completion
            |> O.Outcome.to_string);
          Alcotest.(check string)
            "multipart child transfer direction" "upload"
            (Option.value_exn (dimension "transfer.direction" completion)));
      let logical_samples =
        metric_values "awskit.s3.transfer.logical_bytes" metric_observations
      in
      Alcotest.(check int)
        "multipart logical byte metric" 1
        (List.length logical_samples);
      (match logical_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 value) ] ->
          Alcotest.(check int64)
            "multipart logical byte metric value"
            (Int64.of_int multipart_length)
            value
      | _ -> Alcotest.fail "multipart logical byte metric shape");
      let part_samples =
        metric_values "awskit.s3.transfer.parts" metric_observations
      in
      Alcotest.(check int) "multipart part metric" 1 (List.length part_samples);
      match part_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 2L) ] -> ()
      | _ -> Alcotest.fail "multipart part metric shape")

let test_resume_multipart_upload_topology () =
  with_temp_file ~suffix:".source" (fun source ->
      write_file source (String.make multipart_length 'u');
      let completions = ref [] in
      let sink = all_transfer_and_operation_sink completions in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let list_parts_body =
        Fmt.str
          "<ListPartsResult><Part><PartNumber>1</PartNumber><ETag>\"existing\"</ETag><Size>%d</Size></Part></ListPartsResult>"
          multipart_part_size
      in
      let complete_response_body =
        "<CompleteMultipartUploadResult><ETag>\"complete\"</ETag>"
        ^ "</CompleteMultipartUploadResult>"
      in
      let upload =
        Awskit_s3.Multipart.Upload.resume ~bucket ~key
          ~upload_id:(Awskit_s3.Multipart.Upload_id.of_string_exn "upload-id")
      in
      let responses =
        [
          { status = 200; headers = []; body = list_parts_body };
          { status = 200; headers = [ ("etag", "\"fresh-1\"") ]; body = "" };
          { status = 200; headers = [ ("etag", "\"fresh-2\"") ]; body = "" };
          { status = 200; headers = []; body = complete_response_body };
        ]
      in
      let requests = ref [] in
      let complete_requests = ref [] in
      let result =
        Lwt_main.run
          (with_loopback_server
             ~on_request:(fun method_ _headers body ->
               requests := (method_, String.length body) :: !requests;
               if String.equal method_ "POST" then
                 complete_requests := body :: !complete_requests;
               Lwt.return_unit)
             responses
             (fun endpoint ->
               let connection = make_lwt_connection observer endpoint in
               let options =
                 Awskit_s3.Transfer.upload_options_exn ~multipart_threshold:1L
                   ~part_size:multipart_part_size ~concurrency:1 ()
               in
               Awskit_s3_lwt_unix.Object.Transfer.resume_multipart_upload_file
                 connection ~upload ~options ~path:source ()))
      in
      let result =
        match result with
        | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
        | Ok result -> result
      in
      Alcotest.(check int64)
        "resumed upload logical bytes"
        (Int64.of_int multipart_length)
        result.bytes_transferred;
      Alcotest.(check int) "resumed upload parts" 2 (List.length result.parts);
      let requests = List.rev !requests in
      Alcotest.(check (list string))
        "resume request methods"
        [ "GET"; "PUT"; "PUT"; "POST" ]
        (List.map requests ~f:fst);
      let request_body_lengths = List.map requests ~f:snd in
      Alcotest.(check int)
        "resume ListParts request body length" 0
        (List.nth_exn request_body_lengths 0);
      Alcotest.(check int)
        "resume first upload part body length" multipart_part_size
        (List.nth_exn request_body_lengths 1);
      Alcotest.(check int)
        "resume final upload part body length" 1
        (List.nth_exn request_body_lengths 2);
      let complete_request =
        match List.rev !complete_requests with
        | [ body ] -> body
        | _ -> Alcotest.fail "resume complete request count"
      in
      Alcotest.(check bool)
        "resume completion uses fresh part one" true
        (Base.String.is_substring complete_request ~substring:"fresh-1");
      Alcotest.(check bool)
        "resume completion uses fresh part two" true
        (Base.String.is_substring complete_request ~substring:"fresh-2");
      Alcotest.(check bool)
        "resume completion omits existing part etag" false
        (Base.String.is_substring complete_request ~substring:"existing");
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "one resumed transfer parent" 1 (List.length transfers);
      let transfer = List.hd_exn transfers in
      Alcotest.(check string)
        "resumed transfer direction" "upload"
        (Option.value_exn (dimension "transfer.direction" transfer));
      Alcotest.(check string)
        "resumed transfer outcome" "ok"
        (O.For_projection.Operation.Completion.outcome transfer
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "resumed transfer logical bytes"
        (Some (Int64.of_int multipart_length))
        (measurement "transfer.logical_bytes" transfer);
      Alcotest.(check (option int64))
        "resumed transfer parts" (Some 2L)
        (measurement "transfer.parts" transfer);
      Alcotest.(check (list string))
        "resumed logical child operations"
        [ "CompleteMultipartUpload"; "ListParts"; "UploadPart"; "UploadPart" ]
        (sorted_operation_names completions);
      List.iter (operation_completions completions) ~f:(fun completion ->
          Alcotest.(check string)
            "resumed child outcome" "ok"
            (O.For_projection.Operation.Completion.outcome completion
            |> O.Outcome.to_string);
          Alcotest.(check string)
            "resumed child transfer direction" "upload"
            (Option.value_exn (dimension "transfer.direction" completion)));
      let logical_samples =
        metric_values "awskit.s3.transfer.logical_bytes" metric_observations
      in
      Alcotest.(check int)
        "resumed logical byte metric" 1
        (List.length logical_samples);
      (match logical_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 value) ] ->
          Alcotest.(check int64)
            "resumed logical byte metric value"
            (Int64.of_int multipart_length)
            value
      | _ -> Alcotest.fail "resumed logical byte metric shape");
      let part_samples =
        metric_values "awskit.s3.transfer.parts" metric_observations
      in
      Alcotest.(check int) "resumed part metric" 1 (List.length part_samples);
      match part_samples with
      | [ ([ "upload" ], O.For_projection.Metric.Value.Int64 2L) ] -> ()
      | _ -> Alcotest.fail "resumed part metric shape")

let test_ranged_download_topology () =
  with_temp_file ~suffix:".target" (fun target ->
      Unix.unlink target;
      let first_part = String.make multipart_part_size 'a' in
      let final_part = "b" in
      let object_length = multipart_length in
      let completions = ref [] in
      let sink = all_transfer_and_operation_sink completions in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let responses =
        [
          {
            status = 200;
            headers =
              [
                ("content-length", Int.to_string object_length);
                ("etag", "\"object\"");
              ];
            body = "";
          };
          {
            status = 206;
            headers =
              [
                ( "content-range",
                  Fmt.str "bytes 0-%d/%d" (multipart_part_size - 1)
                    object_length );
              ];
            body = first_part;
          };
          {
            status = 206;
            headers =
              [
                ( "content-range",
                  Fmt.str "bytes %d-%d/%d" multipart_part_size
                    (object_length - 1) object_length );
              ];
            body = final_part;
          };
        ]
      in
      let requests = ref [] in
      let result =
        Lwt_main.run
          (with_loopback_server
             ~on_request:(fun method_ headers _body ->
               requests := (method_, headers) :: !requests;
               Lwt.return_unit)
             responses
             (fun endpoint ->
               let connection = make_lwt_connection observer endpoint in
               let options =
                 Awskit_s3.Transfer.download_options_exn ~multipart_threshold:1L
                   ~part_size:multipart_part_size ~concurrency:1 ()
               in
               Awskit_s3_lwt_unix.Object.Transfer.download_file connection
                 ~bucket ~key ~options ~path:target ()))
      in
      let download =
        match result with
        | Error error -> Alcotest.failf "%a" Awskit.Error.pp error
        | Ok download -> download
      in
      (match Awskit_s3.Transfer.download_strategy download with
      | `Ranged -> ()
      | `Get -> Alcotest.fail "ranged download selected GetObject");
      Alcotest.(check int64)
        "ranged download logical bytes"
        (Int64.of_int object_length)
        (Awskit_s3.Transfer.download_bytes_transferred download);
      (match download with
      | Awskit_s3.Transfer.Ranged result ->
          Alcotest.(check int) "ranged download parts" 2 result.parts
      | Awskit_s3.Transfer.Get _ -> Alcotest.fail "ranged download result shape");
      Alcotest.(check string)
        "ranged download bytes" (first_part ^ final_part) (read_file target);
      let requests = List.rev !requests in
      Alcotest.(check (list string))
        "ranged request methods" [ "HEAD"; "GET"; "GET" ]
        (List.map requests ~f:fst);
      let ranges =
        List.filter_map requests ~f:(fun (_method_, headers) ->
            List.find_map headers ~f:(fun (name, value) ->
                if String.equal "range" name then Some value else None))
      in
      Alcotest.(check (list string))
        "ranged GetObject byte ranges"
        [
          Fmt.str "bytes=0-%d" (multipart_part_size - 1);
          Fmt.str "bytes=%d-%d" multipart_part_size (object_length - 1);
        ]
        ranges;
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "one ranged transfer parent" 1 (List.length transfers);
      let transfer = List.hd_exn transfers in
      Alcotest.(check string)
        "ranged transfer direction" "download"
        (Option.value_exn (dimension "transfer.direction" transfer));
      Alcotest.(check string)
        "ranged transfer outcome" "ok"
        (O.For_projection.Operation.Completion.outcome transfer
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "ranged transfer logical bytes"
        (Some (Int64.of_int object_length))
        (measurement "transfer.logical_bytes" transfer);
      Alcotest.(check (option int64))
        "ranged transfer parts" (Some 2L)
        (measurement "transfer.parts" transfer);
      Alcotest.(check (list string))
        "ranged logical child operations"
        [ "GetObject"; "GetObject"; "HeadObject" ]
        (sorted_operation_names completions);
      List.iter (operation_completions completions) ~f:(fun completion ->
          Alcotest.(check string)
            "ranged child outcome" "ok"
            (O.For_projection.Operation.Completion.outcome completion
            |> O.Outcome.to_string);
          Alcotest.(check string)
            "ranged child transfer direction" "download"
            (Option.value_exn (dimension "transfer.direction" completion)));
      let logical_samples =
        metric_values "awskit.s3.transfer.logical_bytes" metric_observations
      in
      Alcotest.(check int)
        "ranged logical byte metric" 1
        (List.length logical_samples);
      (match logical_samples with
      | [ ([ "download" ], O.For_projection.Metric.Value.Int64 value) ] ->
          Alcotest.(check int64)
            "ranged logical byte metric value"
            (Int64.of_int object_length)
            value
      | _ -> Alcotest.fail "ranged logical byte metric shape");
      let part_samples =
        metric_values "awskit.s3.transfer.parts" metric_observations
      in
      Alcotest.(check int) "ranged part metric" 1 (List.length part_samples);
      match part_samples with
      | [ ([ "download" ], O.For_projection.Metric.Value.Int64 2L) ] -> ()
      | _ -> Alcotest.fail "ranged part metric shape")

let test_failed_upload_completes_once () =
  let completions = ref [] in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"transfer-failure"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.s3.transfer"
          (O.For_projection.Operation.Info.name info))
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
  let observer =
    Awskit_lwt.Observability.create ~logs:false ~trace_sinks:[ sink ] ()
  in
  let missing = Stdlib.Filename.temp_file "awskit-transfer-missing" ".source" in
  Unix.unlink missing;
  let credentials =
    Awskit.Credentials.create_exn ~access_key_id:"AKID"
      ~secret_access_key:"SECRET" ()
  in
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:1 () in
  let endpoint_config =
    Awskit_s3.Endpoint_config.local_plaintext ~endpoint
      ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
      ~addressing_style:`Path ()
    |> ok_or_fail "endpoint config"
  in
  let connection =
    Awskit_s3_lwt_unix.create ~endpoint_config ~region:"us-east-1" ~credentials
      ~clock:(fun () -> Ptime.epoch)
      ~retry_policy:Awskit.Retry.disabled
      ~timeout_policy:Awskit.Timeout.disabled ~observability:observer ()
    |> ok_or_fail "connection"
  in
  let result =
    Awskit_s3_lwt_unix.Object.Transfer.upload_file connection ~bucket ~key
      ~path:missing ()
    |> Lwt_main.run
  in
  Alcotest.(check bool) "missing source fails" true (Result.is_error result);
  let transfers = transfer_completions completions in
  Alcotest.(check int)
    "failed transfer completes once" 1 (List.length transfers);
  Alcotest.(check bool)
    "failure is not success" true
    (not
       (String.equal "ok"
          (O.For_projection.Operation.Completion.outcome (List.hd_exn transfers)
          |> O.Outcome.to_string)));
  Alcotest.(check int64)
    "failed transfer gauge released" 0L
    (gauge_value observer "upload")

let test_progress_callback_exception_completes_once () =
  with_temp_file ~suffix:".source" (fun source ->
      write_file source "upload!";
      let completions = ref [] in
      let sink =
        Awskit_lwt.Observability.Trace_sink.create ~name:"transfer-callback-exn"
          ~needs_clock:false
          ~enabled:(fun info ->
            String.equal "awskit.s3.transfer"
              (O.For_projection.Operation.Info.name info))
          ~start:(fun _ ->
            {
              Awskit_lwt.Observability.Trace_sink.within =
                (fun callback -> callback ());
              correlation = [];
              finish =
                (fun completion -> completions := completion :: !completions);
            })
          ~event_enabled:(fun _ -> false)
          ~event:(fun _ -> ())
      in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let result =
        Lwt_main.run
          (with_loopback_server ~wait_for_server:false ~allow_server_error:true
             [ { status = 200; headers = []; body = "" } ]
             (fun endpoint ->
               let connection = make_lwt_connection observer endpoint in
               Lwt.catch
                 (fun () ->
                   Awskit_s3_lwt_unix.Object.Transfer.upload_file connection
                     ~bucket ~key
                     ~on_progress:(fun _ ->
                       raise (Failure "progress callback failure"))
                     ~path:source ()
                   >>= fun _ -> Lwt.return `Returned)
                 (fun exn -> Lwt.return (`Raised exn))))
      in
      (match result with
      | `Raised (Failure message) ->
          Alcotest.(check string)
            "progress exception is preserved" "progress callback failure"
            message
      | `Raised exn ->
          Alcotest.failf "progress callback exception changed to %s"
            (Exn.to_string exn)
      | `Returned -> Alcotest.fail "progress callback unexpectedly returned");
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "callback failure completes once" 1 (List.length transfers);
      Alcotest.(check string)
        "callback failure outcome" "exception"
        (O.For_projection.Operation.Completion.outcome (List.hd_exn transfers)
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "callback failure has no logical bytes" None
        (measurement "transfer.logical_bytes" (List.hd_exn transfers));
      Alcotest.(check int64)
        "callback failure gauge released" 0L
        (gauge_value observer "upload");
      let samples = metric_values "awskit.s3.transfers" metric_observations in
      Alcotest.(check int)
        "callback failure transfer metric" 1 (List.length samples);
      match samples with
      | [ (labels, O.For_projection.Metric.Value.Int64 1L) ] ->
          Alcotest.(check (list string))
            "callback failure metric labels" [ "upload"; "exception" ] labels
      | [ (_, _) ] -> Alcotest.fail "callback failure metric value was not one"
      | _ -> Alcotest.fail "callback failure metric sample missing")

let test_native_cancellation_completes_once () =
  with_temp_file ~suffix:".source" (fun source ->
      write_file source "upload!";
      let completions = ref [] in
      let sink =
        Awskit_lwt.Observability.Trace_sink.create ~name:"transfer-cancel"
          ~needs_clock:false
          ~enabled:(fun info ->
            String.equal "awskit.s3.transfer"
              (O.For_projection.Operation.Info.name info))
          ~start:(fun _ ->
            {
              Awskit_lwt.Observability.Trace_sink.within =
                (fun callback -> callback ());
              correlation = [];
              finish =
                (fun completion -> completions := completion :: !completions);
            })
          ~event_enabled:(fun _ -> false)
          ~event:(fun _ -> ())
      in
      let observer, metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let timeline = ref [] in
      let note event = timeline := event :: !timeline in
      let request_seen, resolve_request_seen = Lwt.task () in
      let release_response, resolve_response = Lwt.task () in
      let result =
        Lwt_main.run
          (with_loopback_server ~wait_for_server:false ~allow_server_error:true
             ~on_request:(fun _method_ _headers _body ->
               note "request_seen";
               if Lwt.is_sleeping request_seen then
                 Lwt.wakeup_later resolve_request_seen ();
               Lwt.return_unit)
             ~before_response:(fun _method_ _headers _body -> release_response)
             [ { status = 200; headers = []; body = "" } ]
             (fun endpoint ->
               let connection = make_lwt_connection observer endpoint in
               let transfer =
                 Awskit_s3_lwt_unix.Object.Transfer.upload_file connection
                   ~bucket ~key ~path:source ()
               in
               note "transfer_started";
               Lwt.bind request_seen (fun () ->
                   note
                     (Fmt.str "before_cancel:sleep=%b"
                        (Lwt.is_sleeping transfer));
                   Lwt.cancel transfer;
                   note
                     (Fmt.str "after_cancel:sleep=%b" (Lwt.is_sleeping transfer));
                   Lwt.wakeup_later resolve_response ();
                   note "response_released";
                   Lwt.catch
                     (fun () -> transfer >|= fun _ -> `Returned)
                     (fun exn -> Lwt.return (`Raised exn)))))
      in
      (match result with
      | `Raised Lwt.Canceled -> ()
      | `Raised exn ->
          Alcotest.failf "native cancellation changed to %s" (Exn.to_string exn)
      | `Returned ->
          Alcotest.failf
            "cancelled transfer unexpectedly returned (timeline: %s)"
            (String.concat ~sep:" -> " (List.rev !timeline)));
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "cancelled transfer completes once" 1 (List.length transfers);
      Alcotest.(check string)
        "cancelled transfer outcome" "cancelled"
        (O.For_projection.Operation.Completion.outcome (List.hd_exn transfers)
        |> O.Outcome.to_string);
      Alcotest.(check (option int64))
        "cancelled transfer has no logical bytes" None
        (measurement "transfer.logical_bytes" (List.hd_exn transfers));
      Alcotest.(check int64)
        "cancelled transfer gauge released" 0L
        (gauge_value observer "upload");
      let samples = metric_values "awskit.s3.transfers" metric_observations in
      Alcotest.(check int) "cancelled transfer metric" 1 (List.length samples);
      match samples with
      | [ (labels, O.For_projection.Metric.Value.Int64 1L) ] ->
          Alcotest.(check (list string))
            "cancelled transfer metric labels" [ "upload"; "cancelled" ] labels
      | [ (_, _) ] ->
          Alcotest.fail "cancelled transfer metric value was not one"
      | _ -> Alcotest.fail "cancelled transfer metric sample missing")

let test_get_header_decode_error_drains_body () =
  let body = "payload" in
  let drain_completions = ref [] in
  let sink =
    Awskit_lwt.Observability.Trace_sink.create ~name:"get-header-decode-drain"
      ~needs_clock:false
      ~enabled:(fun info ->
        String.equal "awskit.http.response_body.drain"
          (O.For_projection.Operation.Info.name info))
      ~start:(fun _ ->
        {
          Awskit_lwt.Observability.Trace_sink.within =
            (fun callback -> callback ());
          correlation = [];
          finish =
            (fun completion ->
              drain_completions := completion :: !drain_completions);
        })
      ~event_enabled:(fun _ -> false)
      ~event:(fun _ -> ())
  in
  let observer, _metric_observations =
    metric_observer ~trace_sinks:[ sink ] ()
  in
  let result =
    Lwt_main.run
      (with_loopback_server
         [
           { status = 200; headers = [ ("last-modified", "not-a-date") ]; body };
         ]
         (fun endpoint ->
           let connection = make_lwt_connection observer endpoint in
           Awskit_s3_lwt_unix.Object.get connection ~bucket ~key
             ~consume:(fun _reader -> Lwt.return_ok ())
             ()))
  in
  let error =
    match result with
    | Error error -> error
    | Ok _ ->
        Alcotest.fail "malformed GetObject response unexpectedly succeeded"
  in
  (match Awskit.Error.kind error with
  | Decode _ -> ()
  | _ ->
      Alcotest.failf "malformed GetObject response returned %s"
        (Awskit.Error.to_string_hum error));
  let drains =
    List.rev !drain_completions
    |> List.filter ~f:(fun completion ->
        String.equal "awskit.http.response_body.drain"
          (O.For_projection.Operation.Info.name
             (O.For_projection.Operation.Completion.info completion)))
  in
  Alcotest.(check int) "one response drain completion" 1 (List.length drains);
  let drain = List.hd_exn drains in
  Alcotest.(check string)
    "response drain operation" "awskit.http.response_body.drain"
    (O.For_projection.Operation.Info.name
       (O.For_projection.Operation.Completion.info drain));
  Alcotest.(check string)
    "response drain outcome" "ok"
    (O.For_projection.Operation.Completion.outcome drain |> O.Outcome.to_string);
  Alcotest.(check (option int64))
    "response drain bytes"
    (Some (Int64.of_int (String.length body)))
    (measurement "http.response_body.drained_bytes" drain)

let test_download_callback_exception_cleans_target () =
  with_temp_file ~suffix:".target" (fun target ->
      Unix.unlink target;
      let completions = ref [] in
      let sink =
        Awskit_lwt.Observability.Trace_sink.create ~name:"download-callback-exn"
          ~needs_clock:false
          ~enabled:(fun info ->
            String.equal "awskit.s3.transfer"
              (O.For_projection.Operation.Info.name info))
          ~start:(fun _ ->
            {
              Awskit_lwt.Observability.Trace_sink.within =
                (fun callback -> callback ());
              correlation = [];
              finish =
                (fun completion -> completions := completion :: !completions);
            })
          ~event_enabled:(fun _ -> false)
          ~event:(fun _ -> ())
      in
      let observer, _metric_observations =
        metric_observer ~trace_sinks:[ sink ] ()
      in
      let result =
        Lwt_main.run
          (with_loopback_server ~wait_for_server:false ~allow_server_error:true
             [
               { status = 200; headers = []; body = "" };
               { status = 200; headers = []; body = "payload" };
             ]
             (fun endpoint ->
               let connection = make_lwt_connection observer endpoint in
               Lwt.catch
                 (fun () ->
                   Awskit_s3_lwt_unix.Object.Transfer.download_file connection
                     ~bucket ~key
                     ~on_progress:(fun _ ->
                       raise (Failure "download progress callback failure"))
                     ~path:target ()
                   >>= fun _ -> Lwt.return `Returned)
                 (fun exn -> Lwt.return (`Raised exn))))
      in
      (match result with
      | `Raised (Failure message) ->
          Alcotest.(check string)
            "download callback exception is preserved"
            "download progress callback failure" message
      | `Raised exn ->
          Alcotest.failf "download callback exception changed to %s"
            (Exn.to_string exn)
      | `Returned -> Alcotest.fail "download callback unexpectedly returned");
      let target_exists =
        try
          ignore (Unix.stat target);
          true
        with Unix.Unix_error (Unix.ENOENT, _, _) -> false
      in
      Alcotest.(check bool)
        "failed download target was cleaned" false target_exists;
      let transfers = transfer_completions completions in
      Alcotest.(check int)
        "download callback completion once" 1 (List.length transfers);
      let completion = List.hd_exn transfers in
      Alcotest.(check string)
        "download callback outcome" "exception"
        (O.For_projection.Operation.Completion.outcome completion
        |> O.Outcome.to_string);
      Alcotest.(check string)
        "download direction" "download"
        (Option.value_exn (dimension "transfer.direction" completion));
      Alcotest.(check (option int64))
        "failed download has no logical bytes" None
        (measurement "transfer.logical_bytes" completion);
      Alcotest.(check int64)
        "failed download gauge released" 0L
        (gauge_value observer "download"))

let () =
  Alcotest.run "awskit-transfer-observation-lwt"
    [
      ( "behavior:transfer-observation-lwt",
        [
          Alcotest.test_case "success topology and metrics" `Quick
            test_successful_upload_and_download;
          Alcotest.test_case "multipart upload topology" `Quick
            test_multipart_upload_topology;
          Alcotest.test_case "resume multipart upload topology" `Quick
            test_resume_multipart_upload_topology;
          Alcotest.test_case "ranged download topology" `Quick
            test_ranged_download_topology;
          Alcotest.test_case "failure completion" `Quick
            test_failed_upload_completes_once;
          Alcotest.test_case "progress callback exception" `Quick
            test_progress_callback_exception_completes_once;
          Alcotest.test_case "native cancellation" `Quick
            test_native_cancellation_completes_once;
          Alcotest.test_case "GetObject header decode cleanup" `Quick
            test_get_header_decode_error_drains_body;
          Alcotest.test_case "download cleanup on callback exception" `Quick
            test_download_callback_exception_cleans_target;
        ] );
    ]
