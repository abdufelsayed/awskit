module S3 = Awskit_s3_eio
module Client_contract : Awskit_s3.S = S3
module Transfer = Awskit_s3.Transfer

type server_state = { mutable object_body : string; mutable requests : int }

exception Progress_failed

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let bucket = Awskit_s3.Bucket_name.of_string_exn "transfer-bucket"
let key = Awskit_s3.Object_key.of_string_exn "object.bin"

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let remove_file path =
  match Sys.remove path with () -> () | exception Sys_error _ -> ()

let temp_siblings path =
  let dir = Filename.dirname path in
  let prefix = "." ^ Filename.basename path ^ ".awskit-download." in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name ->
      String.length name >= String.length prefix
      && String.sub name 0 (String.length prefix) = prefix)

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let response ?(headers = Cohttp.Header.init ()) ~status ~body writer =
  Cohttp_eio.Server.respond_string ~headers ~status ~body () writer

let requested_range request body =
  match Cohttp.Header.get (Cohttp.Request.headers request) "range" with
  | None -> None
  | Some value -> (
      match String.split_on_char '-' value with
      | [ start; finish ] -> (
          match
            ( int_of_string_opt (String.sub start 6 (String.length start - 6)),
              int_of_string_opt finish )
          with
          | Some start, Some finish ->
              Some (start, String.sub body start (finish - start + 1))
          | _ -> Alcotest.failf "invalid Range header %S" value)
      | _ -> Alcotest.failf "invalid Range header %S" value)

let server_callback state _conn request request_body writer =
  state.requests <- state.requests + 1;
  match Cohttp.Request.meth request with
  | `PUT ->
      state.object_body <- Eio.Flow.read_all request_body;
      response ~status:`OK ~body:"" writer
  | `HEAD ->
      let headers =
        Cohttp.Header.of_list
          [
            ("content-length", string_of_int (String.length state.object_body));
            ("etag", "\"test-etag\"");
          ]
      in
      response ~headers ~status:`OK ~body:"" writer
  | `GET -> (
      match requested_range request state.object_body with
      | None ->
          let headers = Cohttp.Header.of_list [ ("etag", "\"test-etag\"") ] in
          response ~headers ~status:`OK ~body:state.object_body writer
      | Some (start, body) ->
          let finish = start + String.length body - 1 in
          let headers =
            Cohttp.Header.of_list
              [
                ("etag", "\"test-etag\"");
                ( "content-range",
                  Printf.sprintf "bytes %d-%d/%d" start finish
                    (String.length state.object_body) );
              ]
          in
          response ~headers ~status:`Partial_content ~body writer)
  | method_ ->
      Alcotest.failf "unexpected method %s"
        (Cohttp.Code.string_of_method method_)

let listener_bind_denied_by_sandbox = function
  | Unix.Unix_error (Unix.EPERM, "bind", _) -> true
  | _ -> false

let with_server env callback f =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    try
      Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:8
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    with exn when listener_bind_denied_by_sandbox exn -> Alcotest.skip ()
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | _ -> Alcotest.fail "expected TCP listener"
  in
  let server = Cohttp_eio.Server.make ~callback () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run ~on_error:raise socket server);
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port () in
  let endpoint_config =
    Awskit_s3.Endpoint_config.local_plaintext ~endpoint
      ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
      ()
    |> ok_or_fail "endpoint config"
  in
  let client =
    S3.create ~sw ~env ~https:Awskit_eio.http_only ~region:"us-east-1"
      ~credentials ~retry_policy:Awskit.Retry.disabled ~endpoint_config ()
    |> ok_or_fail "Eio S3 client"
  in
  f sw client

let final_progress events =
  match events with
  | event :: _ -> event
  | [] -> Alcotest.fail "expected progress event"

let test_small_roundtrip env () =
  let upload_path = Filename.temp_file "awskit-eio-upload" ".bin" in
  let download_path = Filename.temp_file "awskit-eio-download" ".bin" in
  let body = String.init ((128 * 1024) + 17) (fun i -> Char.chr (i mod 251)) in
  let total = Int64.of_int (String.length body) in
  write_file upload_path body;
  remove_file download_path;
  Fun.protect
    ~finally:(fun () ->
      remove_file upload_path;
      remove_file download_path)
    (fun () ->
      let state = { object_body = "before-upload"; requests = 0 } in
      with_server env (server_callback state) (fun _sw client ->
          let fs = Eio.Stdenv.fs env in
          let upload_progress = ref [] in
          let upload_options =
            Transfer.upload_options_exn ~multipart_threshold:(Int64.succ total)
              ()
          in
          let upload =
            S3.Object.Transfer.upload_file client ~bucket ~key
              ~options:upload_options
              ~path:Eio.Path.(fs / upload_path)
              ~on_progress:(fun event ->
                upload_progress := event :: !upload_progress)
              ()
            |> ok_or_fail "upload file"
          in
          Alcotest.(check bool)
            "put strategy" true
            (Transfer.upload_strategy upload = `Put);
          Alcotest.(check int64)
            "uploaded bytes" total
            (Transfer.upload_bytes_transferred upload);
          Alcotest.(check string) "server body" body state.object_body;
          let upload_final = final_progress !upload_progress in
          Alcotest.(check int64)
            "upload final progress" total upload_final.transferred;
          Alcotest.(check bool)
            "upload phase" true
            (upload_final.phase = Transfer.Single_request);
          let download_progress = ref [] in
          let download_options =
            Transfer.download_options_exn
              ~multipart_threshold:(Int64.succ total) ()
          in
          let download =
            S3.Object.Transfer.download_file client ~bucket ~key
              ~options:download_options
              ~path:Eio.Path.(fs / download_path)
              ~on_progress:(fun event ->
                download_progress := event :: !download_progress)
              ()
            |> ok_or_fail "download file"
          in
          Alcotest.(check bool)
            "get strategy" true
            (Transfer.download_strategy download = `Get);
          Alcotest.(check int64)
            "downloaded bytes" total
            (Transfer.download_bytes_transferred download);
          Alcotest.(check string)
            "published body" body (read_file download_path);
          Alcotest.(check (list string))
            "no temporary files" []
            (temp_siblings download_path);
          let download_final = final_progress !download_progress in
          Alcotest.(check int64)
            "download final progress" total download_final.transferred))

let test_failure_cleanup_and_overwrite_preflight env () =
  let path = Filename.temp_file "awskit-eio-publication" ".bin" in
  let original = "existing-target" in
  write_file path original;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let state = { object_body = "replacement-body"; requests = 0 } in
      with_server env (server_callback state) (fun _sw client ->
          let fs = Eio.Stdenv.fs env in
          let target = Eio.Path.(fs / path) in
          let observed =
            match
              S3.Object.Transfer.download_file client ~bucket ~key ~path:target
                ~on_progress:(fun _ -> raise Progress_failed)
                ()
            with
            | result -> `Returned result
            | exception Progress_failed -> `Raised
          in
          (match observed with
          | `Raised -> ()
          | `Returned (Ok _) -> Alcotest.fail "callback failure was swallowed"
          | `Returned (Error error) ->
              Alcotest.failf "callback failure became an SDK error: %a"
                Awskit.Error.pp error);
          Alcotest.(check string)
            "existing target preserved" original (read_file path);
          Alcotest.(check (list string))
            "failed temporary file removed" [] (temp_siblings path);
          let before = state.requests in
          let options =
            Transfer.download_options_exn ~overwrite:Transfer.Error_if_exists ()
          in
          (match
             S3.Object.Transfer.download_file client ~bucket ~key ~options
               ~path:target ()
           with
          | Error error ->
              Alcotest.(check bool)
                "overwrite error is validation" true
                (Awskit.Error.is_validation error)
          | Ok _ -> Alcotest.fail "existing target should be rejected");
          Alcotest.(check int)
            "overwrite rejected before transport" before state.requests))

let test_ranged_download_strategy env () =
  let path = Filename.temp_file "awskit-eio-ranged" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let body = String.make ((128 * 1024) + 19) 'r' in
      let state = { object_body = body; requests = 0 } in
      with_server env (server_callback state) (fun _sw client ->
          let progress = ref [] in
          let options =
            Transfer.download_options_exn ~multipart_threshold:1L
              ~part_size:Transfer.min_part_size ~concurrency:2 ()
          in
          let result =
            S3.Object.Transfer.download_file client ~bucket ~key ~options
              ~path:Eio.Path.(Eio.Stdenv.fs env / path)
              ~on_progress:(fun event -> progress := event :: !progress)
              ()
            |> ok_or_fail "ranged download"
          in
          Alcotest.(check bool)
            "ranged strategy" true
            (Transfer.download_strategy result = `Ranged);
          Alcotest.(check string) "ranged body" body (read_file path);
          let final = final_progress !progress in
          Alcotest.(check bool)
            "ranged phase" true
            (final.phase = Transfer.Ranged_get);
          Alcotest.(check int64)
            "ranged progress"
            (Int64.of_int (String.length body))
            final.transferred))

let test_cancellation_cleanup env () =
  let path = Filename.temp_file "awskit-eio-cancel" ".bin" in
  let original = "existing-target" in
  write_file path original;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let state = { object_body = "replacement-body"; requests = 0 } in
      with_server env (server_callback state) (fun _sw client ->
          let target = Eio.Path.(Eio.Stdenv.fs env / path) in
          let canceled =
            try
              Eio.Cancel.sub (fun context ->
                  ignore
                    (S3.Object.Transfer.download_file client ~bucket ~key
                       ~path:target
                       ~on_progress:(fun _ ->
                         Eio.Cancel.cancel context Exit;
                         Eio.Fiber.yield ())
                       ()));
              false
            with Eio.Cancel.Cancelled Exit -> true
          in
          Alcotest.(check bool) "native cancellation preserved" true canceled;
          Alcotest.(check string) "target preserved" original (read_file path);
          Alcotest.(check (list string))
            "canceled temporary file removed" [] (temp_siblings path)))

type multipart_state = { mutable deletes : int; mutable puts : int }

let query_has uri name =
  Uri.query uri |> List.exists (fun (key, _values) -> String.equal key name)

let multipart_server state ~cleanup_fails _conn request request_body writer =
  let uri = Cohttp.Request.uri request in
  match (Cohttp.Request.meth request, query_has uri "uploads") with
  | `POST, true ->
      response ~status:`OK
        ~body:
          "<InitiateMultipartUploadResult><Bucket>transfer-bucket</Bucket><Key>object.bin</Key><UploadId>upload-1</UploadId></InitiateMultipartUploadResult>"
        writer
  | `GET, false when query_has uri "uploadId" ->
      response ~status:`OK
        ~body:
          "<ListPartsResult><Bucket>transfer-bucket</Bucket><Key>object.bin</Key><UploadId>upload-1</UploadId><IsTruncated>false</IsTruncated></ListPartsResult>"
        writer
  | `PUT, false when query_has uri "partNumber" ->
      state.puts <- state.puts + 1;
      ignore (Eio.Flow.read_all request_body : string);
      response ~status:`Internal_server_error
        ~body:
          "<Error><Code>InternalError</Code><Message>part \
           failed</Message></Error>"
        writer
  | `DELETE, false when query_has uri "uploadId" ->
      state.deletes <- state.deletes + 1;
      if cleanup_fails then
        response ~status:`Internal_server_error
          ~body:
            "<Error><Code>InternalError</Code><Message>abort \
             failed</Message></Error>"
          writer
      else response ~status:`No_content ~body:"" writer
  | method_, _ ->
      Alcotest.failf "unexpected multipart method %s uri=%s"
        (Cohttp.Code.string_of_method method_)
        (Uri.to_string uri)

let successful_multipart_server state _conn request request_body writer =
  let uri = Cohttp.Request.uri request in
  match (Cohttp.Request.meth request, query_has uri "uploads") with
  | `POST, true ->
      response ~status:`OK
        ~body:
          "<InitiateMultipartUploadResult><Bucket>transfer-bucket</Bucket><Key>object.bin</Key><UploadId>upload-1</UploadId></InitiateMultipartUploadResult>"
        writer
  | `PUT, false when query_has uri "partNumber" ->
      state.puts <- state.puts + 1;
      ignore (Eio.Flow.read_all request_body : string);
      let headers =
        Cohttp.Header.of_list
          [ ("etag", Printf.sprintf "\"part-%d\"" state.puts) ]
      in
      response ~headers ~status:`OK ~body:"" writer
  | `POST, false when query_has uri "uploadId" ->
      ignore (Eio.Flow.read_all request_body : string);
      response ~status:`OK
        ~body:
          "<CompleteMultipartUploadResult><Location>http://localhost/transfer-bucket/object.bin</Location><Bucket>transfer-bucket</Bucket><Key>object.bin</Key><ETag>\"complete\"</ETag></CompleteMultipartUploadResult>"
        writer
  | method_, _ ->
      Alcotest.failf "unexpected successful multipart method %s uri=%s"
        (Cohttp.Code.string_of_method method_)
        (Uri.to_string uri)

let test_multipart_upload_strategy env () =
  let path = Filename.temp_file "awskit-eio-multipart-success" ".bin" in
  let length = Transfer.min_part_size + 17 in
  write_file path (String.make length 'm');
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let state = { deletes = 0; puts = 0 } in
      with_server env (successful_multipart_server state) (fun _sw client ->
          let progress = ref [] in
          let options =
            Transfer.upload_options_exn ~multipart_threshold:1L
              ~part_size:Transfer.min_part_size ~concurrency:2 ()
          in
          let result =
            S3.Object.Transfer.upload_file client ~bucket ~key ~options
              ~path:Eio.Path.(Eio.Stdenv.fs env / path)
              ~on_progress:(fun event -> progress := event :: !progress)
              ()
            |> ok_or_fail "multipart upload"
          in
          Alcotest.(check bool)
            "multipart strategy" true
            (Transfer.upload_strategy result = `Multipart);
          Alcotest.(check int64)
            "multipart bytes" (Int64.of_int length)
            (Transfer.upload_bytes_transferred result);
          Alcotest.(check int) "two uploaded parts" 2 state.puts;
          let final = final_progress !progress in
          Alcotest.(check bool)
            "part progress phase" true
            (final.phase = Transfer.Part);
          Alcotest.(check int64)
            "multipart progress" (Int64.of_int length) final.transferred))

let test_multipart_cleanup_ownership env () =
  let path = Filename.temp_file "awskit-eio-multipart" ".bin" in
  let body = String.make Transfer.min_part_size 'x' in
  write_file path body;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let options =
        Transfer.upload_options_exn ~part_size:Transfer.min_part_size
          ~concurrency:1 ()
      in
      let eio_path = Eio.Path.(Eio.Stdenv.fs env / path) in
      let created_state = { deletes = 0; puts = 0 } in
      with_server env (multipart_server created_state ~cleanup_fails:true)
        (fun _sw client ->
          match
            S3.Object.Transfer.multipart_upload_file client ~bucket ~key
              ~options ~path:eio_path ()
          with
          | Error error -> (
              match Awskit.Error.kind error with
              | Multiple errors ->
                  Alcotest.(check bool)
                    "primary and cleanup failures retained" true
                    (List.length errors >= 2)
              | _ ->
                  Alcotest.failf "expected multiple transfer errors: %a"
                    Awskit.Error.pp error)
          | Ok _ -> Alcotest.fail "failing multipart upload succeeded");
      Alcotest.(check int) "owned upload aborted" 1 created_state.deletes;
      let resumed_state = { deletes = 0; puts = 0 } in
      with_server env (multipart_server resumed_state ~cleanup_fails:false)
        (fun _sw client ->
          let upload =
            Awskit_s3.Multipart.Upload.resume ~bucket ~key
              ~upload_id:
                (Awskit_s3.Multipart.Upload_id.of_string_exn "upload-1")
          in
          match
            S3.Object.Transfer.resume_multipart_upload_file client ~upload
              ~options ~path:eio_path ()
          with
          | Error _ -> ()
          | Ok _ -> Alcotest.fail "failing resumed upload succeeded");
      Alcotest.(check int)
        "caller-owned upload not aborted" 0 resumed_state.deletes;
      Alcotest.(check int) "resumed part attempted" 1 resumed_state.puts)

let suite env =
  [
    ( "integration:awskit-s3-eio:transfer",
      [
        Alcotest.test_case "small upload/download and publication" `Quick
          (test_small_roundtrip env);
        Alcotest.test_case "failure cleanup and overwrite preflight" `Quick
          (test_failure_cleanup_and_overwrite_preflight env);
        Alcotest.test_case "ranged download strategy" `Quick
          (test_ranged_download_strategy env);
        Alcotest.test_case "multipart upload strategy" `Quick
          (test_multipart_upload_strategy env);
        Alcotest.test_case "native cancellation cleanup" `Quick
          (test_cancellation_cleanup env);
        Alcotest.test_case "multipart cleanup ownership" `Quick
          (test_multipart_cleanup_ownership env);
      ] );
  ]

let () = Eio_main.run @@ fun env -> Alcotest.run "awskit-s3-eio" (suite env)
