module S3 = Awskit_s3_lwt_unix
module Client_contract : Awskit_s3.S = S3
module Transfer = Awskit_s3.Transfer

type state = {
  mutable object_body : string;
  mutable requests : int;
  mutable deletes : int;
  mutable part_puts : int;
}

type strategy_state = {
  mutable creates : int;
  mutable single_puts : int;
  mutable part_puts : int;
  mutable deletes : int;
}

exception Progress_failed

let credentials =
  Awskit.Credentials.create_exn ~access_key_id:"AKID"
    ~secret_access_key:"SECRET" ()

let bucket = Awskit_s3.Bucket_name.of_string_exn "transfer-bucket"
let key = Awskit_s3.Object_key.of_string_exn "object.bin"

let ok_or_fail label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Awskit.Error.pp error

let write_file path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let write_sparse_file path length =
  let fd = Unix.openfile path [ Unix.O_CREAT; Unix.O_RDWR ] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.LargeFile.ftruncate fd length)

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

let response ?(headers = Cohttp.Header.init ()) status body =
  (Cohttp.Response.make ~headers ~status (), Cohttp_lwt.Body.of_string body)

let query_has uri name =
  Uri.query uri |> List.exists (fun (key, _values) -> String.equal key name)

let install_simple_transport state =
  Cohttp_lwt_unix.Client.set_cache
    (fun ?headers:_ ?(body = Cohttp_lwt.Body.empty) ?absolute_form:_ meth uri ->
      state.requests <- state.requests + 1;
      match meth with
      | `PUT ->
          Lwt.bind (Cohttp_lwt.Body.to_string body) (fun body ->
              state.object_body <- body;
              Lwt.return (response `OK ""))
      | `HEAD ->
          let headers =
            Cohttp.Header.of_list
              [
                ( "content-length",
                  string_of_int (String.length state.object_body) );
                ("etag", "\"test-etag\"");
              ]
          in
          Lwt.return (response ~headers `OK "")
      | `GET ->
          let headers = Cohttp.Header.of_list [ ("etag", "\"test-etag\"") ] in
          Lwt.return (response ~headers `OK state.object_body)
      | method_ ->
          Alcotest.failf "unexpected method %s uri=%s"
            (Cohttp.Code.string_of_method method_)
            (Uri.to_string uri))

let install_multipart_transport state ~cleanup_fails =
  Cohttp_lwt_unix.Client.set_cache
    (fun ?headers:_ ?(body = Cohttp_lwt.Body.empty) ?absolute_form:_ meth uri ->
      state.requests <- state.requests + 1;
      match (meth, query_has uri "uploads") with
      | `POST, true ->
          Lwt.return
            (response `OK
               "<InitiateMultipartUploadResult><Bucket>transfer-bucket</Bucket><Key>object.bin</Key><UploadId>upload-1</UploadId></InitiateMultipartUploadResult>")
      | `GET, false when query_has uri "uploadId" ->
          Lwt.return
            (response `OK
               "<ListPartsResult><Bucket>transfer-bucket</Bucket><Key>object.bin</Key><UploadId>upload-1</UploadId><MaxParts>1000</MaxParts><IsTruncated>true</IsTruncated><NextPartNumberMarker>1000</NextPartNumberMarker></ListPartsResult>")
      | `PUT, false when query_has uri "partNumber" ->
          state.part_puts <- state.part_puts + 1;
          Lwt.bind (Cohttp_lwt.Body.to_string body) (fun _ ->
              Lwt.return
                (response `Internal_server_error
                   "<Error><Code>InternalError</Code><Message>part \
                    failed</Message></Error>"))
      | `DELETE, false when query_has uri "uploadId" ->
          state.deletes <- state.deletes + 1;
          if cleanup_fails then
            Lwt.return
              (response `Internal_server_error
                 "<Error><Code>InternalError</Code><Message>abort \
                  failed</Message></Error>")
          else Lwt.return (response `No_content "")
      | method_, _ ->
          Alcotest.failf "unexpected multipart method %s uri=%s"
            (Cohttp.Code.string_of_method method_)
            (Uri.to_string uri))

let install_forced_multipart_transport state =
  Cohttp_lwt_unix.Client.set_cache
    (fun ?headers:_ ?(body = Cohttp_lwt.Body.empty) ?absolute_form:_ meth uri ->
      match (meth, query_has uri "uploads") with
      | `POST, true ->
          state.creates <- state.creates + 1;
          Lwt.return
            (response `OK
               "<InitiateMultipartUploadResult><Bucket>transfer-bucket</Bucket><Key>object.bin</Key><UploadId>upload-1</UploadId></InitiateMultipartUploadResult>")
      | `PUT, false when query_has uri "partNumber" ->
          state.part_puts <- state.part_puts + 1;
          Lwt.return
            (response `Internal_server_error
               "<Error><Code>InternalError</Code><Message>part \
                failed</Message></Error>")
      | `PUT, false ->
          state.single_puts <- state.single_puts + 1;
          Lwt.return
            (response `Internal_server_error
               "<Error><Code>InternalError</Code><Message>unexpected \
                PutObject</Message></Error>")
      | `DELETE, false when query_has uri "uploadId" ->
          state.deletes <- state.deletes + 1;
          Lwt.return (response `No_content "")
      | method_, _ ->
          Alcotest.failf "unexpected forced strategy method %s uri=%s"
            (Cohttp.Code.string_of_method method_)
            (Uri.to_string uri))

let create_client () =
  let endpoint = Awskit.Endpoint.http_exn ~host:"127.0.0.1" ~port:9 () in
  let endpoint_config =
    Awskit_s3.Endpoint_config.local_plaintext ~endpoint
      ~signing_region:(Awskit.Region.of_string_exn "us-east-1")
      ()
    |> ok_or_fail "endpoint config"
  in
  S3.create ~endpoint_config ~region:"us-east-1" ~credentials
    ~clock:(fun () -> Ptime.epoch)
    ~retry_policy:Awskit.Retry.disabled ~timeout_policy:Awskit.Timeout.disabled
    ()
  |> ok_or_fail "Lwt Unix S3 client"

let await result = Lwt_main.run result

let test_roundtrip_publication_and_preflight () =
  let upload_path = Filename.temp_file "awskit-lwt-upload" ".bin" in
  let download_path = Filename.temp_file "awskit-lwt-download" ".bin" in
  let body = String.init ((128 * 1024) + 17) (fun i -> Char.chr (i mod 251)) in
  let total = Int64.of_int (String.length body) in
  write_file upload_path body;
  remove_file download_path;
  Fun.protect
    ~finally:(fun () ->
      remove_file upload_path;
      remove_file download_path)
    (fun () ->
      let state =
        {
          object_body = "before-upload";
          requests = 0;
          deletes = 0;
          part_puts = 0;
        }
      in
      install_simple_transport state;
      let client = create_client () in
      let upload_progress = ref [] in
      let upload_options =
        Transfer.upload_options_exn ~multipart_threshold:(Int64.succ total) ()
      in
      let upload =
        await
          (S3.Object.Transfer.upload_file client ~bucket ~key
             ~options:upload_options ~path:upload_path
             ~on_progress:(fun event ->
               upload_progress := event :: !upload_progress)
             ())
        |> ok_or_fail "upload file"
      in
      Alcotest.(check bool)
        "put strategy" true
        (Transfer.upload_strategy upload = `Put);
      Alcotest.(check int64)
        "uploaded bytes" total
        (Transfer.upload_bytes_transferred upload);
      Alcotest.(check string) "server body" body state.object_body;
      let download =
        await
          (S3.Object.Transfer.download_file client ~bucket ~key
             ~options:
               (Transfer.download_options_exn
                  ~multipart_threshold:(Int64.succ total) ())
             ~path:download_path ())
        |> ok_or_fail "download file"
      in
      Alcotest.(check bool)
        "get strategy" true
        (Transfer.download_strategy download = `Get);
      Alcotest.(check string) "published body" body (read_file download_path);
      Alcotest.(check (list string))
        "no temporary files" []
        (temp_siblings download_path);
      let before = state.requests in
      let options =
        Transfer.download_options_exn ~overwrite:Transfer.Error_if_exists ()
      in
      (match
         await
           (S3.Object.Transfer.download_file client ~bucket ~key ~options
              ~path:download_path ())
       with
      | Error error ->
          Alcotest.(check bool)
            "overwrite error is validation" true
            (Awskit.Error.is_validation error)
      | Ok _ -> Alcotest.fail "existing target should be rejected");
      Alcotest.(check int)
        "overwrite rejected before transport" before state.requests)

let test_callback_and_cancellation_cleanup () =
  let run ~on_progress label is_expected =
    let path = Filename.temp_file label ".bin" in
    let original = "existing-target" in
    write_file path original;
    Fun.protect
      ~finally:(fun () -> remove_file path)
      (fun () ->
        let state =
          {
            object_body = "replacement-body";
            requests = 0;
            deletes = 0;
            part_puts = 0;
          }
        in
        install_simple_transport state;
        let client = create_client () in
        let observed =
          try
            `Returned
              (await
                 (S3.Object.Transfer.download_file client ~bucket ~key ~path
                    ~on_progress ()))
          with exn -> `Raised exn
        in
        (match observed with
        | `Raised exn when is_expected exn -> ()
        | `Raised exn -> raise exn
        | `Returned (Ok _) -> Alcotest.fail "progress failure was swallowed"
        | `Returned (Error error) ->
            Alcotest.failf "progress failure became SDK error: %a"
              Awskit.Error.pp error);
        Alcotest.(check string)
          "existing target preserved" original (read_file path);
        Alcotest.(check (list string))
          "temporary file removed" [] (temp_siblings path))
  in
  run
    ~on_progress:(fun _ -> raise Progress_failed)
    "awskit-lwt-callback"
    (function Progress_failed -> true | _ -> false);
  run
    ~on_progress:(fun _ -> raise Lwt.Canceled)
    "awskit-lwt-cancel"
    (function Lwt.Canceled -> true | _ -> false)

let test_oversized_upload_forces_multipart () =
  let path = Filename.temp_file "awskit-lwt-oversized-upload" ".bin" in
  let content_length = Int64.succ Transfer.max_single_request_size in
  write_sparse_file path content_length;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let state =
        { creates = 0; single_puts = 0; part_puts = 0; deletes = 0 }
      in
      install_forced_multipart_transport state;
      let client = create_client () in
      let options =
        Transfer.upload_options_exn
          ~multipart_threshold:(Int64.succ Transfer.max_single_request_size)
          ~concurrency:1 ()
      in
      (match
         await
           (S3.Object.Transfer.upload_file client ~bucket ~key ~options ~path ())
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "forced multipart upload unexpectedly succeeded");
      Alcotest.(check int) "multipart upload created" 1 state.creates;
      Alcotest.(check int) "multipart part attempted" 1 state.part_puts;
      Alcotest.(check int) "single PutObject not selected" 0 state.single_puts;
      Alcotest.(check int) "failed upload aborted" 1 state.deletes)

let test_multipart_cleanup_ownership () =
  let path = Filename.temp_file "awskit-lwt-multipart" ".bin" in
  write_file path (String.make Transfer.min_part_size 'x');
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let options =
        Transfer.upload_options_exn ~part_size:Transfer.min_part_size
          ~concurrency:1 ()
      in
      let created =
        { object_body = ""; requests = 0; deletes = 0; part_puts = 0 }
      in
      install_multipart_transport created ~cleanup_fails:true;
      let client = create_client () in
      (match
         await
           (S3.Object.Transfer.multipart_upload_file client ~bucket ~key
              ~options ~path ())
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
      Alcotest.(check int) "owned upload aborted" 1 created.deletes;
      let resumed =
        { object_body = ""; requests = 0; deletes = 0; part_puts = 0 }
      in
      install_multipart_transport resumed ~cleanup_fails:false;
      let upload =
        Awskit_s3.Multipart.Upload.resume ~bucket ~key
          ~upload_id:(Awskit_s3.Multipart.Upload_id.of_string_exn "upload-1")
      in
      (match
         await
           (S3.Object.Transfer.resume_multipart_upload_file client ~upload
              ~options ~path ())
       with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "failing resumed upload succeeded");
      Alcotest.(check int) "caller-owned upload not aborted" 0 resumed.deletes;
      Alcotest.(check int) "resumed part attempted" 1 resumed.part_puts)

let () =
  Alcotest.run "awskit-s3-lwt-unix"
    [
      ( "integration:awskit-s3-lwt-unix:transfer",
        [
          Alcotest.test_case "roundtrip publication and overwrite preflight"
            `Quick test_roundtrip_publication_and_preflight;
          Alcotest.test_case "callback and cancellation cleanup" `Quick
            test_callback_and_cancellation_cleanup;
          Alcotest.test_case "oversized upload forces multipart" `Quick
            test_oversized_upload_forces_multipart;
          Alcotest.test_case "multipart cleanup ownership" `Quick
            test_multipart_cleanup_ownership;
        ] );
    ]
