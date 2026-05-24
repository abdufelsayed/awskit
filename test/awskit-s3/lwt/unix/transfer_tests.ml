module Runtime = struct
  type connection = { download_body : string }
  type 'a t = 'a Lwt.t
  type upload_body = string
  type download_body = string
  type upload_writer = Buffer.t
  type download_reader = { body : string; mutable offset : int }

  let return = Lwt.return
  let bind = Lwt.bind
  let now _ = Ptime.epoch
  let region _ = Awskit.Region.of_string_exn "us-east-1"

  let credentials _ =
    Lwt.return_ok
      (Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK"
         ())

  let endpoint _ = None
  let retry_policy _ = Awskit.Retry.default
  let sleep _ _ = Lwt.return_unit
  let s3_endpoint_config _ = Awskit_s3.default_endpoint_config
  let empty_body = ""
  let string_body value = value
  let bytes_body = Bytes.to_string
  let stream_body _descriptor ~write:_ = ""

  let upload_descriptor body =
    {
      Awskit.Body.Upload.content_length =
        Some (Int64.of_int (String.length body));
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable = true;
    }

  let write_string writer value =
    Buffer.add_string writer value;
    Lwt.return_ok ()

  let read reader bytes ~off ~len =
    if len = 0 then Lwt.return_ok 0
    else
      let remaining = String.length reader.body - reader.offset in
      if remaining <= 0 then Lwt.return_ok 0
      else
        let copied = min len remaining in
        String.blit reader.body reader.offset bytes off copied;
        reader.offset <- reader.offset + copied;
        Lwt.return_ok copied

  let with_download_body body ~consume = consume { body; offset = 0 }
  let discard_download_body _ = Lwt.return_ok ()

  let call _ _ _ =
    Lwt.return_error (Awskit.Error.transport ~retryable:false "not implemented")
end

let unsupported () =
  Lwt.return_error (Awskit.Error.validation ~field:"test" "not implemented")

module S3 = struct
  module Object = struct
    type connection = Runtime.connection
    type 'a io = 'a Lwt.t
    type upload_body = Runtime.upload_body
    type download_reader = Runtime.download_reader

    let put _ ~bucket:_ ~key:_ ?options:_ ~body:_ () = unsupported ()

    let get conn ~bucket:_ ~key:_ ?options:_ ~consume () =
      Lwt.bind
        (consume { Runtime.body = conn.Runtime.download_body; offset = 0 })
        (function
          | Error _ as error -> Lwt.return error
          | Ok value ->
              let request = Awskit.Response.create_exn ~status:200 () in
              Lwt.return_ok
                ( {
                    Awskit_s3.Object.Get.etag = None;
                    content_type = None;
                    content_length =
                      Some
                        (Int64.of_int
                           (String.length conn.Runtime.download_body));
                    last_modified = None;
                    metadata = [];
                    storage_class = None;
                    version_id = None;
                    checksum = None;
                    server_side_encryption = None;
                    request;
                  },
                  value ))

    let head _ ~bucket:_ ~key:_ ?options:_ () = unsupported ()
    let exists _ ~bucket:_ ~key:_ = unsupported ()
    let delete _ ~bucket:_ ~key:_ ?options:_ () = unsupported ()
    let delete_many _ ~bucket:_ ~objects:_ = unsupported ()

    let copy _ ~src_bucket:_ ~src_key:_ ~dst_bucket:_ ~dst_key:_ ?options:_ () =
      unsupported ()

    let list_versions _ ~bucket:_ ?options:_ () = unsupported ()
    let list _ ~bucket:_ ?options:_ () = unsupported ()
    let list_keys _ ~bucket:_ ?options:_ () = unsupported ()

    module Paginator = struct
      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        unsupported ()

      let pages _ ~bucket:_ ?options:_ ?max_pages:_ () = unsupported ()
      let objects _ ~bucket:_ ?options:_ ?max_pages:_ () = unsupported ()
      let keys _ ~bucket:_ ?options:_ ?max_pages:_ () = unsupported ()
    end

    module Versions = struct
      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        unsupported ()

      let pages _ ~bucket:_ ?options:_ ?max_pages:_ () = unsupported ()

      let object_versions _ ~bucket:_ ?options:_ ?max_pages:_ () =
        unsupported ()

      let delete_markers _ ~bucket:_ ?options:_ ?max_pages:_ () = unsupported ()
    end

    module Buffer = struct
      let put_string _ ~bucket:_ ~key:_ ?options:_ _ = unsupported ()
      let put_bytes _ ~bucket:_ ~key:_ ?options:_ _ = unsupported ()

      let get_string _ ~bucket:_ ~key:_ ~max_size:_ ?options:_ () =
        unsupported ()

      let get_bytes _ ~bucket:_ ~key:_ ~max_size:_ ?options:_ () =
        unsupported ()
    end

    module Tagging = struct
      let get _ ~bucket:_ ~key:_ = unsupported ()
      let put _ ~bucket:_ ~key:_ _ = unsupported ()
      let delete _ ~bucket:_ ~key:_ = unsupported ()
    end
  end

  module Multipart = struct
    type connection = Runtime.connection
    type 'a io = 'a Lwt.t
    type upload_body = Runtime.upload_body

    let create _ ~bucket:_ ~key:_ ?options:_ () = unsupported ()

    let upload_part _ ~bucket:_ ~key:_ ~upload_id:_ ~part_number:_ ~body:_
        ?options:_ () =
      unsupported ()

    let complete _ ~bucket:_ ~key:_ ~upload_id:_ _ = unsupported ()
    let abort _ ~bucket:_ ~key:_ ~upload_id:_ = unsupported ()

    let list_parts _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ () =
      unsupported ()

    module Paginator = struct
      let fold_pages _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ ?max_pages:_
          ~init:_ ~f:_ () =
        unsupported ()

      let pages _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ ?max_pages:_ () =
        unsupported ()

      let parts _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ ?max_pages:_ () =
        unsupported ()
    end

    module Managed = struct
      let upload_string _ ~bucket:_ ~key:_ ?options:_ _ = unsupported ()
      let upload_bytes _ ~bucket:_ ~key:_ ?options:_ _ = unsupported ()
    end
  end
end

module Transfer = Awskit_s3_lwt_unix__Transfer.Make (Runtime) (S3)

let remove_file path = try Sys.remove path with Sys_error _ -> ()

let with_umask mask f =
  let previous = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask previous)) f

let test_download_to_path_creates_private_file () =
  let path = Filename.temp_file "awskit-download-perm" ".bin" in
  let body = "secret download body" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      with_umask 0 (fun () ->
          match
            Lwt_main.run
              (Transfer.download_to_path
                 { Runtime.download_body = body }
                 ~bucket:"bucket" ~key:"key" ~path ())
          with
          | Error error ->
              Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
          | Ok _ -> ());
      let stat = Unix.stat path in
      Alcotest.(check int) "mode" 0o600 (stat.Unix.st_perm land 0o777))

let suite () =
  [
    ( "transfer",
      [
        Alcotest.test_case "download creates private file" `Quick
          test_download_to_path_creates_private_file;
      ] );
  ]
