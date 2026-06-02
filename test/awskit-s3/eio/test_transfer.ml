let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

module Runtime = struct
  type connection = {
    response_body : string;
    mutable uploaded_body : string option;
    mutable put_count : int;
    mutable get_count : int;
    mutable head_count : int;
    mutable multipart_create_count : int;
    mutable upload_part_count : int;
    mutable complete_count : int;
    mutable get_ranges : string list;
  }

  type 'a t = 'a
  type request_body_writer = { buffer : Buffer.t }

  type request_body =
    | Body of
        Awskit.Body.Request.descriptor * (string, Awskit_s3.Error.t) result
    | Stream of
        Awskit.Body.Request.descriptor
        * (request_body_writer -> (unit, Awskit_s3.Error.t) result)

  type response_body = string
  type response_body_reader = { body : string; mutable offset : int }

  let return value = value
  let bind value f = f value
  let now _ = Ptime.epoch
  let region _ = Awskit.Region.of_string_exn "us-east-1"

  let credentials _ =
    Ok
      (Awskit.Credentials.create_exn ~access_key_id:"AK" ~secret_access_key:"SK"
         ())

  let endpoint _ = None
  let retry_policy _ = Awskit.Retry.default
  let sleep _ _ = ()
  let s3_endpoint_config _ = Awskit_s3.default_endpoint_config

  let descriptor_for_string body =
    {
      Awskit.Body.Request.content_length =
        Some (Int64.of_int (String.length body));
      payload_hash = Awskit.Body.Payload_hash.sha256_of_string body;
      replayable = true;
    }

  let empty_request_body = Body (descriptor_for_string "", Ok "")
  let string_request_body value = Body (descriptor_for_string value, Ok value)
  let bytes_request_body value = string_request_body (Bytes.to_string value)
  let stream_request_body descriptor ~write = Stream (descriptor, write)

  let drain_request_body = function
    | Body (_, body) -> body
    | Stream (_, write) ->
        let writer = { buffer = Buffer.create 1024 } in
        Result.map (fun () -> Buffer.contents writer.buffer) (write writer)

  let request_body_descriptor = function
    | Body (descriptor, _) | Stream (descriptor, _) -> descriptor

  let write_request_body_string writer value =
    Buffer.add_string writer.buffer value;
    Ok ()

  let read_response_body reader bytes ~off ~len =
    if len = 0 then Ok 0
    else
      let remaining = String.length reader.body - reader.offset in
      if remaining <= 0 then Ok 0
      else
        let copied = min len remaining in
        String.blit reader.body reader.offset bytes off copied;
        reader.offset <- reader.offset + copied;
        Ok copied

  let with_response_body body ~consume = consume { body; offset = 0 }
  let discard_response_body _ = Ok ()

  module Request_body = struct
    let empty = empty_request_body
    let of_string = string_request_body
    let of_bytes = bytes_request_body
    let of_stream = stream_request_body
    let descriptor = request_body_descriptor
    let write_string = write_request_body_string
  end

  module Response_body = struct
    let read = read_response_body
    let with_reader = with_response_body
    let discard = discard_response_body
  end

  let with_response _ _ _ ~f:_ =
    Error (Awskit.Error.transport ~retryable:false "not implemented")
end

let connection ?(response_body = "") () =
  {
    Runtime.response_body;
    uploaded_body = None;
    put_count = 0;
    get_count = 0;
    head_count = 0;
    multipart_create_count = 0;
    upload_part_count = 0;
    complete_count = 0;
    get_ranges = [];
  }

let response status = Awskit.Response.create_exn ~status ()

let empty_checksum : Awskit_s3.Object.Checksum.response =
  { values = []; checksum_type = None }

let put_result () : Awskit_s3.Put_object.result =
  {
    etag = None;
    version_id = None;
    checksum = empty_checksum;
    response = response 200;
  }

let get_result content_length : Awskit_s3.Get_object.result =
  {
    etag = None;
    content_type = None;
    content_length;
    last_modified = None;
    metadata = [];
    storage_class = None;
    version_id = None;
    checksum = empty_checksum;
    server_side_encryption = None;
    response = response 200;
  }

let parse_range_header header body =
  match String.split_on_char '-' header with
  | [ start_part; finish_part ] -> (
      match String.split_on_char '=' start_part with
      | [ "bytes"; start ] ->
          let start = int_of_string start in
          let finish = int_of_string finish_part in
          String.sub body start (finish - start + 1)
      | _ -> body)
  | _ -> body

module S3 = struct
  module Object = struct
    type connection = Runtime.connection
    type 'a io = 'a
    type request_body = Runtime.request_body
    type response_body_reader = Runtime.response_body_reader

    let put conn ~bucket:_ ~key:_ ?options:_ ~body () =
      conn.Runtime.put_count <- conn.Runtime.put_count + 1;
      let body = Runtime.drain_request_body body in
      Result.map
        (fun body ->
          conn.Runtime.uploaded_body <- Some body;
          put_result ())
        body

    let get conn ~bucket:_ ~key:_ ?options ~consume () =
      conn.Runtime.get_count <- conn.Runtime.get_count + 1;
      let body =
        match
          Option.bind options (fun (options : Awskit_s3.Get_object.options) ->
              options.range)
        with
        | None -> conn.Runtime.response_body
        | Some range ->
            let header = Awskit_s3.Range.to_header range in
            conn.Runtime.get_ranges <- conn.Runtime.get_ranges @ [ header ];
            parse_range_header header conn.Runtime.response_body
      in
      let consumed = consume { Runtime.body; offset = 0 } in
      Result.map
        (fun value ->
          ( get_result
              (Some (Int64.of_int (String.length conn.Runtime.response_body))),
            value ))
        consumed

    let head conn ~bucket:_ ~key:_ ?options:_ () =
      conn.Runtime.head_count <- conn.Runtime.head_count + 1;
      Ok
        (get_result
           (Some (Int64.of_int (String.length conn.Runtime.response_body))))

    let exists _ ~bucket:_ ~key:_ = assert false
    let delete _ ~bucket:_ ~key:_ ?options:_ () = assert false
    let delete_objects _ ~bucket:_ ~objects:_ ?options:_ () = assert false

    let copy _ ~source_bucket:_ ~source_key:_ ~destination_bucket:_
        ~destination_key:_ ?options:_ () =
      assert false

    let list_versions _ ~bucket:_ ?options:_ () = assert false
    let list _ ~bucket:_ ?options:_ () = assert false
    let list_keys _ ~bucket:_ ?options:_ () = assert false

    module List_objects_v2 = struct
      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        assert false

      let pages _ ~bucket:_ ?options:_ ?max_pages:_ () = assert false
      let objects _ ~bucket:_ ?options:_ ?max_pages:_ () = assert false
      let keys _ ~bucket:_ ?options:_ ?max_pages:_ () = assert false
    end

    module List_object_versions = struct
      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        assert false

      let pages _ ~bucket:_ ?options:_ ?max_pages:_ () = assert false
      let object_versions _ ~bucket:_ ?options:_ ?max_pages:_ () = assert false
      let delete_markers _ ~bucket:_ ?options:_ ?max_pages:_ () = assert false
    end

    let put_string _ ~bucket:_ ~key:_ ?options:_ _ = assert false
    let put_bytes _ ~bucket:_ ~key:_ ?options:_ _ = assert false

    let get_as_string _ ~bucket:_ ~key:_ ~max_bytes:_ ?options:_ () =
      assert false

    let get_as_bytes _ ~bucket:_ ~key:_ ~max_bytes:_ ?options:_ () =
      assert false

    module Tagging = struct
      let get _ ~bucket:_ ~key:_ = assert false
      let put _ ~bucket:_ ~key:_ _ = assert false
      let delete _ ~bucket:_ ~key:_ = assert false
    end
  end

  module Multipart = struct
    type connection = Runtime.connection
    type 'a io = 'a
    type request_body = Runtime.request_body

    let create_upload conn ~bucket ~key ?options:_ () =
      conn.Runtime.multipart_create_count <-
        conn.Runtime.multipart_create_count + 1;
      let upload_id = Awskit_s3.Multipart.Upload_id.of_string_exn "upload-1" in
      let upload =
        Awskit_s3.Multipart.Upload.create_exn ~bucket ~key ~upload_id
      in
      Ok { Awskit_s3.Create_multipart_upload.upload; response = response 200 }

    let upload_part conn ~bucket:_ ~key:_ ~upload_id:_ ~part_number ~body
        ?options:_ () =
      conn.Runtime.upload_part_count <- conn.Runtime.upload_part_count + 1;
      let* body = Runtime.drain_request_body body in
      ignore body;
      let etag =
        Awskit_s3.Object.Etag.of_string_exn (Fmt.str "etag-%d" part_number)
      in
      let part = Awskit_s3.Multipart.Part.create_exn ~part_number ~etag () in
      Ok
        {
          Awskit_s3.Upload_part.part;
          checksum = empty_checksum;
          response = response 200;
        }

    let complete_upload conn ~bucket:_ ~key:_ ~upload_id:_ ?options:_ _ =
      conn.Runtime.complete_count <- conn.Runtime.complete_count + 1;
      Ok
        {
          Awskit_s3.Complete_multipart_upload.etag = None;
          version_id = None;
          checksum = empty_checksum;
          response = response 200;
        }

    let abort_upload _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ () =
      Ok (response 204)

    let list_parts _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ () = assert false

    module List_parts = struct
      let fold_pages _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ ?max_pages:_
          ~init:_ ~f:_ () =
        assert false

      let pages _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ ?max_pages:_ () =
        assert false

      let parts _ ~bucket:_ ~key:_ ~upload_id:_ ?options:_ ?max_pages:_ () =
        Ok []
    end
  end
end

module Transfer = Awskit_s3_eio__Transfer.Make (Runtime) (S3)

let remove_file path = try Sys.remove path with Sys_error _ -> ()

let write_file path body =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel body)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let path_of_native env path = Eio.Path.(Eio.Stdenv.fs env / path)

let test_upload_file_strategies env () =
  let small_path = Filename.temp_file "awskit-eio-upload-small" ".bin" in
  let multipart_path = Filename.temp_file "awskit-eio-upload-large" ".bin" in
  write_file small_path "small";
  write_file multipart_path (String.make Awskit_s3.Transfer.min_part_size 'm');
  Fun.protect
    ~finally:(fun () ->
      remove_file small_path;
      remove_file multipart_path)
    (fun () ->
      let small_conn = connection () in
      let small_options =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = 1024L;
        }
      in
      let small =
        Transfer.upload_file small_conn ~bucket:"bucket" ~key:"key"
          ~options:small_options
          ~path:(path_of_native env small_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "small strategy" true
        (Awskit_s3.Transfer.upload_strategy small = `Put);
      Alcotest.(check int) "small put count" 1 small_conn.Runtime.put_count;
      let multipart_conn = connection () in
      let multipart_options =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = Int64.of_int Awskit_s3.Transfer.min_part_size;
          part_size = Awskit_s3.Transfer.min_part_size;
        }
      in
      let multipart =
        Transfer.upload_file multipart_conn ~bucket:"bucket" ~key:"key"
          ~options:multipart_options
          ~path:(path_of_native env multipart_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "multipart strategy" true
        (Awskit_s3.Transfer.upload_strategy multipart = `Multipart);
      Alcotest.(check int)
        "multipart part count" 1 multipart_conn.Runtime.upload_part_count)

let test_download_file_ranges env () =
  let native_path = Filename.temp_file "awskit-eio-download" ".bin" in
  remove_file native_path;
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let conn = connection ~response_body:body () in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = Int64.of_int part_size;
          part_size;
          concurrency = 2;
        }
      in
      let result =
        Transfer.download_file conn ~bucket:"bucket" ~key:"key" ~options
          ~path:(path_of_native env native_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "strategy" true
        (Awskit_s3.Transfer.download_strategy result = `Ranged);
      Alcotest.(check string) "body" body (read_file native_path);
      Alcotest.(check (list string))
        "ranges"
        [
          Fmt.str "bytes=0-%d" (part_size - 1);
          Fmt.str "bytes=%d-%d" part_size (String.length body - 1);
        ]
        conn.Runtime.get_ranges)

let suite env =
  [
    ( "transfer",
      [
        Alcotest.test_case "upload strategies" `Quick
          (test_upload_file_strategies env);
        Alcotest.test_case "download ranges" `Quick
          (test_download_file_ranges env);
      ] );
  ]
