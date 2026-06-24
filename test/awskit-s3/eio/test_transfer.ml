let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let is_body_error error =
  let open Awskit.Error in
  match kind error with Body _ -> true | _ -> false

let is_validation_field field error =
  Awskit.Error.is_validation error
  && Awskit.Error.validation_field error = Some field

let string_contains haystack needle =
  let haystack_length = String.length haystack in
  let needle_length = String.length needle in
  let rec loop offset =
    offset + needle_length <= haystack_length
    && (String.sub haystack offset needle_length = needle || loop (offset + 1))
  in
  needle_length = 0 || loop 0

let check_multiple_error_text label error snippets =
  match Awskit.Error.kind error with
  | Multiple errors ->
      Alcotest.(check int)
        (label ^ " count") (List.length snippets) (List.length errors);
      let text = Fmt.str "%a" Awskit.Error.pp error in
      List.iter
        (fun snippet ->
          Alcotest.(check bool)
            (label ^ " includes " ^ snippet)
            true
            (string_contains text snippet))
        snippets
  | _ -> Alcotest.failf "expected Multiple error, got %a" Awskit.Error.pp error

let check_multiple_error_text_in_order label error snippets =
  match Awskit.Error.kind error with
  | Multiple errors ->
      Alcotest.(check int)
        (label ^ " count") (List.length snippets) (List.length errors);
      List.iter2
        (fun snippet error ->
          let text = Fmt.str "%a" Awskit.Error.pp error in
          Alcotest.(check bool)
            (label ^ " includes " ^ snippet)
            true
            (string_contains text snippet))
        snippets errors
  | _ -> Alcotest.failf "expected Multiple error, got %a" Awskit.Error.pp error

let check_validation_result label field = function
  | Error error when is_validation_field field error -> ()
  | Error error ->
      Alcotest.failf "%s: unexpected error: %a" label Awskit.Error.pp error
  | Ok _ -> Alcotest.failf "%s: expected validation error" label

let bucket = Awskit_s3.Bucket_name.of_string_exn "bucket"
let key = Awskit_s3.Object_key.of_string_exn "key"

let has_progress_event ~direction ~phase ?total ?part_number transferred
    (progress : Awskit_s3.Transfer.progress) =
  progress.direction = direction
  && progress.phase = phase
  && Int64.equal progress.transferred transferred
  && progress.total = total
  && progress.part_number = part_number

let find_progress_event ~direction ~phase ?total ?part_number transferred
    progress =
  List.find_opt
    (has_progress_event ~direction ~phase ?total ?part_number transferred)
    progress

module Runtime = struct
  type connection = {
    response_body : string;
    head_etag : Awskit_s3.Object.Etag.t option;
    head_version_id : Awskit_s3.Object.Version_id.t option;
    mutable uploaded_body : string option;
    mutable put_count : int;
    mutable get_count : int;
    mutable head_count : int;
    mutable multipart_create_count : int;
    mutable upload_part_count : int;
    mutable uploaded_part_numbers : int list;
    mutable fail_upload_part_number : int option;
    mutable complete_count : int;
    mutable abort_count : int;
    mutable listed_parts : Awskit_s3.Multipart.List_parts.part_info list;
    mutable list_parts_max_pages : int option;
    mutable completed_part_etags : string list;
    mutable fail_ranged_get : bool;
    mutable fail_complete_upload : bool;
    mutable fail_abort_upload : bool;
    mutable get_ranges : string list;
    mutable ranged_get_version_ids : string option list;
    mutable ranged_get_if_matches : string option list;
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

  let read_error_after_bytes = ref None
  let read_cancel_after_bytes = ref None

  let reset_read_fault () =
    read_error_after_bytes := None;
    read_cancel_after_bytes := None

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
    Awskit.Body.Request.descriptor_exn
      ~content_length:(Int64.of_int (String.length body))
      ~payload_hash:(Awskit.Body.Payload_hash.sha256_of_string body)
      ~replayable:true ()

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
      match !read_error_after_bytes with
      | Some limit when reader.offset >= limit ->
          Error (Awskit.Error.Producer.body "simulated response read failure")
      | _ -> (
          match !read_cancel_after_bytes with
          | Some limit when reader.offset >= limit ->
              raise (Eio.Cancel.Cancelled Exit)
          | _ ->
              let remaining = String.length reader.body - reader.offset in
              if remaining <= 0 then Ok 0
              else
                let copied = min len remaining in
                String.blit reader.body reader.offset bytes off copied;
                reader.offset <- reader.offset + copied;
                Ok copied)

  let with_response_body body ~consume = consume { body; offset = 0 }
  let discard_response_body _ = Ok ()

  module Request_body = struct
    type 'a io = 'a
    type t = request_body
    type writer = request_body_writer

    let empty = empty_request_body
    let of_string = string_request_body
    let of_bytes = bytes_request_body
    let of_stream = stream_request_body
    let descriptor = request_body_descriptor
    let content_length body = (request_body_descriptor body).content_length
    let write_string = write_request_body_string

    let write_bytes writer bytes =
      write_request_body_string writer (Bytes.to_string bytes)

    let write_subbytes writer bytes ~off ~len =
      write_request_body_string writer (Bytes.sub_string bytes off len)
  end

  module Response_body = struct
    type 'a io = 'a
    type t = response_body
    type reader = response_body_reader

    let read = read_response_body

    let next ?(chunk_size = 8192) reader =
      if chunk_size <= 0 then
        Error (Awskit.Error.Producer.body "chunk_size must be positive")
      else
        let bytes = Bytes.create chunk_size in
        match read_response_body reader bytes ~off:0 ~len:chunk_size with
        | Error _ as error -> error
        | Ok 0 -> Ok None
        | Ok n -> Ok (Some (Bytes.sub bytes 0 n))

    let with_reader = with_response_body
    let discard = discard_response_body
  end

  module IO = struct
    type 'a t = 'a

    let return value = value
    let bind value f = f value
  end

  module Transport = struct
    type 'a io = 'a
    type nonrec connection = connection
    type nonrec request_body = request_body
    type nonrec response_body = response_body

    let with_response _ _ ~body:_ ~consume:_ =
      Error (Awskit.Error.Producer.transport ~retryable:false "not implemented")
  end

  module Clock = struct
    type nonrec connection = connection

    let now _ = Ptime.epoch
  end

  module Sleeper = struct
    type 'a io = 'a
    type nonrec connection = connection

    let sleep _ _ = ()
  end

  module Random = struct
    type nonrec connection = connection

    let float _ ~upper_bound = upper_bound /. 2.
  end

  module Credentials = struct
    type 'a io = 'a
    type nonrec connection = connection

    let resolve _ =
      Ok
        (Awskit.Credentials.create_exn ~access_key_id:"AK"
           ~secret_access_key:"SK" ())
  end

  module Endpoint = struct
    type nonrec connection = connection

    let region _ = Awskit.Region.of_string_exn "us-east-1"
    let endpoint _ = None
  end

  module Retry = struct
    type nonrec connection = connection

    let policy _ = Awskit.Retry.default
  end

  module Timeout = struct
    type nonrec connection = connection

    let policy _ = Awskit.Timeout.default
  end

  module S3_endpoint = struct
    type nonrec connection = connection

    let s3_endpoint_config _ = Awskit_s3.default_endpoint_config
  end
end

let connection ?(response_body = "") ?head_etag ?head_version_id () =
  {
    Runtime.response_body;
    head_etag;
    head_version_id;
    uploaded_body = None;
    put_count = 0;
    get_count = 0;
    head_count = 0;
    multipart_create_count = 0;
    upload_part_count = 0;
    uploaded_part_numbers = [];
    fail_upload_part_number = None;
    complete_count = 0;
    abort_count = 0;
    listed_parts = [];
    list_parts_max_pages = None;
    completed_part_etags = [];
    fail_ranged_get = false;
    fail_complete_upload = false;
    fail_abort_upload = false;
    get_ranges = [];
    ranged_get_version_ids = [];
    ranged_get_if_matches = [];
  }

let recorded_uploaded_part_numbers conn =
  List.rev conn.Runtime.uploaded_part_numbers

let recorded_get_ranges conn = List.rev conn.Runtime.get_ranges

let recorded_ranged_get_version_ids conn =
  List.rev conn.Runtime.ranged_get_version_ids

let recorded_ranged_get_if_matches conn =
  List.rev conn.Runtime.ranged_get_if_matches

let response status = Awskit.Response.create_exn ~status ()

let empty_checksum : Awskit_s3.Object.Checksum.response =
  { values = []; checksum_type = None }

let listed_part ~part_number ~size ~etag =
  {
    Awskit_s3.Multipart.List_parts.part_number =
      Awskit_s3.Multipart.Part_number.of_int_exn part_number;
    etag = Some (Awskit_s3.Object.Etag.of_string_exn etag);
    size = Some size;
    last_modified = None;
    checksum = empty_checksum;
  }

let put_result () : Awskit_s3.Object.Put.result =
  {
    etag = None;
    version_id = None;
    checksum = empty_checksum;
    response = response 200;
  }

let get_info ?etag ?version_id content_length : Awskit_s3.Object.Get.info =
  {
    etag;
    content_type = None;
    content_length;
    content_range = None;
    last_modified = None;
    metadata = Awskit_s3.Metadata.empty;
    storage_class = None;
    version_id;
    checksum = empty_checksum;
    server_side_encryption = None;
    response = response 200;
  }

let get_result ?etag ?version_id content_length value :
    _ Awskit_s3.Object.Get.result =
  let info = get_info ?etag ?version_id content_length in
  {
    Awskit_s3.Object.Get.value;
    etag = info.etag;
    content_type = info.content_type;
    content_length = info.content_length;
    content_range = info.content_range;
    last_modified = info.last_modified;
    metadata = info.metadata;
    storage_class = info.storage_class;
    version_id = info.version_id;
    checksum = info.checksum;
    server_side_encryption = info.server_side_encryption;
    response = info.response;
  }

let etag_condition_to_string = function
  | Awskit_s3.Object.Etag_condition.Any -> "*"
  | Awskit_s3.Object.Etag_condition.Etag etag ->
      Awskit_s3.Object.Etag.to_string etag

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
          Option.bind options (fun (options : Awskit_s3.Object.Get.options) ->
              options.range)
        with
        | None -> conn.Runtime.response_body
        | Some range ->
            let header = Awskit_s3.Range.to_header range in
            conn.Runtime.get_ranges <- header :: conn.Runtime.get_ranges;
            conn.Runtime.ranged_get_version_ids <-
              Option.bind options
                (fun (options : Awskit_s3.Object.Get.options) ->
                  Option.map Awskit_s3.Object.Version_id.to_string
                    options.version_id)
              :: conn.Runtime.ranged_get_version_ids;
            conn.Runtime.ranged_get_if_matches <-
              Option.bind options
                (fun (options : Awskit_s3.Object.Get.options) ->
                  Option.map etag_condition_to_string
                    options.preconditions.if_match)
              :: conn.Runtime.ranged_get_if_matches;
            parse_range_header header conn.Runtime.response_body
      in
      if
        conn.Runtime.fail_ranged_get
        && Option.is_some (Option.bind options (fun options -> options.range))
      then Error (Awskit.Error.Producer.body "simulated ranged get failure")
      else
        let consumed = consume { Runtime.body; offset = 0 } in
        Result.map
          (fun value ->
            get_result
              (Some (Int64.of_int (String.length conn.Runtime.response_body)))
              value)
          consumed

    let head conn ~bucket:_ ~key:_ ?options:_ () =
      conn.Runtime.head_count <- conn.Runtime.head_count + 1;
      Ok
        (get_info ?etag:conn.Runtime.head_etag
           ?version_id:conn.Runtime.head_version_id
           (Some (Int64.of_int (String.length conn.Runtime.response_body))))

    let exists _ ~bucket:_ ~key:_ ?options:_ () = assert false
    let delete _ ~bucket:_ ~key:_ ?options:_ () = assert false
    let delete_objects _ ~bucket:_ ~objects:_ ?options:_ () = assert false

    let copy _ ~source_bucket:_ ~source_key:_ ~destination_bucket:_
        ~destination_key:_ ?options:_ () =
      assert false

    let list_versions _ ~bucket:_ ?options:_ () = assert false
    let list _ ~bucket:_ ?options:_ () = assert false

    module List = struct
      type 'acc fold_step = Continue of 'acc | Stop of 'acc

      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        assert false

      let fold_pages_until _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        assert false

      let pages _ ~bucket:_ ?options:_ ~max_pages:_ () = assert false
      let objects _ ~bucket:_ ?options:_ ~max_pages:_ () = assert false
      let keys _ ~bucket:_ ?options:_ ~max_pages:_ () = assert false
    end

    module Versions = struct
      type 'acc fold_step = Continue of 'acc | Stop of 'acc

      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        assert false

      let fold_pages_until _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        assert false

      let pages _ ~bucket:_ ?options:_ ~max_pages:_ () = assert false
      let object_versions _ ~bucket:_ ?options:_ ~max_pages:_ () = assert false
      let delete_markers _ ~bucket:_ ?options:_ ~max_pages:_ () = assert false
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
      let upload = Awskit_s3.Multipart.Upload.created ~bucket ~key ~upload_id in
      Ok { Awskit_s3.Multipart.Create.upload; response = response 200 }

    let upload_part conn ~upload:_ ~part_number ~body ?options:_ () =
      conn.Runtime.upload_part_count <- conn.Runtime.upload_part_count + 1;
      let part_number_int =
        Awskit_s3.Multipart.Part_number.to_int part_number
      in
      conn.Runtime.uploaded_part_numbers <-
        part_number_int :: conn.Runtime.uploaded_part_numbers;
      match conn.Runtime.fail_upload_part_number with
      | Some failed when failed = part_number_int ->
          Error (Awskit.Error.Producer.body "simulated upload-part failure")
      | _ ->
          let* body = Runtime.drain_request_body body in
          let etag =
            Awskit_s3.Object.Etag.of_string_exn
              (Fmt.str "etag-%d" part_number_int)
          in
          let size = Int64.of_int (String.length body) in
          let part =
            Awskit_s3.Multipart.Part.create_exn ~part_number ~etag ~size ()
          in
          Ok
            {
              Awskit_s3.Multipart.Upload_part.part;
              checksum = empty_checksum;
              response = response 200;
            }

    let complete_upload conn ~upload:_ ?options:_ ~parts () =
      conn.Runtime.complete_count <- conn.Runtime.complete_count + 1;
      conn.Runtime.completed_part_etags <-
        List.map
          (fun (part : Awskit_s3.Multipart.Part.t) ->
            part
            |> Awskit_s3.Multipart.Part.etag
            |> Awskit_s3.Object.Etag.to_string)
          parts;
      if conn.Runtime.fail_complete_upload then
        Error (Awskit.Error.Producer.body "simulated complete failure")
      else
        Ok
          {
            Awskit_s3.Multipart.Complete.etag = None;
            version_id = None;
            checksum = empty_checksum;
            response = response 200;
          }

    let abort_upload conn ~upload:_ ?options:_ () =
      conn.Runtime.abort_count <- conn.Runtime.abort_count + 1;
      if conn.Runtime.fail_abort_upload then
        Error (Awskit.Error.Producer.body "simulated abort failure")
      else Ok { Awskit_s3.Multipart.Abort.response = response 204 }

    let list_parts _ ~upload:_ ?options:_ () = assert false

    module List_parts = struct
      let fold_pages _ ~upload:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        assert false

      let pages _ ~upload:_ ?options:_ ?max_pages:_ () = assert false

      let parts conn ~upload:_ ?options:_ ?max_pages () =
        conn.Runtime.list_parts_max_pages <- max_pages;
        Ok conn.Runtime.listed_parts
    end
  end
end

module Core = Awskit_s3.Make (Runtime)
module Body_reader = Transfer_under_test.Make_body_reader (Runtime) (Core)
module Body = Body_reader.Body
module Reader = Body_reader.Reader
module Transfer = Transfer_under_test.Make (Runtime) (S3) (Body) (Reader)

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

let with_umask mask f =
  let previous = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask previous)) f

let response_reader body = { Runtime.body; offset = 0 }

let download_temp_paths path =
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  let prefix = "." ^ base ^ ".awskit-download." in
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name ->
      let prefix_length = String.length prefix in
      String.length name >= prefix_length
      && String.sub name 0 prefix_length = prefix)
  |> List.map (Filename.concat dir)

let remove_download_temps path =
  List.iter remove_file (download_temp_paths path)

let check_body_descriptor label ~content_length ~replayable body =
  let body_descriptor = Runtime.Request_body.descriptor body in
  Alcotest.(check (option int64))
    (label ^ " content length")
    (Some content_length) body_descriptor.content_length;
  Alcotest.(check bool)
    (label ^ " replayable") replayable body_descriptor.replayable

let body_or_fail label = function
  | Ok body -> body
  | Error error -> Alcotest.failf "%s: %a" label Awskit_s3.Error.pp error

let checksum_value : Awskit_s3.Object.Checksum.value =
  Awskit_s3.Object.Checksum.value_exn
    ~algorithm:Awskit_s3.Object.Checksum.Algorithm.Sha256 ~value:"checksum"

let test_transfer_option_builders_reject_invalid_options _env () =
  check_validation_result "upload concurrency" "concurrency"
    (Awskit_s3.Transfer.upload_options ~concurrency:0 ());
  let upload_part_options =
    Awskit_s3.Multipart.Upload_part.options_exn ~checksum:checksum_value ()
  in
  check_validation_result "upload-part checksum" "upload_part_options.checksum"
    (Awskit_s3.Transfer.upload_options ~upload_part_options ());
  check_validation_result "download part size" "part_size"
    (Awskit_s3.Transfer.download_options ~part_size:0 ());
  let get_options =
    Awskit_s3.Object.Get.options_exn
      ~range:(Awskit_s3.Range.bytes_exn ~start:0L ~finish:1L)
      ()
  in
  check_validation_result "download range" "get_options.range"
    (Awskit_s3.Transfer.download_options ~get_options ())

let test_body_of_flow_streams_flow _env () =
  let conn = connection () in
  let progress = ref [] in
  let payload = "flow upload body" in
  let body =
    Body.of_flow
      ~content_length:(Int64.of_int (String.length payload))
      ~on_progress:(fun transferred -> progress := transferred :: !progress)
      (Eio.Flow.string_source payload)
    |> body_or_fail "flow body"
  in
  check_body_descriptor "flow body"
    ~content_length:(Int64.of_int (String.length payload))
    ~replayable:false body;
  match S3.Object.put conn ~bucket ~key ~body () with
  | Error error -> Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
  | Ok _ ->
      Alcotest.(check (option string))
        "uploaded body" (Some payload) conn.Runtime.uploaded_body;
      Alcotest.(check (list int64))
        "progress"
        [ Int64.of_int (String.length payload) ]
        (List.rev !progress)

let test_body_of_path_streams_file_body env () =
  let native_path = Filename.temp_file "awskit-eio-upload-body" ".bin" in
  let payload = "first line\nsecond line\n" in
  write_file native_path payload;
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      let progress = ref [] in
      match
        Body.of_path
          ~on_progress:(fun transferred -> progress := transferred :: !progress)
          (path_of_native env native_path)
      with
      | Error error -> Alcotest.failf "body failed: %a" Awskit_s3.Error.pp error
      | Ok body -> (
          check_body_descriptor "path body"
            ~content_length:(Int64.of_int (String.length payload))
            ~replayable:true body;
          match S3.Object.put conn ~bucket ~key ~body () with
          | Error error ->
              Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
          | Ok _ ->
              Alcotest.(check (option string))
                "uploaded body" (Some payload) conn.Runtime.uploaded_body;
              Alcotest.(check (list int64))
                "progress"
                [ Int64.of_int (String.length payload) ]
                (List.rev !progress)))

let test_body_of_path_rejects_non_regular_file env () =
  let native_path = Filename.temp_file "awskit-eio-upload-directory" "" in
  remove_file native_path;
  Unix.mkdir native_path 0o700;
  Fun.protect
    ~finally:(fun () -> Unix.rmdir native_path)
    (fun () ->
      match Body.of_path (path_of_native env native_path) with
      | Error error when is_validation_field "path" error -> ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected path validation")

let test_reader_to_flow_writes_response_body _env () =
  let buffer = Buffer.create 32 in
  let progress = ref [] in
  let payload = "flow download body" in
  match
    Reader.to_flow
      ~on_progress:(fun transferred -> progress := transferred :: !progress)
      (Eio.Flow.buffer_sink buffer)
      (response_reader payload)
  with
  | Error error -> Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
  | Ok () ->
      Alcotest.(check string) "body" payload (Buffer.contents buffer);
      Alcotest.(check (list int64))
        "progress"
        [ Int64.of_int (String.length payload) ]
        (List.rev !progress)

let test_reader_to_path_creates_private_file env () =
  let native_path = Filename.temp_file "awskit-eio-download-perm" ".bin" in
  let payload = "secret downloaded file body" in
  write_file native_path "old body";
  Unix.chmod native_path 0o666;
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      with_umask 0 (fun () ->
          match
            Reader.to_path
              (path_of_native env native_path)
              (response_reader payload)
          with
          | Error error ->
              Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
          | Ok () -> ());
      Alcotest.(check string) "body" payload (read_file native_path);
      let stat = Unix.stat native_path in
      Alcotest.(check int) "mode" 0o600 (stat.Unix.st_perm land 0o777))

let test_reader_to_path_failure_preserves_target env () =
  let native_path = Filename.temp_file "awskit-eio-download-failure" ".bin" in
  let old_body = "old body" in
  let new_body = "new body" in
  write_file native_path old_body;
  Runtime.read_error_after_bytes := Some 0;
  Fun.protect
    ~finally:(fun () ->
      Runtime.reset_read_fault ();
      remove_download_temps native_path;
      remove_file native_path)
    (fun () ->
      match
        Reader.to_path
          (path_of_native env native_path)
          (response_reader new_body)
      with
      | Ok () -> Alcotest.fail "download succeeded despite read failure"
      | Error error when is_body_error error ->
          Alcotest.(check string)
            "preserved body" old_body (read_file native_path);
          Alcotest.(check int)
            "temp files removed" 0
            (List.length (download_temp_paths native_path))
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error)

let test_temp_download_cleanup_combines_failures_in_order env () =
  let native_path =
    Filename.temp_file "awskit-eio-download-close-failure" ".bin"
  in
  let primary_error = Awskit.Error.Producer.body "primary download failure" in
  let close_error =
    Awskit.Error.Producer.body "failed to close temporary download"
  in
  Fun.protect
    ~finally:(fun () ->
      remove_download_temps native_path;
      remove_file native_path)
    (fun () ->
      match
        Transfer_under_test.cleanup_temp_download_with_failures
          (path_of_native env native_path)
          [ primary_error; close_error ]
      with
      | Ok () -> Alcotest.fail "download succeeded despite primary error"
      | Error error ->
          check_multiple_error_text_in_order "close cleanup error" error
            [ "primary download failure"; "failed to close temporary download" ])

let test_reader_to_path_cancellation_preserves_target env () =
  let native_path = Filename.temp_file "awskit-eio-download-cancel" ".bin" in
  let old_body = "old body" in
  let new_body = "new body" in
  write_file native_path old_body;
  Runtime.read_cancel_after_bytes := Some 0;
  Fun.protect
    ~finally:(fun () ->
      Runtime.reset_read_fault ();
      remove_download_temps native_path;
      remove_file native_path)
    (fun () ->
      match
        Reader.to_path
          (path_of_native env native_path)
          (response_reader new_body)
      with
      | exception Eio.Cancel.Cancelled _ ->
          Alcotest.(check string)
            "preserved body" old_body (read_file native_path);
          Alcotest.(check int)
            "temp files removed" 0
            (List.length (download_temp_paths native_path))
      | Ok () -> Alcotest.fail "download succeeded despite cancellation"
      | Error error ->
          Alcotest.failf "cancellation became error: %a" Awskit_s3.Error.pp
            error)

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
      let small_progress = ref [] in
      let small_options =
        Awskit_s3.Transfer.upload_options_exn ~multipart_threshold:1024L ()
      in
      let small =
        Transfer.upload_file small_conn ~bucket ~key ~options:small_options
          ~on_progress:(fun progress ->
            small_progress := progress :: !small_progress)
          ~path:(path_of_native env small_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "small strategy" true
        (Awskit_s3.Transfer.upload_strategy small = `Put);
      Alcotest.(check int64)
        "small bytes" 5L
        (Awskit_s3.Transfer.upload_bytes_transferred small);
      Alcotest.(check int) "small put count" 1 small_conn.Runtime.put_count;
      Alcotest.(check bool)
        "small progress" true
        (Option.is_some
           (find_progress_event ~direction:Awskit_s3.Transfer.Upload
              ~phase:Awskit_s3.Transfer.Single_request ~total:5L 5L
              !small_progress));
      let multipart_conn = connection () in
      let multipart_progress = ref [] in
      let multipart_options =
        Awskit_s3.Transfer.upload_options_exn
          ~multipart_threshold:(Int64.of_int Awskit_s3.Transfer.min_part_size)
          ~part_size:Awskit_s3.Transfer.min_part_size ()
      in
      let multipart =
        Transfer.upload_file multipart_conn ~bucket ~key
          ~options:multipart_options
          ~on_progress:(fun progress ->
            multipart_progress := progress :: !multipart_progress)
          ~path:(path_of_native env multipart_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "multipart strategy" true
        (Awskit_s3.Transfer.upload_strategy multipart = `Multipart);
      Alcotest.(check int64)
        "multipart bytes"
        (Int64.of_int Awskit_s3.Transfer.min_part_size)
        (Awskit_s3.Transfer.upload_bytes_transferred multipart);
      Alcotest.(check int)
        "multipart part count" 1 multipart_conn.Runtime.upload_part_count;
      let part_number = Awskit_s3.Multipart.Part_number.of_int_exn 1 in
      Alcotest.(check bool)
        "multipart progress" true
        (Option.is_some
           (find_progress_event ~direction:Awskit_s3.Transfer.Upload
              ~phase:Awskit_s3.Transfer.Part
              ~total:(Int64.of_int Awskit_s3.Transfer.min_part_size)
              ~part_number
              (Int64.of_int Awskit_s3.Transfer.min_part_size)
              !multipart_progress)))

let test_upload_file_progress_exception_propagates env () =
  let exception Progress_failed in
  let native_path =
    Filename.temp_file "awskit-eio-upload-progress-exn" ".bin"
  in
  write_file native_path "small";
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      let options =
        Awskit_s3.Transfer.upload_options_exn ~multipart_threshold:1024L ()
      in
      match
        Transfer.upload_file conn ~bucket ~key ~options
          ~on_progress:(fun _progress -> raise Progress_failed)
          ~path:(path_of_native env native_path)
          ()
      with
      | exception exn when exn == Progress_failed -> ()
      | exception exn ->
          Alcotest.failf "unexpected raised exception: %s"
            (Printexc.to_string exn)
      | Error error ->
          Alcotest.failf "callback returned error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "upload succeeded despite callback exception")

let test_upload_empty_file_uses_put_at_zero_threshold env () =
  let native_path = Filename.temp_file "awskit-eio-upload-empty-put" ".bin" in
  write_file native_path "";
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      let options =
        Awskit_s3.Transfer.upload_options_exn ~multipart_threshold:0L ()
      in
      match
        Transfer.upload_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error ->
          Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.upload_strategy result = `Put);
          Alcotest.(check int64)
            "bytes transferred" 0L
            (Awskit_s3.Transfer.upload_bytes_transferred result);
          Alcotest.(check int) "put count" 1 conn.Runtime.put_count;
          Alcotest.(check int)
            "multipart create count" 0 conn.Runtime.multipart_create_count;
          Alcotest.(check (option string))
            "uploaded body" (Some "") conn.Runtime.uploaded_body)

let test_multipart_upload_file env () =
  let native_path = Filename.temp_file "awskit-eio-upload-multipart" ".bin" in
  write_file native_path (String.make Awskit_s3.Transfer.min_part_size 'm');
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      let options =
        Awskit_s3.Transfer.upload_options_exn
          ~part_size:Awskit_s3.Transfer.min_part_size ()
      in
      match
        Transfer.multipart_upload_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error ->
          Alcotest.failf "multipart upload failed: %a" Awskit_s3.Error.pp error
      | Ok _ ->
          Alcotest.(check int)
            "multipart create count" 1 conn.Runtime.multipart_create_count;
          Alcotest.(check int)
            "upload part count" 1 conn.Runtime.upload_part_count;
          Alcotest.(check int) "complete count" 1 conn.Runtime.complete_count)

let test_multipart_upload_reports_abort_failure env () =
  let native_path =
    Filename.temp_file "awskit-eio-upload-abort-failure" ".bin"
  in
  write_file native_path (String.make Awskit_s3.Transfer.min_part_size 'a');
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      conn.Runtime.fail_complete_upload <- true;
      conn.Runtime.fail_abort_upload <- true;
      let options =
        Awskit_s3.Transfer.upload_options_exn
          ~part_size:Awskit_s3.Transfer.min_part_size ()
      in
      match
        Transfer.multipart_upload_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Ok _ -> Alcotest.fail "multipart upload succeeded despite failures"
      | Error error ->
          Alcotest.(check int) "complete count" 1 conn.Runtime.complete_count;
          Alcotest.(check int) "abort count" 1 conn.Runtime.abort_count;
          check_multiple_error_text "abort error" error
            [ "simulated complete failure"; "simulated abort failure" ])

let test_multipart_upload_aborts_on_progress_exception env () =
  let exception Progress_failed in
  let native_path =
    Filename.temp_file "awskit-eio-upload-multipart-progress-exn" ".bin"
  in
  write_file native_path (String.make Awskit_s3.Transfer.min_part_size 'p');
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      let options =
        Awskit_s3.Transfer.upload_options_exn
          ~part_size:Awskit_s3.Transfer.min_part_size ()
      in
      match
        Transfer.multipart_upload_file conn ~bucket ~key ~options
          ~on_progress:(fun _transferred -> raise Progress_failed)
          ~path:(path_of_native env native_path)
          ()
      with
      | exception exn when exn == Progress_failed ->
          Alcotest.(check int) "abort count" 1 conn.Runtime.abort_count;
          Alcotest.(check int) "complete count" 0 conn.Runtime.complete_count
      | exception exn ->
          Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)
      | Error error ->
          Alcotest.failf "callback returned error: %a" Awskit_s3.Error.pp error
      | Ok _ ->
          Alcotest.fail "multipart upload succeeded despite callback exception")

let test_multipart_upload_aborts_on_progress_cancellation env () =
  let native_path =
    Filename.temp_file "awskit-eio-upload-multipart-progress-cancel" ".bin"
  in
  write_file native_path (String.make Awskit_s3.Transfer.min_part_size 'c');
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      let options =
        Awskit_s3.Transfer.upload_options_exn
          ~part_size:Awskit_s3.Transfer.min_part_size ()
      in
      match
        Transfer.multipart_upload_file conn ~bucket ~key ~options
          ~on_progress:(fun _transferred -> raise (Eio.Cancel.Cancelled Exit))
          ~path:(path_of_native env native_path)
          ()
      with
      | exception Eio.Cancel.Cancelled _ ->
          Alcotest.(check int) "abort count" 1 conn.Runtime.abort_count;
          Alcotest.(check int) "complete count" 0 conn.Runtime.complete_count
      | exception exn ->
          Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)
      | Error error ->
          Alcotest.failf "cancellation returned error: %a" Awskit_s3.Error.pp
            error
      | Ok _ -> Alcotest.fail "multipart upload succeeded despite cancellation")

let test_resume_multipart_upload_file env () =
  let native_path = Filename.temp_file "awskit-eio-resume-multipart" ".bin" in
  write_file native_path
    (String.make (Awskit_s3.Transfer.min_part_size * 2) 'r');
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      conn.Runtime.listed_parts <-
        [
          listed_part ~part_number:1
            ~size:(Int64.of_int Awskit_s3.Transfer.min_part_size)
            ~etag:"stale-etag-1";
        ];
      let upload_id = Awskit_s3.Multipart.Upload_id.of_string_exn "upload-1" in
      let upload = Awskit_s3.Multipart.Upload.resume ~bucket ~key ~upload_id in
      let options =
        Awskit_s3.Transfer.upload_options_exn
          ~part_size:Awskit_s3.Transfer.min_part_size ()
      in
      match
        Transfer.resume_multipart_upload_file conn ~upload ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error ->
          Alcotest.failf "resume failed: %a" Awskit_s3.Error.pp error
      | Ok _ ->
          Alcotest.(check (option int))
            "list max pages" (Some 1) conn.Runtime.list_parts_max_pages;
          Alcotest.(check (list int))
            "uploaded part numbers" [ 1; 2 ]
            (recorded_uploaded_part_numbers conn);
          Alcotest.(check int)
            "upload part count" 2 conn.Runtime.upload_part_count;
          Alcotest.(check int) "complete count" 1 conn.Runtime.complete_count;
          Alcotest.(check (list string))
            "completed part etags" [ "etag-1"; "etag-2" ]
            conn.Runtime.completed_part_etags)

let test_resume_multipart_upload_ignores_mismatched_listed_part env () =
  let native_path =
    Filename.temp_file "awskit-eio-resume-multipart-mismatch" ".bin"
  in
  write_file native_path
    (String.make (Awskit_s3.Transfer.min_part_size * 2) 'r');
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      conn.Runtime.listed_parts <-
        [ listed_part ~part_number:1 ~size:1L ~etag:"stale-etag-1" ];
      let upload_id = Awskit_s3.Multipart.Upload_id.of_string_exn "upload-1" in
      let upload = Awskit_s3.Multipart.Upload.resume ~bucket ~key ~upload_id in
      let options =
        Awskit_s3.Transfer.upload_options_exn
          ~part_size:Awskit_s3.Transfer.min_part_size ()
      in
      match
        Transfer.resume_multipart_upload_file conn ~upload ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error ->
          Alcotest.failf "resume failed: %a" Awskit_s3.Error.pp error
      | Ok _ ->
          Alcotest.(check (list int))
            "uploaded part numbers" [ 1; 2 ]
            (recorded_uploaded_part_numbers conn);
          Alcotest.(check int)
            "upload part count" 2 conn.Runtime.upload_part_count;
          Alcotest.(check int) "complete count" 1 conn.Runtime.complete_count;
          Alcotest.(check (list string))
            "completed part etags" [ "etag-1"; "etag-2" ]
            conn.Runtime.completed_part_etags)

let test_multipart_upload_finishes_error_batch_before_stopping env () =
  let native_path =
    Filename.temp_file "awskit-eio-upload-multipart-batch-error" ".bin"
  in
  write_file native_path
    (String.make (Awskit_s3.Transfer.min_part_size * 3) 'b');
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection () in
      conn.Runtime.fail_upload_part_number <- Some 1;
      let options =
        Awskit_s3.Transfer.upload_options_exn
          ~part_size:Awskit_s3.Transfer.min_part_size ~concurrency:2 ()
      in
      match
        Transfer.multipart_upload_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error when is_body_error error ->
          Alcotest.(check (list int))
            "attempted current batch only" [ 1; 2 ]
            (recorded_uploaded_part_numbers conn);
          Alcotest.(check int) "complete count" 0 conn.Runtime.complete_count;
          Alcotest.(check int) "abort count" 1 conn.Runtime.abort_count
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "multipart upload succeeded despite part failure")

let test_download_file_uses_get_below_threshold env () =
  let native_path = Filename.temp_file "awskit-eio-download-get" ".bin" in
  remove_file native_path;
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let payload = "small download" in
      let conn = connection ~response_body:payload () in
      let progress = ref [] in
      let options =
        Awskit_s3.Transfer.download_options_exn ~multipart_threshold:1024L ()
      in
      match
        Transfer.download_file conn ~bucket ~key ~options
          ~on_progress:(fun event -> progress := event :: !progress)
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error ->
          Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.download_strategy result = `Get);
          Alcotest.(check int64)
            "bytes transferred"
            (Int64.of_int (String.length payload))
            (Awskit_s3.Transfer.download_bytes_transferred result);
          Alcotest.(check string) "body" payload (read_file native_path);
          Alcotest.(check int) "head count" 1 conn.Runtime.head_count;
          Alcotest.(check int) "get count" 1 conn.Runtime.get_count;
          Alcotest.(check int)
            "range count" 0
            (List.length (recorded_get_ranges conn));
          Alcotest.(check bool)
            "download progress" true
            (Option.is_some
               (find_progress_event ~direction:Awskit_s3.Transfer.Download
                  ~phase:Awskit_s3.Transfer.Single_request
                  ~total:(Int64.of_int (String.length payload))
                  (Int64.of_int (String.length payload))
                  !progress)))

let test_download_file_ranges env () =
  let native_path = Filename.temp_file "awskit-eio-download" ".bin" in
  remove_file native_path;
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let conn = connection ~response_body:body () in
      let progress = ref [] in
      let options =
        Awskit_s3.Transfer.download_options_exn
          ~multipart_threshold:(Int64.of_int part_size) ~part_size
          ~concurrency:2 ()
      in
      let result =
        Transfer.download_file conn ~bucket ~key ~options
          ~on_progress:(fun event -> progress := event :: !progress)
          ~path:(path_of_native env native_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "strategy" true
        (Awskit_s3.Transfer.download_strategy result = `Ranged);
      Alcotest.(check int64)
        "bytes transferred"
        (Int64.of_int (String.length body))
        (Awskit_s3.Transfer.download_bytes_transferred result);
      Alcotest.(check string) "body" body (read_file native_path);
      Alcotest.(check (list string))
        "ranges"
        [
          Fmt.str "bytes=0-%d" (part_size - 1);
          Fmt.str "bytes=%d-%d" part_size (String.length body - 1);
        ]
        (recorded_get_ranges conn);
      Alcotest.(check bool)
        "ranged progress" true
        (Option.is_some
           (find_progress_event ~direction:Awskit_s3.Transfer.Download
              ~phase:Awskit_s3.Transfer.Ranged_get
              ~total:(Int64.of_int (String.length body))
              (Int64.of_int (String.length body))
              !progress)))

let test_download_file_finishes_error_batch_before_stopping env () =
  let native_path =
    Filename.temp_file "awskit-eio-download-ranged-batch-error" ".bin"
  in
  remove_file native_path;
  Fun.protect
    ~finally:(fun () ->
      remove_download_temps native_path;
      remove_file native_path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let payload = String.make (part_size * 3) 'd' in
      let conn = connection ~response_body:payload () in
      conn.Runtime.fail_ranged_get <- true;
      let options =
        Awskit_s3.Transfer.download_options_exn
          ~multipart_threshold:(Int64.of_int part_size) ~part_size
          ~concurrency:2 ()
      in
      match
        Transfer.download_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error when is_body_error error ->
          Alcotest.(check int)
            "attempted current batch only" 2
            (List.length (recorded_get_ranges conn))
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "download succeeded despite ranged failure")

let test_download_file_ranged_progress_exception_propagates env () =
  let exception Progress_failed in
  let native_path =
    Filename.temp_file "awskit-eio-download-ranged-progress-exn" ".bin"
  in
  remove_file native_path;
  Fun.protect
    ~finally:(fun () ->
      remove_download_temps native_path;
      remove_file native_path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let conn = connection ~response_body:body () in
      let options =
        Awskit_s3.Transfer.download_options_exn
          ~multipart_threshold:(Int64.of_int part_size) ~part_size
          ~concurrency:2 ()
      in
      match
        Transfer.download_file conn ~bucket ~key ~options
          ~on_progress:(fun _event -> raise Progress_failed)
          ~path:(path_of_native env native_path)
          ()
      with
      | exception exn when exn == Progress_failed ->
          Alcotest.(check int)
            "temp files removed" 0
            (List.length (download_temp_paths native_path))
      | exception exn ->
          Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)
      | Error error ->
          Alcotest.failf "callback returned error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "download succeeded despite callback exception")

let test_download_file_ranges_use_head_version_id env () =
  let native_path = Filename.temp_file "awskit-eio-download-version" ".bin" in
  remove_file native_path;
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let version_id = Awskit_s3.Object.Version_id.of_string_exn "version-1" in
      let conn =
        connection ~response_body:body ~head_version_id:version_id ()
      in
      let options =
        Awskit_s3.Transfer.download_options_exn
          ~multipart_threshold:(Int64.of_int part_size) ~part_size
          ~concurrency:2 ()
      in
      let result =
        Transfer.download_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "strategy" true
        (Awskit_s3.Transfer.download_strategy result = `Ranged);
      Alcotest.(check (list (option string)))
        "ranged version ids"
        [ Some "version-1"; Some "version-1" ]
        (recorded_ranged_get_version_ids conn))

let test_download_file_ranges_use_head_etag env () =
  let native_path = Filename.temp_file "awskit-eio-download-etag" ".bin" in
  remove_file native_path;
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let etag = Awskit_s3.Object.Etag.of_string_exn "\"head-etag\"" in
      let conn = connection ~response_body:body ~head_etag:etag () in
      let options =
        Awskit_s3.Transfer.download_options_exn
          ~multipart_threshold:(Int64.of_int part_size) ~part_size
          ~concurrency:2 ()
      in
      let result =
        Transfer.download_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
        |> Result.get_ok
      in
      Alcotest.(check bool)
        "strategy" true
        (Awskit_s3.Transfer.download_strategy result = `Ranged);
      Alcotest.(check (list (option string)))
        "ranged if-matches"
        [ Some "\"head-etag\""; Some "\"head-etag\"" ]
        (recorded_ranged_get_if_matches conn))

let test_download_file_rejects_existing_target_without_overwrite env () =
  let native_path =
    Filename.temp_file "awskit-eio-download-no-overwrite" ".bin"
  in
  write_file native_path "existing";
  Fun.protect
    ~finally:(fun () -> remove_file native_path)
    (fun () ->
      let conn = connection ~response_body:"new" () in
      let options =
        Awskit_s3.Transfer.download_options_exn
          ~overwrite:Awskit_s3.Transfer.Error_if_exists ()
      in
      match
        Transfer.download_file conn ~bucket ~key ~options
          ~path:(path_of_native env native_path)
          ()
      with
      | Error error when is_validation_field "path" error ->
          Alcotest.(check string)
            "preserved target" "existing" (read_file native_path);
          Alcotest.(check int) "head count" 0 conn.Runtime.head_count;
          Alcotest.(check int) "get count" 0 conn.Runtime.get_count
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected overwrite validation")

let suite env =
  [
    ( "transfer",
      [
        Alcotest.test_case "body streams flow" `Quick
          (test_body_of_flow_streams_flow env);
        Alcotest.test_case "body streams path" `Quick
          (test_body_of_path_streams_file_body env);
        Alcotest.test_case "body path rejects non-regular file" `Quick
          (test_body_of_path_rejects_non_regular_file env);
        Alcotest.test_case "reader writes flow" `Quick
          (test_reader_to_flow_writes_response_body env);
        Alcotest.test_case "reader path creates private file" `Quick
          (test_reader_to_path_creates_private_file env);
        Alcotest.test_case "reader path failure preserves target" `Quick
          (test_reader_to_path_failure_preserves_target env);
        Alcotest.test_case "download cleanup combines failures in order" `Quick
          (test_temp_download_cleanup_combines_failures_in_order env);
        Alcotest.test_case "reader path cancellation preserves target" `Quick
          (test_reader_to_path_cancellation_preserves_target env);
        Alcotest.test_case "option builders reject invalid options" `Quick
          (test_transfer_option_builders_reject_invalid_options env);
        Alcotest.test_case "upload strategies" `Quick
          (test_upload_file_strategies env);
        Alcotest.test_case "upload progress exception propagates" `Quick
          (test_upload_file_progress_exception_propagates env);
        Alcotest.test_case "upload empty file uses put at zero threshold" `Quick
          (test_upload_empty_file_uses_put_at_zero_threshold env);
        Alcotest.test_case "multipart upload" `Quick
          (test_multipart_upload_file env);
        Alcotest.test_case "multipart upload reports abort failure" `Quick
          (test_multipart_upload_reports_abort_failure env);
        Alcotest.test_case "multipart upload aborts on progress exception"
          `Quick
          (test_multipart_upload_aborts_on_progress_exception env);
        Alcotest.test_case "multipart upload aborts on progress cancellation"
          `Quick
          (test_multipart_upload_aborts_on_progress_cancellation env);
        Alcotest.test_case "resume multipart upload" `Quick
          (test_resume_multipart_upload_file env);
        Alcotest.test_case "resume ignores mismatched listed part" `Quick
          (test_resume_multipart_upload_ignores_mismatched_listed_part env);
        Alcotest.test_case "multipart upload stops after error batch" `Quick
          (test_multipart_upload_finishes_error_batch_before_stopping env);
        Alcotest.test_case "download uses get below threshold" `Quick
          (test_download_file_uses_get_below_threshold env);
        Alcotest.test_case "download ranges" `Quick
          (test_download_file_ranges env);
        Alcotest.test_case "download ranges stop after error batch" `Quick
          (test_download_file_finishes_error_batch_before_stopping env);
        Alcotest.test_case "download ranged progress exception propagates"
          `Quick
          (test_download_file_ranged_progress_exception_propagates env);
        Alcotest.test_case "download ranges use head version id" `Quick
          (test_download_file_ranges_use_head_version_id env);
        Alcotest.test_case "download ranges use head etag" `Quick
          (test_download_file_ranges_use_head_etag env);
        Alcotest.test_case "download rejects existing target without overwrite"
          `Quick
          (test_download_file_rejects_existing_target_without_overwrite env);
      ] );
  ]
