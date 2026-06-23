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

let bucket = Awskit_s3.Bucket_name.of_string_exn "bucket"
let key = Awskit_s3.Object_key.of_string_exn "key"
let account_id = Awskit_s3.Account_id.of_string_exn

let has_progress_event ~direction ~phase ?total ?part_number transferred
    (progress : Awskit_s3.Transfer.progress) =
  progress.direction = direction
  && progress.phase = phase
  && Int64.equal progress.transferred transferred
  && progress.total = total
  && progress.part_number = part_number

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
    mutable complete_count : int;
    mutable abort_count : int;
    mutable listed_parts : Awskit_s3.List_parts.part_info list;
    mutable completed_part_etags : string list;
    mutable get_ranges : string list;
    mutable ranged_get_version_ids : string option list;
    mutable ranged_get_if_matches : string option list;
    mutable fail_ranged_get : bool;
    mutable fail_complete_upload : bool;
    mutable fail_abort_upload : bool;
    mutable list_parts_expected_owner : Awskit_s3.Account_id.t option;
  }

  type 'a t = 'a Lwt.t

  type request_body_writer = {
    buffer : Buffer.t;
    fail_after_bytes : int option;
    mutable written : int;
  }

  type request_body =
    | Body of
        Awskit.Body.Request.descriptor * (string, Awskit_s3.Error.t) result
    | Stream of
        Awskit.Body.Request.descriptor
        * (request_body_writer -> (unit, Awskit_s3.Error.t) result Lwt.t)

  type response_body = string
  type response_body_reader = { body : string; mutable offset : int }

  let write_error_after_bytes = ref None
  let read_error_after_bytes = ref None
  let reset_write_fault () = write_error_after_bytes := None
  let reset_read_fault () = read_error_after_bytes := None
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
    | Body (_, body) -> Lwt.return body
    | Stream (_, write) ->
        let writer =
          {
            buffer = Buffer.create 1024;
            fail_after_bytes = !write_error_after_bytes;
            written = 0;
          }
        in
        Lwt.bind (write writer) (function
          | Ok () -> Lwt.return_ok (Buffer.contents writer.buffer)
          | Error _ as error -> Lwt.return error)

  let request_body_descriptor = function
    | Body (descriptor, _) | Stream (descriptor, _) -> descriptor

  let write_request_body_string writer value =
    let length = String.length value in
    match writer.fail_after_bytes with
    | Some limit when writer.written + length > limit ->
        Lwt.return_error
          (Awskit.Error.Producer.body "simulated upload write failure")
    | _ ->
        Buffer.add_string writer.buffer value;
        writer.written <- writer.written + length;
        Lwt.return_ok ()

  let read_response_body reader bytes ~off ~len =
    if len = 0 then Lwt.return_ok 0
    else
      match !read_error_after_bytes with
      | Some limit when reader.offset >= limit ->
          Lwt.return_error
            (Awskit.Error.Producer.body "simulated response read failure")
      | _ ->
          let remaining = String.length reader.body - reader.offset in
          if remaining <= 0 then Lwt.return_ok 0
          else
            let copied = min len remaining in
            String.blit reader.body reader.offset bytes off copied;
            reader.offset <- reader.offset + copied;
            Lwt.return_ok copied

  let with_response_body body ~consume = consume { body; offset = 0 }
  let discard_response_body _ = Lwt.return_ok ()

  module Request_body = struct
    type 'a io = 'a Lwt.t
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
  end

  module Response_body = struct
    type 'a io = 'a Lwt.t
    type t = response_body
    type reader = response_body_reader

    let read = read_response_body

    let next ?(chunk_size = 8192) reader =
      if chunk_size <= 0 then
        Lwt.return_error
          (Awskit.Error.Producer.body "chunk_size must be positive")
      else
        let bytes = Bytes.create chunk_size in
        Lwt.bind (read_response_body reader bytes ~off:0 ~len:chunk_size)
          (function
          | Error _ as error -> Lwt.return error
          | Ok 0 -> Lwt.return_ok None
          | Ok n -> Lwt.return_ok (Some (Bytes.sub bytes 0 n)))

    let with_reader = with_response_body
    let discard = discard_response_body
  end

  module IO = struct
    type 'a t = 'a Lwt.t

    let return = Lwt.return
    let bind = Lwt.bind
  end

  module Transport = struct
    type 'a io = 'a Lwt.t
    type nonrec connection = connection
    type nonrec request_body = request_body
    type nonrec response_body = response_body

    let with_response _ _ ~body:_ ~consume:_ =
      Lwt.return_error
        (Awskit.Error.Producer.transport ~retryable:false "not implemented")
  end

  module Clock = struct
    type nonrec connection = connection

    let now _ = Ptime.epoch
  end

  module Sleeper = struct
    type 'a io = 'a Lwt.t
    type nonrec connection = connection

    let sleep _ _ = Lwt.return_unit
  end

  module Random = struct
    type nonrec connection = connection

    let float _ ~upper_bound = upper_bound /. 2.
  end

  module Credentials = struct
    type 'a io = 'a Lwt.t
    type nonrec connection = connection

    let resolve _ =
      Lwt.return_ok
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

let unsupported () =
  Lwt.return_error
    (Awskit.Error.Producer.validation ~field:"test" "not implemented")

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
    complete_count = 0;
    abort_count = 0;
    listed_parts = [];
    completed_part_etags = [];
    get_ranges = [];
    ranged_get_version_ids = [];
    ranged_get_if_matches = [];
    fail_ranged_get = false;
    fail_complete_upload = false;
    fail_abort_upload = false;
    list_parts_expected_owner = None;
  }

let response status = Awskit.Response.create_exn ~status ()

let empty_checksum : Awskit_s3.Object.Checksum.response =
  { values = []; checksum_type = None }

let listed_part ~part_number ~size ~etag =
  {
    Awskit_s3.List_parts.part_number =
      Awskit_s3.Multipart.Part_number.of_int_exn part_number;
    etag = Some (Awskit_s3.Object.Etag.of_string_exn etag);
    size = Some size;
    last_modified = None;
    checksum = empty_checksum;
  }

let put_result () : Awskit_s3.Put_object.result =
  {
    etag = None;
    version_id = None;
    checksum = empty_checksum;
    response = response 200;
  }

let get_info ?etag ?version_id content_length : Awskit_s3.Get_object.info =
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
    _ Awskit_s3.Get_object.result =
  let info = get_info ?etag ?version_id content_length in
  {
    Awskit_s3.Get_object.value;
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
    type 'a io = 'a Lwt.t
    type request_body = Runtime.request_body
    type response_body_reader = Runtime.response_body_reader

    let put conn ~bucket:_ ~key:_ ?options:_ ~body () =
      conn.Runtime.put_count <- conn.Runtime.put_count + 1;
      Lwt.bind (Runtime.drain_request_body body) (function
        | Error _ as error -> Lwt.return error
        | Ok body ->
            conn.Runtime.uploaded_body <- Some body;
            Lwt.return_ok (put_result ()))

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
            conn.Runtime.ranged_get_version_ids <-
              conn.Runtime.ranged_get_version_ids
              @ [
                  Option.bind options
                    (fun (options : Awskit_s3.Get_object.options) ->
                      Option.map Awskit_s3.Object.Version_id.to_string
                        options.version_id);
                ];
            conn.Runtime.ranged_get_if_matches <-
              conn.Runtime.ranged_get_if_matches
              @ [
                  Option.bind options
                    (fun (options : Awskit_s3.Get_object.options) ->
                      Option.map etag_condition_to_string
                        options.preconditions.if_match);
                ];
            parse_range_header header conn.Runtime.response_body
      in
      if
        conn.Runtime.fail_ranged_get
        && Option.is_some (Option.bind options (fun options -> options.range))
      then
        Lwt.return_error
          (Awskit.Error.Producer.body "simulated ranged get failure")
      else
        Lwt.bind
          (consume { Runtime.body; offset = 0 })
          (function
            | Error _ as error -> Lwt.return error
            | Ok value ->
                Lwt.return_ok
                  (get_result
                     (Some
                        (Int64.of_int
                           (String.length conn.Runtime.response_body)))
                     value))

    let head conn ~bucket:_ ~key:_ ?options:_ () =
      conn.Runtime.head_count <- conn.Runtime.head_count + 1;
      Lwt.return_ok
        (get_info ?etag:conn.Runtime.head_etag
           ?version_id:conn.Runtime.head_version_id
           (Some (Int64.of_int (String.length conn.Runtime.response_body))))

    let exists _ ~bucket:_ ~key:_ = unsupported ()
    let delete _ ~bucket:_ ~key:_ ?options:_ () = unsupported ()
    let delete_objects _ ~bucket:_ ~objects:_ ?options:_ () = unsupported ()

    let copy _ ~source_bucket:_ ~source_key:_ ~destination_bucket:_
        ~destination_key:_ ?options:_ () =
      unsupported ()

    let list_versions _ ~bucket:_ ?options:_ () = unsupported ()
    let list _ ~bucket:_ ?options:_ () = unsupported ()

    module List = struct
      type 'acc fold_step = Continue of 'acc | Stop of 'acc

      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        unsupported ()

      let fold_pages_until _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        unsupported ()

      let pages _ ~bucket:_ ?options:_ ~max_pages:_ () = unsupported ()
      let objects _ ~bucket:_ ?options:_ ~max_pages:_ () = unsupported ()
      let keys _ ~bucket:_ ?options:_ ~max_pages:_ () = unsupported ()
    end

    module Versions = struct
      type 'acc fold_step = Continue of 'acc | Stop of 'acc

      let fold_pages _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        unsupported ()

      let fold_pages_until _ ~bucket:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        unsupported ()

      let pages _ ~bucket:_ ?options:_ ~max_pages:_ () = unsupported ()

      let object_versions _ ~bucket:_ ?options:_ ~max_pages:_ () =
        unsupported ()

      let delete_markers _ ~bucket:_ ?options:_ ~max_pages:_ () = unsupported ()
    end

    let put_string _ ~bucket:_ ~key:_ ?options:_ _ = unsupported ()
    let put_bytes _ ~bucket:_ ~key:_ ?options:_ _ = unsupported ()

    let get_as_string _ ~bucket:_ ~key:_ ~max_bytes:_ ?options:_ () =
      unsupported ()

    let get_as_bytes _ ~bucket:_ ~key:_ ~max_bytes:_ ?options:_ () =
      unsupported ()

    module Tagging = struct
      let get _ ~bucket:_ ~key:_ = unsupported ()
      let put _ ~bucket:_ ~key:_ _ = unsupported ()
      let delete _ ~bucket:_ ~key:_ = unsupported ()
    end
  end

  module Multipart = struct
    type connection = Runtime.connection
    type 'a io = 'a Lwt.t
    type request_body = Runtime.request_body

    let create_upload conn ~bucket ~key ?options:_ () =
      conn.Runtime.multipart_create_count <-
        conn.Runtime.multipart_create_count + 1;
      let upload_id = Awskit_s3.Multipart.Upload_id.of_string_exn "upload-1" in
      let upload = Awskit_s3.Multipart.Upload.created ~bucket ~key ~upload_id in
      Lwt.return_ok
        { Awskit_s3.Create_multipart_upload.upload; response = response 200 }

    let upload_part conn ~upload:_ ~part_number ~body ?options:_ () =
      conn.Runtime.upload_part_count <- conn.Runtime.upload_part_count + 1;
      Lwt.bind (Runtime.drain_request_body body) (function
        | Error _ as error -> Lwt.return error
        | Ok body ->
            let part_number_int =
              Awskit_s3.Multipart.Part_number.to_int part_number
            in
            let etag =
              Awskit_s3.Object.Etag.of_string_exn
                (Fmt.str "etag-%d" part_number_int)
            in
            let size = Int64.of_int (String.length body) in
            let part =
              Awskit_s3.Multipart.Part.create_exn ~part_number ~etag ~size ()
            in
            Lwt.return_ok
              {
                Awskit_s3.Upload_part.part;
                checksum = empty_checksum;
                response = response 200;
              })

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
        Lwt.return_error
          (Awskit.Error.Producer.body "simulated complete failure")
      else
        Lwt.return_ok
          {
            Awskit_s3.Complete_multipart_upload.etag = None;
            version_id = None;
            checksum = empty_checksum;
            response = response 200;
          }

    let abort_upload conn ~upload:_ ?options:_ () =
      conn.Runtime.abort_count <- conn.Runtime.abort_count + 1;
      if conn.Runtime.fail_abort_upload then
        Lwt.return_error (Awskit.Error.Producer.body "simulated abort failure")
      else
        Lwt.return_ok
          { Awskit_s3.Abort_multipart_upload.response = response 204 }

    let list_parts _ ~upload:_ ?options:_ () = unsupported ()

    module List_parts = struct
      let fold_pages _ ~upload:_ ?options:_ ?max_pages:_ ~init:_ ~f:_ () =
        unsupported ()

      let pages _ ~upload:_ ?options:_ ?max_pages:_ () = unsupported ()

      let parts conn ~upload:_ ?options ?max_pages:_ () =
        conn.Runtime.list_parts_expected_owner <-
          Option.bind options (fun (options : Awskit_s3.List_parts.options) ->
              options.expected_bucket_owner);
        Lwt.return_ok conn.Runtime.listed_parts
    end
  end
end

module Core = Awskit_s3.Make (Runtime)
module Body_reader = Transfer_under_test.Make_body_reader (Runtime) (Core)
module Body = Body_reader.Body
module Reader = Body_reader.Reader
module Transfer = Transfer_under_test.Make (Runtime) (S3) (Body) (Reader)

let remove_file path = try Sys.remove path with Sys_error _ -> ()

let with_umask mask f =
  let previous = Unix.umask mask in
  Fun.protect ~finally:(fun () -> ignore (Unix.umask previous)) f

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

let response_reader body = { Runtime.body; offset = 0 }

type 'a observed = Returned of 'a | Raised of exn

let observe_lwt promise =
  Lwt.catch
    (fun () -> Lwt.map (fun value -> Returned value) promise)
    (fun exn -> Lwt.return (Raised exn))

let check_body_descriptor label ~content_length ~replayable body =
  let descriptor = Runtime.Request_body.descriptor body in
  let open Awskit.Body.Request in
  Alcotest.(check (option int64))
    (label ^ " content length")
    (Some content_length) descriptor.content_length;
  Alcotest.(check bool) (label ^ " replayable") replayable descriptor.replayable

let body_or_fail label = function
  | Ok body -> body
  | Error error -> Alcotest.failf "%s: %a" label Awskit_s3.Error.pp error

let checksum_value : Awskit_s3.Object.Checksum.value =
  { algorithm = Awskit_s3.Object.Checksum.Algorithm.Sha256; value = "checksum" }

let test_body_of_lwt_stream_streams_chunks () =
  Runtime.reset_write_fault ();
  let conn = connection () in
  let body =
    Body.of_lwt_stream ~content_length:11L
      (Lwt_stream.of_list [ "one"; "two"; "three" ])
    |> body_or_fail "stream body"
  in
  check_body_descriptor "stream body" ~content_length:11L ~replayable:false body;
  match Lwt_main.run (S3.Object.put conn ~bucket ~key ~body ()) with
  | Error error -> Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
  | Ok _ ->
      Alcotest.(check (option string))
        "uploaded body" (Some "onetwothree") conn.Runtime.uploaded_body

let test_body_of_lwt_stream_propagates_cancellation () =
  Runtime.reset_write_fault ();
  let conn = connection () in
  let stream = Lwt_stream.from (fun () -> Lwt.fail Lwt.Canceled) in
  let body =
    Body.of_lwt_stream ~content_length:1L stream |> body_or_fail "stream body"
  in
  match
    Lwt_main.run (observe_lwt (S3.Object.put conn ~bucket ~key ~body ()))
  with
  | Raised exn ->
      Alcotest.(check bool) "raised cancellation" true (exn == Lwt.Canceled)
  | Returned (Error error) ->
      Alcotest.failf "cancellation returned error: %a" Awskit_s3.Error.pp error
  | Returned (Ok _) -> Alcotest.fail "upload succeeded despite cancellation"

let test_body_of_channel_streams_channel () =
  Runtime.reset_write_fault ();
  let path = Filename.temp_file "awskit-upload-channel" ".bin" in
  let payload = "channel upload body" in
  write_file path payload;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let progress = ref [] in
      match
        Lwt_main.run
          (Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel ->
               let body =
                 Body.of_channel
                   ~content_length:(Int64.of_int (String.length payload))
                   ~on_progress:(fun transferred ->
                     progress := transferred :: !progress)
                   channel
                 |> body_or_fail "channel body"
               in
               check_body_descriptor "channel body"
                 ~content_length:(Int64.of_int (String.length payload))
                 ~replayable:false body;
               S3.Object.put conn ~bucket ~key ~body ()))
      with
      | Error error ->
          Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
      | Ok _ ->
          Alcotest.(check (option string))
            "uploaded body" (Some payload) conn.Runtime.uploaded_body;
          Alcotest.(check (list int64))
            "progress"
            [ Int64.of_int (String.length payload) ]
            (List.rev !progress))

let test_body_of_path_streams_file_body () =
  Runtime.reset_write_fault ();
  let path = Filename.temp_file "awskit-upload" ".bin" in
  let body = "first line\nsecond line\n" in
  write_file path body;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let progress = ref [] in
      match
        Lwt_main.run
          (Lwt.bind
             (Body.of_path
                ~on_progress:(fun transferred ->
                  progress := transferred :: !progress)
                path)
             (function
               | Error _ as error -> Lwt.return error
               | Ok request_body ->
                   check_body_descriptor "path body"
                     ~content_length:(Int64.of_int (String.length body))
                     ~replayable:true request_body;
                   S3.Object.put conn ~bucket ~key ~body:request_body ()))
      with
      | Error error ->
          Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
      | Ok _ ->
          Alcotest.(check (option string))
            "uploaded body" (Some body) conn.Runtime.uploaded_body;
          Alcotest.(check (list int64))
            "progress"
            [ Int64.of_int (String.length body) ]
            (List.rev !progress))

let test_body_of_path_returns_stream_write_error () =
  Runtime.write_error_after_bytes := Some 0;
  let path = Filename.temp_file "awskit-upload-error" ".bin" in
  write_file path "body that cannot be written";
  Fun.protect
    ~finally:(fun () ->
      Runtime.reset_write_fault ();
      remove_file path)
    (fun () ->
      let conn = connection () in
      match
        Lwt_main.run
          (Lwt.bind (Body.of_path path) (function
            | Error _ as error -> Lwt.return error
            | Ok body -> S3.Object.put conn ~bucket ~key ~body ()))
      with
      | Ok _ -> Alcotest.fail "upload succeeded despite write failure"
      | Error error ->
          Alcotest.(check (option string))
            "no stored body" None conn.Runtime.uploaded_body;
          Alcotest.(check string)
            "error" "body: simulated upload write failure"
            (Fmt.str "%a" Awskit_s3.Error.pp error))

let test_body_of_path_rejects_non_regular_file () =
  let path = Filename.temp_file "awskit-upload-directory" "" in
  remove_file path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () -> Unix.rmdir path)
    (fun () ->
      match Lwt_main.run (Body.of_path path) with
      | Error error when is_validation_field "path" error -> ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected path validation")

let test_upload_file_progress_exception_propagates () =
  Runtime.reset_write_fault ();
  let exception Progress_failed in
  let path = Filename.temp_file "awskit-upload-progress-exn" ".bin" in
  write_file path "small";
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = 1024L;
        }
      in
      match
        Lwt_main.run
          (observe_lwt
             (Transfer.upload_file conn ~bucket ~key ~options
                ~on_progress:(fun _transferred -> raise Progress_failed)
                ~path ()))
      with
      | Raised exn ->
          Alcotest.(check bool)
            "raised callback exception" true (exn == Progress_failed)
      | Returned (Error error) ->
          Alcotest.failf "callback returned error: %a" Awskit_s3.Error.pp error
      | Returned (Ok _) ->
          Alcotest.fail "upload succeeded despite callback exception")

let test_reader_to_channel_writes_response_body () =
  let path = Filename.temp_file "awskit-download-channel" ".bin" in
  let body = "channel download body" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let progress = ref [] in
      match
        Lwt_main.run
          (Lwt_io.with_file ~mode:Lwt_io.Output path (fun channel ->
               Reader.to_channel
                 ~on_progress:(fun transferred ->
                   progress := transferred :: !progress)
                 channel (response_reader body)))
      with
      | Error error ->
          Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
      | Ok () ->
          Alcotest.(check string) "body" body (read_file path);
          Alcotest.(check (list int64))
            "progress"
            [ Int64.of_int (String.length body) ]
            (List.rev !progress))

let test_reader_to_path_creates_private_file () =
  let path = Filename.temp_file "awskit-download-perm" ".bin" in
  let body = "secret downloaded file body" in
  write_file path "old body";
  Unix.chmod path 0o666;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      with_umask 0 (fun () ->
          match Lwt_main.run (Reader.to_path path (response_reader body)) with
          | Error error ->
              Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
          | Ok () -> ());
      Alcotest.(check string) "body" body (read_file path);
      let stat = Unix.stat path in
      Alcotest.(check int) "mode" 0o600 (stat.Unix.st_perm land 0o777))

let test_reader_to_path_failure_preserves_target () =
  let path = Filename.temp_file "awskit-download-failure" ".bin" in
  let old_body = "old body" in
  let new_body = "new body" in
  write_file path old_body;
  Runtime.read_error_after_bytes := Some 0;
  Fun.protect
    ~finally:(fun () ->
      Runtime.reset_read_fault ();
      remove_download_temps path;
      remove_file path)
    (fun () ->
      match Lwt_main.run (Reader.to_path path (response_reader new_body)) with
      | Ok () -> Alcotest.fail "download succeeded despite read failure"
      | Error error when is_body_error error ->
          Alcotest.(check string) "preserved body" old_body (read_file path);
          Alcotest.(check int)
            "temp files removed" 0
            (List.length (download_temp_paths path))
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error)

let test_reader_to_path_reports_cleanup_failure () =
  let dir = Filename.temp_file "awskit-download-cleanup" "" in
  remove_file dir;
  Unix.mkdir dir 0o700;
  let path = Filename.concat dir "target.bin" in
  let old_body = "old body" in
  let new_body = String.make ((128 * 1024) + 1) 'n' in
  write_file path old_body;
  Runtime.read_error_after_bytes := Some (128 * 1024);
  Fun.protect
    ~finally:(fun () ->
      Runtime.reset_read_fault ();
      Unix.chmod dir 0o700;
      remove_download_temps path;
      remove_file path;
      Unix.rmdir dir)
    (fun () ->
      match
        Lwt_main.run
          (Reader.to_path
             ~on_progress:(fun _transferred -> Unix.chmod dir 0o500)
             path (response_reader new_body))
      with
      | Ok () -> Alcotest.fail "download succeeded despite read failure"
      | Error error ->
          Alcotest.(check string) "preserved body" old_body (read_file path);
          check_multiple_error_text "cleanup error" error
            [
              "simulated response read failure";
              "failed to remove temporary download";
            ])

let test_upload_file_uses_put_below_threshold () =
  Runtime.reset_write_fault ();
  let path = Filename.temp_file "awskit-upload-put" ".bin" in
  write_file path "small";
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let progress = ref [] in
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = 1024L;
        }
      in
      match
        Lwt_main.run
          (Transfer.upload_file conn ~bucket ~key ~options ~path
             ~on_progress:(fun event -> progress := event :: !progress)
             ())
      with
      | Error error ->
          Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.upload_strategy result = `Put);
          Alcotest.(check int64)
            "bytes transferred" 5L
            (Awskit_s3.Transfer.upload_bytes_transferred result);
          Alcotest.(check int) "put count" 1 conn.Runtime.put_count;
          Alcotest.(check int)
            "multipart create count" 0 conn.Runtime.multipart_create_count;
          Alcotest.(check bool)
            "progress" true
            (Option.is_some
               (List.find_opt
                  (has_progress_event ~direction:Awskit_s3.Transfer.Upload
                     ~phase:Awskit_s3.Transfer.Single_request ~total:5L 5L)
                  !progress)))

let test_upload_empty_file_uses_put_at_zero_threshold () =
  Runtime.reset_write_fault ();
  let path = Filename.temp_file "awskit-upload-empty-put" ".bin" in
  write_file path "";
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = 0L;
        }
      in
      match
        Lwt_main.run (Transfer.upload_file conn ~bucket ~key ~options ~path ())
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

let test_upload_file_allows_put_checksum_below_threshold () =
  Runtime.reset_write_fault ();
  let path = Filename.temp_file "awskit-upload-put-checksum" ".bin" in
  write_file path "small";
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let put_options =
        {
          Awskit_s3.Put_object.default_options with
          checksum = Some checksum_value;
        }
      in
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = 1024L;
          put_options;
        }
      in
      match
        Lwt_main.run (Transfer.upload_file conn ~bucket ~key ~options ~path ())
      with
      | Error error ->
          Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.upload_strategy result = `Put);
          Alcotest.(check int) "put count" 1 conn.Runtime.put_count;
          Alcotest.(check int)
            "multipart create count" 0 conn.Runtime.multipart_create_count)

let test_upload_file_uses_multipart_at_threshold () =
  Runtime.reset_write_fault ();
  let path = Filename.temp_file "awskit-upload-multipart" ".bin" in
  let body = String.make Awskit_s3.Transfer.min_part_size 'm' in
  write_file path body;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let progress = ref [] in
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = Int64.of_int Awskit_s3.Transfer.min_part_size;
          part_size = Awskit_s3.Transfer.min_part_size;
        }
      in
      match
        Lwt_main.run
          (Transfer.upload_file conn ~bucket ~key ~options ~path
             ~on_progress:(fun event -> progress := event :: !progress)
             ())
      with
      | Error error ->
          Alcotest.failf "upload failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.upload_strategy result = `Multipart);
          Alcotest.(check int64)
            "bytes transferred"
            (Int64.of_int Awskit_s3.Transfer.min_part_size)
            (Awskit_s3.Transfer.upload_bytes_transferred result);
          Alcotest.(check int) "put count" 0 conn.Runtime.put_count;
          Alcotest.(check int)
            "multipart create count" 1 conn.Runtime.multipart_create_count;
          Alcotest.(check int)
            "upload part count" 1 conn.Runtime.upload_part_count;
          Alcotest.(check int) "complete count" 1 conn.Runtime.complete_count;
          Alcotest.(check bool)
            "progress" true
            (Option.is_some
               (List.find_opt
                  (has_progress_event ~direction:Awskit_s3.Transfer.Upload
                     ~phase:Awskit_s3.Transfer.Part
                     ~total:(Int64.of_int Awskit_s3.Transfer.min_part_size)
                     ~part_number:(Awskit_s3.Multipart.Part_number.of_int_exn 1)
                     (Int64.of_int Awskit_s3.Transfer.min_part_size))
                  !progress)))

let test_upload_file_rejects_invalid_options () =
  let path = Filename.temp_file "awskit-upload-invalid" ".bin" in
  write_file path (String.make Awskit_s3.Transfer.min_part_size 'x');
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let bad_concurrency =
        { Awskit_s3.Transfer.default_upload_options with concurrency = 0 }
      in
      (match
         Lwt_main.run
           (Transfer.upload_file conn ~bucket ~key ~options:bad_concurrency
              ~path ())
       with
      | Error error when is_validation_field "concurrency" error -> ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected concurrency validation");
      let put_options =
        {
          Awskit_s3.Put_object.default_options with
          checksum = Some checksum_value;
        }
      in
      let bad_put_checksum =
        {
          Awskit_s3.Transfer.default_upload_options with
          multipart_threshold = 0L;
          put_options;
        }
      in
      (match
         Lwt_main.run
           (Transfer.upload_file conn ~bucket ~key ~options:bad_put_checksum
              ~path ())
       with
      | Error error when is_validation_field "put_options.checksum" error -> ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected put checksum validation");
      (match
         Lwt_main.run
           (Transfer.multipart_upload_file conn ~bucket ~key
              ~options:bad_put_checksum ~path ())
       with
      | Error error when is_validation_field "put_options.checksum" error -> ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected explicit multipart checksum validation");
      let upload_part_options =
        {
          Awskit_s3.Upload_part.default_options with
          checksum = Some checksum_value;
        }
      in
      let bad_part_checksum =
        { Awskit_s3.Transfer.default_upload_options with upload_part_options }
      in
      (match
         Lwt_main.run
           (Transfer.multipart_upload_file conn ~bucket ~key
              ~options:bad_part_checksum ~path ())
       with
      | Error error
        when is_validation_field "upload_part_options.checksum" error ->
          ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected upload-part checksum validation");
      let create_options =
        {
          Awskit_s3.Create_multipart_upload.default_options with
          checksum_algorithm = Some Awskit_s3.Object.Checksum.Algorithm.Sha256;
        }
      in
      let bad_create_checksum =
        { Awskit_s3.Transfer.default_upload_options with create_options }
      in
      (match
         Lwt_main.run
           (Transfer.multipart_upload_file conn ~bucket ~key
              ~options:bad_create_checksum ~path ())
       with
      | Error error
        when is_validation_field "create_options.checksum_algorithm" error ->
          ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected create checksum validation");
      let complete_options =
        {
          Awskit_s3.Complete_multipart_upload.default_options with
          checksum = Some checksum_value;
        }
      in
      let bad_complete_checksum =
        { Awskit_s3.Transfer.default_upload_options with complete_options }
      in
      (match
         Lwt_main.run
           (Transfer.multipart_upload_file conn ~bucket ~key
              ~options:bad_complete_checksum ~path ())
       with
      | Error error when is_validation_field "complete_options.checksum" error
        ->
          ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected complete checksum validation");
      let create_options =
        {
          Awskit_s3.Create_multipart_upload.default_options with
          checksum_type = Some Awskit_s3.Object.Checksum.Type.Composite;
        }
      in
      let bad_create_checksum_type =
        { Awskit_s3.Transfer.default_upload_options with create_options }
      in
      (match
         Lwt_main.run
           (Transfer.multipart_upload_file conn ~bucket ~key
              ~options:bad_create_checksum_type ~path ())
       with
      | Error error
        when is_validation_field "create_options.checksum_type" error ->
          ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected create checksum-type validation");
      let complete_options =
        {
          Awskit_s3.Complete_multipart_upload.default_options with
          checksum_type = Some Awskit_s3.Object.Checksum.Type.Composite;
        }
      in
      let bad_complete_checksum_type =
        { Awskit_s3.Transfer.default_upload_options with complete_options }
      in
      match
        Lwt_main.run
          (Transfer.multipart_upload_file conn ~bucket ~key
             ~options:bad_complete_checksum_type ~path ())
      with
      | Error error
        when is_validation_field "complete_options.checksum_type" error ->
          ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected complete checksum-type validation")

let test_resume_multipart_upload_file_uses_list_parts_options () =
  let path = Filename.temp_file "awskit-resume-list-options" ".bin" in
  write_file path (String.make Awskit_s3.Transfer.min_part_size 'r');
  Fun.protect
    ~finally:(fun () -> remove_file path)
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
      let list_parts_options =
        {
          Awskit_s3.List_parts.default_options with
          expected_bucket_owner = Some (account_id "123456789012");
        }
      in
      let options =
        { Awskit_s3.Transfer.default_upload_options with list_parts_options }
      in
      match
        Lwt_main.run
          (Transfer.resume_multipart_upload_file conn ~upload ~options ~path ())
      with
      | Error error ->
          Alcotest.failf "resume failed: %a" Awskit_s3.Error.pp error
      | Ok _ ->
          Alcotest.(
            check
              (option
                 (testable Awskit_s3.Account_id.pp Awskit_s3.Account_id.equal)))
            "list expected owner"
            (Some (account_id "123456789012"))
            conn.Runtime.list_parts_expected_owner;
          Alcotest.(check int)
            "upload part count" 1 conn.Runtime.upload_part_count;
          Alcotest.(check int) "complete count" 1 conn.Runtime.complete_count;
          Alcotest.(check (list string))
            "completed fresh part etags" [ "etag-1" ]
            conn.Runtime.completed_part_etags)

let test_multipart_upload_reports_abort_failure () =
  let path = Filename.temp_file "awskit-upload-abort-failure" ".bin" in
  write_file path (String.make Awskit_s3.Transfer.min_part_size 'a');
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      conn.Runtime.fail_complete_upload <- true;
      conn.Runtime.fail_abort_upload <- true;
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          part_size = Awskit_s3.Transfer.min_part_size;
        }
      in
      match
        Lwt_main.run
          (Transfer.multipart_upload_file conn ~bucket ~key ~options ~path ())
      with
      | Ok _ -> Alcotest.fail "multipart upload succeeded despite failures"
      | Error error ->
          Alcotest.(check int) "complete count" 1 conn.Runtime.complete_count;
          Alcotest.(check int) "abort count" 1 conn.Runtime.abort_count;
          check_multiple_error_text "abort error" error
            [ "simulated complete failure"; "simulated abort failure" ])

let test_multipart_upload_aborts_on_progress_exception () =
  Runtime.reset_write_fault ();
  let exception Progress_failed in
  let path = Filename.temp_file "awskit-upload-multipart-progress-exn" ".bin" in
  write_file path (String.make (Awskit_s3.Transfer.min_part_size * 2) 'p');
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          part_size = Awskit_s3.Transfer.min_part_size;
          concurrency = 2;
        }
      in
      match
        Lwt_main.run
          (observe_lwt
             (Transfer.multipart_upload_file conn ~bucket ~key ~options
                ~on_progress:(fun _transferred -> raise Progress_failed)
                ~path ()))
      with
      | Raised exn ->
          Alcotest.(check bool)
            "raised callback exception" true (exn == Progress_failed);
          Alcotest.(check int)
            "upload part count" 2 conn.Runtime.upload_part_count;
          Alcotest.(check int) "abort count" 1 conn.Runtime.abort_count;
          Alcotest.(check int) "complete count" 0 conn.Runtime.complete_count
      | Returned (Error error) ->
          Alcotest.failf "callback returned error: %a" Awskit_s3.Error.pp error
      | Returned (Ok _) ->
          Alcotest.fail "multipart upload succeeded despite callback exception")

let test_multipart_upload_aborts_on_progress_cancellation () =
  Runtime.reset_write_fault ();
  let path =
    Filename.temp_file "awskit-upload-multipart-progress-cancel" ".bin"
  in
  write_file path (String.make Awskit_s3.Transfer.min_part_size 'c');
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection () in
      let options =
        {
          Awskit_s3.Transfer.default_upload_options with
          part_size = Awskit_s3.Transfer.min_part_size;
        }
      in
      match
        Lwt_main.run
          (observe_lwt
             (Transfer.multipart_upload_file conn ~bucket ~key ~options
                ~on_progress:(fun _transferred -> raise Lwt.Canceled)
                ~path ()))
      with
      | Raised exn ->
          Alcotest.(check bool) "raised cancellation" true (exn == Lwt.Canceled);
          Alcotest.(check int) "abort count" 1 conn.Runtime.abort_count;
          Alcotest.(check int) "complete count" 0 conn.Runtime.complete_count
      | Returned (Error error) ->
          Alcotest.failf "cancellation returned error: %a" Awskit_s3.Error.pp
            error
      | Returned (Ok _) ->
          Alcotest.fail "multipart upload succeeded despite cancellation")

let test_download_file_uses_get_below_threshold () =
  let path = Filename.temp_file "awskit-download-get" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let body = "small download" in
      let conn = connection ~response_body:body () in
      let progress = ref [] in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = 1024L;
        }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path
             ~on_progress:(fun event -> progress := event :: !progress)
             ())
      with
      | Error error ->
          Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.download_strategy result = `Get);
          Alcotest.(check int64)
            "bytes transferred"
            (Int64.of_int (String.length body))
            (Awskit_s3.Transfer.download_bytes_transferred result);
          Alcotest.(check string) "body" body (read_file path);
          Alcotest.(check int) "head count" 1 conn.Runtime.head_count;
          Alcotest.(check int) "get count" 1 conn.Runtime.get_count;
          Alcotest.(check int)
            "range count" 0
            (List.length conn.Runtime.get_ranges);
          Alcotest.(check bool)
            "progress" true
            (Option.is_some
               (List.find_opt
                  (has_progress_event ~direction:Awskit_s3.Transfer.Download
                     ~phase:Awskit_s3.Transfer.Single_request
                     ~total:(Int64.of_int (String.length body))
                     (Int64.of_int (String.length body)))
                  !progress)))

let test_download_file_progress_exception_propagates () =
  let exception Progress_failed in
  let path = Filename.temp_file "awskit-download-progress-exn" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () ->
      remove_download_temps path;
      remove_file path)
    (fun () ->
      let conn = connection ~response_body:"small download" () in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = 1024L;
        }
      in
      match
        Lwt_main.run
          (observe_lwt
             (Transfer.download_file conn ~bucket ~key ~options
                ~on_progress:(fun _transferred -> raise Progress_failed)
                ~path ()))
      with
      | Raised exn ->
          Alcotest.(check bool)
            "raised callback exception" true (exn == Progress_failed)
      | Returned (Error error) ->
          Alcotest.failf "callback returned error: %a" Awskit_s3.Error.pp error
      | Returned (Ok _) ->
          Alcotest.fail "download succeeded despite callback exception")

let test_download_file_ranged_progress_exception_propagates () =
  let exception Progress_failed in
  let path = Filename.temp_file "awskit-download-ranged-progress-exn" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () ->
      remove_download_temps path;
      remove_file path)
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
      let observed =
        Lwt_main.run
          (Lwt.catch
             (fun () ->
               Lwt.map
                 (fun result -> Returned result)
                 (Transfer.download_file conn ~bucket ~key ~options
                    ~on_progress:(fun _transferred -> raise Progress_failed)
                    ~path ()))
             (fun exn -> Lwt.return (Raised exn)))
      in
      Alcotest.(check bool)
        "ranged get attempted" true
        (List.length conn.Runtime.get_ranges > 0);
      match observed with
      | Raised exn ->
          Alcotest.(check bool)
            "raised callback exception" true (exn == Progress_failed)
      | Returned (Error error) ->
          Alcotest.failf "callback returned error: %a" Awskit_s3.Error.pp error
      | Returned (Ok _) ->
          Alcotest.fail "download succeeded despite callback exception")

let test_download_file_uses_ranges_at_threshold () =
  let path = Filename.temp_file "awskit-download-ranged" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let conn = connection ~response_body:body () in
      let progress = ref [] in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = Int64.of_int part_size;
          part_size;
          concurrency = 2;
        }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path
             ~on_progress:(fun transferred ->
               progress := transferred :: !progress)
             ())
      with
      | Error error ->
          Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.download_strategy result = `Ranged);
          Alcotest.(check int64)
            "bytes transferred"
            (Int64.of_int (String.length body))
            (Awskit_s3.Transfer.download_bytes_transferred result);
          (match result with
          | Awskit_s3.Transfer.Ranged { parts; _ } ->
              Alcotest.(check int) "parts" 2 parts
          | _ -> Alcotest.fail "expected ranged result");
          Alcotest.(check string) "body" body (read_file path);
          Alcotest.(check int) "head count" 1 conn.Runtime.head_count;
          Alcotest.(check int) "get count" 2 conn.Runtime.get_count;
          Alcotest.(check (list string))
            "ranges"
            [
              Fmt.str "bytes=0-%d" (part_size - 1);
              Fmt.str "bytes=%d-%d" part_size (String.length body - 1);
            ]
            conn.Runtime.get_ranges;
          Alcotest.(check (option int64))
            "final progress"
            (Some (Int64.of_int (String.length body)))
            (List.find_map
               (fun progress ->
                 if
                   has_progress_event ~direction:Awskit_s3.Transfer.Download
                     ~phase:Awskit_s3.Transfer.Ranged_get
                     ~total:(Int64.of_int (String.length body))
                     (Int64.of_int (String.length body))
                     progress
                 then Some progress.transferred
                 else None)
               !progress))

let test_download_file_ranges_use_head_version_id () =
  let path = Filename.temp_file "awskit-download-ranged-version" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let version_id = Awskit_s3.Object.Version_id.of_string_exn "version-1" in
      let conn =
        connection ~response_body:body ~head_version_id:version_id ()
      in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = Int64.of_int part_size;
          part_size;
          concurrency = 2;
        }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path ())
      with
      | Error error ->
          Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.download_strategy result = `Ranged);
          Alcotest.(check (list (option string)))
            "ranged version ids"
            [ Some "version-1"; Some "version-1" ]
            conn.Runtime.ranged_get_version_ids)

let test_download_file_ranges_use_head_etag () =
  let path = Filename.temp_file "awskit-download-ranged-etag" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let body = String.make part_size 'a' ^ "tail" in
      let etag = Awskit_s3.Object.Etag.of_string_exn "\"head-etag\"" in
      let conn = connection ~response_body:body ~head_etag:etag () in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = Int64.of_int part_size;
          part_size;
          concurrency = 2;
        }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path ())
      with
      | Error error ->
          Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          Alcotest.(check bool)
            "strategy" true
            (Awskit_s3.Transfer.download_strategy result = `Ranged);
          Alcotest.(check (list (option string)))
            "ranged if-matches"
            [ Some "\"head-etag\""; Some "\"head-etag\"" ]
            conn.Runtime.ranged_get_if_matches)

let test_download_file_allows_small_range_parts () =
  let path = Filename.temp_file "awskit-download-small-ranges" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let body = "abcdefghi" in
      let conn = connection ~response_body:body () in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = 1L;
          part_size = 4;
          concurrency = 2;
        }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path ())
      with
      | Error error ->
          Alcotest.failf "download failed: %a" Awskit_s3.Error.pp error
      | Ok result ->
          (match result with
          | Awskit_s3.Transfer.Ranged { parts; _ } ->
              Alcotest.(check int) "parts" 3 parts
          | _ -> Alcotest.fail "expected ranged result");
          Alcotest.(check string) "body" body (read_file path);
          Alcotest.(check int) "get count" 3 conn.Runtime.get_count;
          Alcotest.(check (list string))
            "ranges"
            [ "bytes=0-3"; "bytes=4-7"; "bytes=8-8" ]
            conn.Runtime.get_ranges)

let test_download_file_rejects_range_option () =
  let path = Filename.temp_file "awskit-download-range-option" ".bin" in
  remove_file path;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection ~response_body:"body" () in
      let get_options =
        {
          Awskit_s3.Get_object.default_options with
          range = Some (Awskit_s3.Range.bytes_exn ~start:0L ~finish:1L);
        }
      in
      let options =
        { Awskit_s3.Transfer.default_download_options with get_options }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path ())
      with
      | Error error when is_validation_field "get_options.range" error -> ()
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected range validation")

let test_download_file_rejects_existing_target_without_overwrite () =
  let path = Filename.temp_file "awskit-download-no-overwrite" ".bin" in
  write_file path "existing";
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let conn = connection ~response_body:"new" () in
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          overwrite = Awskit_s3.Transfer.Error_if_exists;
        }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path ())
      with
      | Error error when is_validation_field "path" error ->
          Alcotest.(check string) "preserved target" "existing" (read_file path);
          Alcotest.(check int) "head count" 0 conn.Runtime.head_count;
          Alcotest.(check int) "get count" 0 conn.Runtime.get_count
      | Error error ->
          Alcotest.failf "unexpected error: %a" Awskit_s3.Error.pp error
      | Ok _ -> Alcotest.fail "expected overwrite validation")

let test_download_file_ranged_failure_preserves_target () =
  let path = Filename.temp_file "awskit-download-preserve" ".bin" in
  let original = "existing target" in
  write_file path original;
  Fun.protect
    ~finally:(fun () -> remove_file path)
    (fun () ->
      let part_size = Awskit_s3.Transfer.min_part_size in
      let conn = connection ~response_body:(String.make part_size 'x') () in
      conn.Runtime.fail_ranged_get <- true;
      let options =
        {
          Awskit_s3.Transfer.default_download_options with
          multipart_threshold = Int64.of_int part_size;
          part_size;
        }
      in
      match
        Lwt_main.run
          (Transfer.download_file conn ~bucket ~key ~options ~path ())
      with
      | Ok _ -> Alcotest.fail "download succeeded despite ranged failure"
      | Error _ ->
          Alcotest.(check string) "target preserved" original (read_file path);
          Alcotest.(check int)
            "temp files removed" 0
            (List.length (download_temp_paths path)))

let suite () =
  [
    ( "transfer",
      [
        Alcotest.test_case "body streams lwt stream" `Quick
          test_body_of_lwt_stream_streams_chunks;
        Alcotest.test_case "body lwt stream propagates cancellation" `Quick
          test_body_of_lwt_stream_propagates_cancellation;
        Alcotest.test_case "body streams channel" `Quick
          test_body_of_channel_streams_channel;
        Alcotest.test_case "body streams path" `Quick
          test_body_of_path_streams_file_body;
        Alcotest.test_case "body path returns stream write error" `Quick
          test_body_of_path_returns_stream_write_error;
        Alcotest.test_case "body path rejects non-regular file" `Quick
          test_body_of_path_rejects_non_regular_file;
        Alcotest.test_case "upload progress exception propagates" `Quick
          test_upload_file_progress_exception_propagates;
        Alcotest.test_case "reader writes channel" `Quick
          test_reader_to_channel_writes_response_body;
        Alcotest.test_case "reader path creates private file" `Quick
          test_reader_to_path_creates_private_file;
        Alcotest.test_case "reader path failure preserves target" `Quick
          test_reader_to_path_failure_preserves_target;
        Alcotest.test_case "reader path reports cleanup failure" `Quick
          test_reader_to_path_reports_cleanup_failure;
        Alcotest.test_case "upload uses put below threshold" `Quick
          test_upload_file_uses_put_below_threshold;
        Alcotest.test_case "upload empty file uses put at zero threshold" `Quick
          test_upload_empty_file_uses_put_at_zero_threshold;
        Alcotest.test_case "upload allows put checksum below threshold" `Quick
          test_upload_file_allows_put_checksum_below_threshold;
        Alcotest.test_case "upload uses multipart at threshold" `Quick
          test_upload_file_uses_multipart_at_threshold;
        Alcotest.test_case "upload rejects invalid options" `Quick
          test_upload_file_rejects_invalid_options;
        Alcotest.test_case "resume uses list-parts options" `Quick
          test_resume_multipart_upload_file_uses_list_parts_options;
        Alcotest.test_case "multipart upload reports abort failure" `Quick
          test_multipart_upload_reports_abort_failure;
        Alcotest.test_case "multipart upload aborts on progress exception"
          `Quick test_multipart_upload_aborts_on_progress_exception;
        Alcotest.test_case "multipart upload aborts on progress cancellation"
          `Quick test_multipart_upload_aborts_on_progress_cancellation;
        Alcotest.test_case "download uses get below threshold" `Quick
          test_download_file_uses_get_below_threshold;
        Alcotest.test_case "download progress exception propagates" `Quick
          test_download_file_progress_exception_propagates;
        Alcotest.test_case "download ranged progress exception propagates"
          `Quick test_download_file_ranged_progress_exception_propagates;
        Alcotest.test_case "download uses ranges at threshold" `Quick
          test_download_file_uses_ranges_at_threshold;
        Alcotest.test_case "download ranges use head version id" `Quick
          test_download_file_ranges_use_head_version_id;
        Alcotest.test_case "download ranges use head etag" `Quick
          test_download_file_ranges_use_head_etag;
        Alcotest.test_case "download allows small range parts" `Quick
          test_download_file_allows_small_range_parts;
        Alcotest.test_case "download rejects range option" `Quick
          test_download_file_rejects_range_option;
        Alcotest.test_case "download rejects existing target without overwrite"
          `Quick test_download_file_rejects_existing_target_without_overwrite;
        Alcotest.test_case "download ranged failure preserves target" `Quick
          test_download_file_ranged_failure_preserves_target;
      ] );
  ]
