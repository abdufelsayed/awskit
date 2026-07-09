let buffer_size = 128 * 1024
let temp_counter = Atomic.make 0

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let body_error action path exn =
  Awskit.Error.Producer.body
    (Fmt.str "failed to %s path %a: %s" action Eio.Path.pp path
       (Printexc.to_string exn))

let target_error action target exn =
  Awskit.Error.Producer.body
    (Fmt.str "failed to %s %s: %s" action target (Printexc.to_string exn))

exception Callback_raised of exn

let notify_progress callback value =
  match callback with
  | None -> ()
  | Some f -> (
      try f value with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | Callback_raised _ as exn -> raise exn
      | exn -> raise (Callback_raised exn))

let raise_callback_or_raise = function
  | Callback_raised exn -> raise exn
  | exn -> raise exn

let body_error_or_raise_callback action path = function
  | Callback_raised exn -> raise exn
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (body_error action path exn)

let body_error_or_escape_callback action path = function
  | Callback_raised exn -> Awskit.Body.Request.raise_escaped_exn exn
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (body_error action path exn)

let body_error_or_preserve_callback action path = function
  | Callback_raised _ as exn -> raise exn
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (body_error action path exn)

let target_error_or_raise_callback action target = function
  | Callback_raised exn -> raise exn
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (target_error action target exn)

let target_error_or_escape_callback action target = function
  | Callback_raised exn -> Awskit.Body.Request.raise_escaped_exn exn
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (target_error action target exn)

let raise_escaped_callback_or_raise = function
  | exn -> (
      match Awskit.Body.Request.escaped_exn exn with
      | Some escaped -> raise escaped
      | None -> raise exn)

let regular_file_length path =
  try
    let stat = Eio.Path.stat ~follow:true path in
    match stat.kind with
    | `Regular_file -> Ok (Optint.Int63.to_int64 stat.size)
    | kind ->
        Error
          (Awskit.Error.Producer.validation ~field:"path"
             (Fmt.str "expected regular file, got %a" Eio.File.Stat.pp_kind kind))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (body_error "stat upload" path exn)

let reject_existing_download_target path =
  try
    ignore (Eio.Path.stat ~follow:true path);
    Error
      (Awskit.Error.Producer.validation ~field:"path"
         "download target already exists")
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Ok ()
  | exn -> Error (body_error "stat download target" path exn)

let validate_download_target path
    (options : Awskit_s3.Transfer.download_options) =
  match options.overwrite with
  | Awskit_s3.Transfer.Replace -> Ok ()
  | Awskit_s3.Transfer.Error_if_exists -> reject_existing_download_target path

let descriptor ~content_length ~replayable =
  Awskit.Body.Request.descriptor_exn ~content_length
    ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable ()

let temp_download_path path attempt =
  match Eio.Path.split path with
  | Some (dir, base) ->
      let id = Atomic.fetch_and_add temp_counter 1 in
      Ok
        Eio.Path.(
          dir / Fmt.str ".%s.awskit-download.%08x.%d.tmp" base id attempt)
  | None ->
      Error
        (Awskit.Error.Producer.validation ~field:"path"
           "could not derive temporary download path")

let remove_temp_download path =
  try
    Eio.Path.unlink path;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Ok ()
  | exn -> Error (body_error "remove temporary download" path exn)

let close_temp_download path file =
  try
    Eio.Resource.close file;
    Ok ()
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (body_error "close temporary download" path exn)

let cleanup_temp_download path error =
  match remove_temp_download path with
  | Ok () -> Error error
  | Error cleanup_error ->
      Error
        (Awskit.Error.Producer.multiple [ error; cleanup_error ]
        |> Awskit.Error.Producer.with_context
             "download failed and temporary file cleanup also failed")

let cleanup_temp_download_with_failures path errors =
  let errors =
    match remove_temp_download path with
    | Ok () -> errors
    | Error cleanup_error -> errors @ [ cleanup_error ]
  in
  Error
    (Awskit.Error.Producer.multiple errors
    |> Awskit.Error.Producer.with_context
         "download failed and temporary file cleanup also failed")

let cleanup_temp_download_before_raise path exn =
  Eio.Cancel.protect (fun () -> ignore (remove_temp_download path));
  raise exn

let cleanup_open_temp_download_before_raise path file exn =
  Eio.Cancel.protect (fun () ->
      ignore (close_temp_download path file);
      ignore (remove_temp_download path));
  raise exn

let reserve_temp_download_file ~sw path =
  let rec loop attempt =
    if attempt >= 100 then
      Error
        (Awskit.Error.Producer.body
           (Fmt.str "failed to reserve temporary download path for %a"
              Eio.Path.pp path))
    else
      let* temp_path = temp_download_path path attempt in
      try
        let file = Eio.Path.open_out ~sw ~create:(`Exclusive 0o600) temp_path in
        Ok (temp_path, file)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | Eio.Exn.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> loop (attempt + 1)
      | exn -> Error (body_error "create temporary download" temp_path exn)
  in
  loop 0

let with_temp_download path f =
  Eio.Switch.run ~name:"awskit temporary download" @@ fun sw ->
  match reserve_temp_download_file ~sw path with
  | Error _ as error -> error
  | Ok (temp_path, file) -> (
      let close_and_cleanup error =
        match close_temp_download temp_path file with
        | exception (Eio.Cancel.Cancelled _ as exn) ->
            cleanup_open_temp_download_before_raise temp_path file exn
        | Error close_error ->
            cleanup_temp_download_with_failures temp_path [ error; close_error ]
        | Ok () -> cleanup_temp_download temp_path error
      in
      let close_and_publish value =
        match close_temp_download temp_path file with
        | exception (Eio.Cancel.Cancelled _ as exn) ->
            cleanup_open_temp_download_before_raise temp_path file exn
        | Error close_error -> cleanup_temp_download temp_path close_error
        | Ok () -> (
            try
              Eio.Path.rename temp_path path;
              Ok value
            with
            | Eio.Cancel.Cancelled _ as exn ->
                cleanup_temp_download_before_raise temp_path exn
            | exn ->
                cleanup_temp_download temp_path
                  (body_error "rename download" path exn))
      in
      match f temp_path file with
      | exception (Eio.Cancel.Cancelled _ as exn) ->
          cleanup_open_temp_download_before_raise temp_path file exn
      | exception Callback_raised exn ->
          cleanup_open_temp_download_before_raise temp_path file exn
      | exception exn ->
          close_and_cleanup (body_error "write download" path exn)
      | Error error -> close_and_cleanup error
      | Ok value -> close_and_publish value)

module Make_body_reader
    (Runtime : Awskit.Runtime.S with type 'a t = 'a)
    (S3 : sig
      module Body :
        Awskit_s3.BODY with type 'a io := 'a and type t = Runtime.request_body

      module Reader :
        Awskit_s3.READER
          with type 'a io := 'a
           and type t = Runtime.response_body_reader
    end) =
struct
  module Body = struct
    include S3.Body

    let copy_flow_to_writer ?on_progress flow writer =
      let cstruct = Cstruct.create buffer_size in
      let rec loop transferred =
        match Eio.Flow.single_read flow cstruct with
        | 0 -> Ok ()
        | n -> (
            match
              Writer.write_string writer (Cstruct.to_string ~len:n cstruct)
            with
            | Error _ as error -> error
            | Ok () ->
                let transferred = Int64.add transferred (Int64.of_int n) in
                notify_progress on_progress transferred;
                loop transferred)
        | exception End_of_file -> Ok ()
      in
      loop 0L

    let of_flow ~content_length ?on_progress flow =
      let write writer =
        try copy_flow_to_writer ?on_progress flow writer
        with exn ->
          target_error_or_escape_callback "read upload flow" "<flow>" exn
      in
      of_stream ~content_length ~replayable:false ~write

    let of_path ?on_progress path =
      let* content_length = regular_file_length path in
      let write writer =
        try
          Eio.Path.with_open_in path (fun file ->
              copy_flow_to_writer ?on_progress file writer)
        with exn -> body_error_or_escape_callback "read upload" path exn
      in
      of_stream ~content_length ~replayable:true ~write
  end

  module Reader = struct
    include S3.Reader

    let copy_reader_to_flow ?on_progress flow reader =
      let bytes = Bytes.create buffer_size in
      let rec loop transferred =
        match
          Runtime.Response_body.read reader bytes ~off:0 ~len:buffer_size
        with
        | Error _ as error -> error
        | Ok 0 -> Ok ()
        | Ok n ->
            Eio.Flow.write flow [ Cstruct.of_bytes ~len:n bytes ];
            let transferred = Int64.add transferred (Int64.of_int n) in
            notify_progress on_progress transferred;
            loop transferred
      in
      loop 0L

    let to_flow ?on_progress flow reader =
      try copy_reader_to_flow ?on_progress flow reader
      with exn ->
        target_error_or_raise_callback "write download flow" "<flow>" exn

    let to_path ?on_progress path reader =
      with_temp_download path (fun _temp_path file ->
          try copy_reader_to_flow ?on_progress file reader
          with exn ->
            body_error_or_preserve_callback "write download" path exn)
  end
end

module Make
    (Runtime : Awskit.Runtime.S with type 'a t = 'a)
    (S3 : sig
      type t

      module Object : sig
        val put :
          t ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Object.Put.options ->
          body:Runtime.request_body ->
          unit ->
          (Awskit_s3.Object.Put.result, Awskit_s3.Error.t) result

        val get :
          t ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Object.Get.options ->
          consume:
            (Runtime.response_body_reader -> ('a, Awskit_s3.Error.t) result) ->
          unit ->
          ('a Awskit_s3.Object.Get.result, Awskit_s3.Error.t) result

        val head :
          t ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Object.Head.options ->
          unit ->
          (Awskit_s3.Object.Head.result, Awskit_s3.Error.t) result
      end

      module Multipart :
        Awskit_s3.MULTIPART
          with type client := t
           and type 'a io := 'a
           and type request_body := Runtime.request_body
    end)
    (Body : sig
      type t = Runtime.request_body

      val of_path :
        ?on_progress:(int64 -> unit) ->
        'a Eio.Path.t ->
        (t, Awskit_s3.Error.t) result
    end)
    (Reader : sig
      type t = Runtime.response_body_reader

      val to_path :
        ?on_progress:(int64 -> unit) ->
        'a Eio.Path.t ->
        t ->
        (unit, Awskit_s3.Error.t) result
    end) =
struct
  module Transfer = Awskit_s3.Transfer
  module Plan = Transfer.Plan

  let notify_transfer_progress callback ~direction ~phase ?total ?part_number
      transferred =
    let progress =
      Transfer.progress ~direction ~phase ~transferred ?total ?part_number ()
    in
    notify_progress callback progress

  let byte_progress_callback callback ~direction ~phase ?total ?part_number () =
    match callback with
    | None -> None
    | Some _ ->
        Some
          (fun transferred ->
            notify_transfer_progress callback ~direction ~phase ?total
              ?part_number transferred)

  let tracked_byte_progress_callback callback ~direction ~phase ?total
      ?part_number transferred_ref =
    Some
      (fun transferred ->
        transferred_ref := transferred;
        notify_transfer_progress callback ~direction ~phase ?total ?part_number
          transferred)

  let range_body_of_path path (spec : Plan.upload_part) =
    let descriptor =
      descriptor ~content_length:(Int64.of_int spec.length) ~replayable:true
    in
    let write writer =
      try
        Eio.Path.with_open_in path (fun file ->
            ignore (Eio.File.seek file (Optint.Int63.of_int64 spec.offset) `Set);
            let cstruct = Cstruct.create buffer_size in
            let rec loop remaining =
              if remaining = 0 then Ok ()
              else
                let len = min buffer_size remaining in
                let read_buffer = Cstruct.sub cstruct 0 len in
                match Eio.Flow.single_read file read_buffer with
                | 0 ->
                    Error
                      (Awskit.Error.Producer.body
                         (Fmt.str
                            "multipart upload file read made no progress while \
                             reading part %d from %a"
                            (Awskit_s3.Multipart.Part_number.to_int
                               spec.part_number)
                            Eio.Path.pp path))
                | n -> (
                    match
                      Runtime.Request_body.write_string writer
                        (Cstruct.to_string ~len:n read_buffer)
                    with
                    | Error _ as error -> error
                    | Ok () -> loop (remaining - n))
                | exception End_of_file ->
                    Error
                      (Awskit.Error.Producer.body
                         (Fmt.str
                            "unexpected end of file while reading part %d from \
                             %a"
                            (Awskit_s3.Multipart.Part_number.to_int
                               spec.part_number)
                            Eio.Path.pp path))
            in
            loop spec.length)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> Error (body_error "read multipart upload" path exn)
    in
    Runtime.Request_body.of_stream descriptor ~write

  let take_batch ~concurrency specs =
    let rec loop remaining acc specs =
      if remaining = 0 then (List.rev acc, specs)
      else
        match specs () with
        | Seq.Nil -> (List.rev acc, Seq.empty)
        | Seq.Cons (spec, rest) -> loop (remaining - 1) (spec :: acc) rest
    in
    loop concurrency [] specs

  type 'a batch_outcome =
    | Batch_result of ('a, Awskit_s3.Error.t) result
    | Batch_raised of exn

  let joined_batch f batch =
    Eio.Switch.run @@ fun sw ->
    batch
    |> List.map (fun item ->
        Eio.Fiber.fork_promise ~sw (fun () ->
            match f item with
            | result -> Batch_result result
            | exception (Eio.Cancel.Cancelled _ as exn) ->
                Eio.Switch.fail sw exn;
                Batch_raised exn
            | exception exn -> Batch_raised exn))
    |> List.map Eio.Promise.await_exn

  let first_batch_error_or_parts outcomes =
    match
      List.find_map
        (function Batch_raised exn -> Some exn | Batch_result _ -> None)
        outcomes
    with
    | Some exn -> raise exn
    | None ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | Batch_result (Error error) :: _ -> Error error
          | Batch_result (Ok part) :: rest -> loop (part :: acc) rest
          | Batch_raised _ :: _ -> assert false
        in
        loop [] outcomes

  let first_batch_error_or_unit outcomes =
    match
      List.find_map
        (function Batch_raised exn -> Some exn | Batch_result _ -> None)
        outcomes
    with
    | Some exn -> raise exn
    | None ->
        let rec loop = function
          | [] -> Ok ()
          | Batch_result (Error error) :: _ -> Error error
          | Batch_result (Ok ()) :: rest -> loop rest
          | Batch_raised _ :: _ -> assert false
        in
        loop outcomes

  let upload_part_from_path conn ~upload ~options ~path
      (spec : Plan.upload_part) =
    let body = range_body_of_path path spec in
    match
      S3.Multipart.upload_part conn ~upload ~part_number:spec.part_number ~body
        ~options:options.Awskit_s3.Transfer.upload_part_options ()
    with
    | Error _ as error -> error
    | Ok uploaded -> Ok uploaded.part

  let upload_missing_parts conn ~upload ~options ~path ?on_progress
      ~content_length specs =
    let completed = ref 0L in
    let upload_one spec =
      match upload_part_from_path conn ~upload ~options ~path spec with
      | Error _ as error -> error
      | Ok part ->
          completed := Int64.add !completed (Int64.of_int spec.length);
          notify_transfer_progress on_progress ~direction:Transfer.Upload
            ~phase:Transfer.Part ~total:content_length
            ~part_number:spec.part_number !completed;
          Ok part
    in
    let rec loop acc specs =
      let batch, rest =
        take_batch ~concurrency:options.Awskit_s3.Transfer.concurrency specs
      in
      match batch with
      | [] -> Ok (List.rev acc)
      | _ ->
          let outcomes = joined_batch upload_one batch in
          let* parts = first_batch_error_or_parts outcomes in
          loop (List.rev_append parts acc) rest
    in
    loop [] specs

  let sort_parts parts =
    List.sort
      (fun (left : Awskit_s3.Multipart.Part.t) right ->
        Int.compare
          (Awskit_s3.Multipart.Part.part_number left
          |> Awskit_s3.Multipart.Part_number.to_int)
          (Awskit_s3.Multipart.Part.part_number right
          |> Awskit_s3.Multipart.Part_number.to_int))
      parts

  let verify_resume_upload conn ~upload ~options =
    match
      S3.Multipart.List_parts.parts conn ~upload
        ~options:options.Awskit_s3.Transfer.list_parts_options ~max_pages:1 ()
    with
    | Error _ as error -> error
    | Ok _parts -> Ok ()

  let complete_multipart conn ~upload ~options ~bytes_transferred parts =
    let parts = sort_parts parts in
    match
      S3.Multipart.complete_upload conn ~upload ~parts
        ~options:options.Awskit_s3.Transfer.complete_options ()
    with
    | Error _ as error -> error
    | Ok complete ->
        Ok
          {
            Awskit_s3.Transfer.upload =
              Awskit_s3.Multipart.Upload.as_caller_owned upload;
            parts;
            complete;
            bytes_transferred;
          }

  let resume_multipart_upload_file conn ~upload ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_upload_options options
    in
    let* () = Awskit_s3.Transfer.validate_upload_options options in
    let* () = Awskit_s3.Transfer.validate_upload_multipart_selection options in
    let* content_length = regular_file_length path in
    let* specs =
      Plan.upload_part_seq ~content_length ~part_size:options.part_size
    in
    let* () = verify_resume_upload conn ~upload ~options in
    let* uploaded_now =
      upload_missing_parts conn ~upload ~options ~path ?on_progress
        ~content_length specs
    in
    complete_multipart conn ~upload ~options ~bytes_transferred:content_length
      uploaded_now

  let multipart_upload_file conn ~bucket ~key ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_upload_options options
    in
    let* () = Awskit_s3.Transfer.validate_upload_options options in
    let* () = Awskit_s3.Transfer.validate_upload_multipart_selection options in
    let* content_length = regular_file_length path in
    let* specs =
      Plan.upload_part_seq ~content_length ~part_size:options.part_size
    in
    let* created =
      S3.Multipart.create_upload conn ~bucket ~key
        ~options:options.create_options ()
    in
    let abort_and_return error =
      match
        S3.Multipart.abort_upload conn ~upload:created.upload
          ?expected_bucket_owner:options.abort_expected_bucket_owner ()
      with
      | Ok _ -> Error error
      | Error cleanup_error ->
          Error
            (Awskit.Error.Producer.multiple [ error; cleanup_error ]
            |> Awskit.Error.Producer.with_context
                 "multipart upload failed and abort also failed")
    in
    let abort_cleanup_ignore_errors () =
      Eio.Cancel.protect (fun () ->
          match
            S3.Multipart.abort_upload conn ~upload:created.upload
              ?expected_bucket_owner:options.abort_expected_bucket_owner ()
          with
          | Ok _ | Error _ -> ()
          | exception _ -> ())
    in
    let abort_then_raise exn =
      abort_cleanup_ignore_errors ();
      raise_callback_or_raise exn
    in
    let upload_and_complete () =
      match
        upload_missing_parts conn ~upload:created.upload ~options ~path
          ?on_progress ~content_length specs
      with
      | Error error -> Error error
      | Ok parts -> (
          match
            complete_multipart conn ~upload:created.upload ~options
              ~bytes_transferred:content_length parts
          with
          | Ok _ as result -> result
          | Error _ as error -> error)
    in
    match upload_and_complete () with
    | exception exn -> abort_then_raise exn
    | Ok _ as result -> result
    | Error error -> abort_and_return error

  let upload_file conn ~bucket ~key ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_upload_options options
    in
    let* () = Awskit_s3.Transfer.validate_upload_options options in
    let* content_length = regular_file_length path in
    if
      Int64.equal content_length 0L
      || Int64.compare content_length options.multipart_threshold < 0
    then
      let upload_progress =
        byte_progress_callback on_progress ~direction:Transfer.Upload
          ~phase:Transfer.Single_request ~total:content_length ()
      in
      let* body = Body.of_path ?on_progress:upload_progress path in
      let* result =
        match
          S3.Object.put conn ~bucket ~key ~options:options.put_options ~body ()
        with
        | exception exn -> raise_escaped_callback_or_raise exn
        | result -> result
      in
      Ok
        (Awskit_s3.Transfer.Put
           { put = result; bytes_transferred = content_length })
    else
      let* () =
        Awskit_s3.Transfer.validate_upload_multipart_selection options
      in
      let* result =
        multipart_upload_file conn ~bucket ~key ~options ?on_progress ~path ()
      in
      Ok (Awskit_s3.Transfer.Multipart result)

  let head_options_of_get_options (options : Awskit_s3.Object.Get.options) :
      Awskit_s3.Object.Head.options =
    {
      preconditions = options.preconditions;
      version_id = options.version_id;
      checksum_mode = options.checksum_mode;
      source_encryption = options.source_encryption;
      expected_bucket_owner = options.expected_bucket_owner;
    }

  let get_info (result : _ Awskit_s3.Object.Get.result) :
      Awskit_s3.Object.Get.info =
    {
      etag = result.etag;
      content_type = result.content_type;
      content_length = result.content_length;
      content_range = result.content_range;
      last_modified = result.last_modified;
      metadata = result.metadata;
      storage_class = result.storage_class;
      version_id = result.version_id;
      checksum = result.checksum;
      encryption = result.encryption;
      response = result.response;
    }

  let ranged_get_options_of_head (info : Awskit_s3.Object.Head.result)
      (get_options : Awskit_s3.Object.Get.options) :
      Awskit_s3.Object.Get.options =
    match info.version_id with
    | Some version_id -> { get_options with version_id = Some version_id }
    | None -> (
        match info.etag with
        | None -> get_options
        | Some etag ->
            let preconditions =
              {
                get_options.preconditions with
                if_match = Some (Awskit_s3.Object.Etag_condition.Etag etag);
              }
            in
            { get_options with preconditions })

  let download_range_to_file conn ~bucket ~key
      ~(get_options : Awskit_s3.Object.Get.options) ~path ~file ~completed
      ~content_length ?on_progress (spec : Plan.download_range) =
    let get_options = { get_options with range = Some spec.range } in
    let consume reader =
      try
        let bytes = Bytes.create buffer_size in
        let rec loop position remaining =
          if remaining = 0 then Ok ()
          else
            let len = min buffer_size remaining in
            match Runtime.Response_body.read reader bytes ~off:0 ~len with
            | Error _ as error -> error
            | Ok 0 ->
                Error
                  (Awskit.Error.Producer.body
                     (Fmt.str
                        "unexpected end of response while downloading range %d"
                        spec.index))
            | Ok n ->
                Eio.File.pwrite_all file
                  ~file_offset:(Optint.Int63.of_int64 position)
                  [ Cstruct.of_bytes ~len:n bytes ];
                completed := Int64.add !completed (Int64.of_int n);
                notify_transfer_progress on_progress
                  ~direction:Transfer.Download ~phase:Transfer.Ranged_get
                  ~total:content_length !completed;
                loop (Int64.add position (Int64.of_int n)) (remaining - n)
        in
        loop spec.offset spec.length
      with exn -> body_error_or_preserve_callback "write download" path exn
    in
    match S3.Object.get conn ~bucket ~key ~options:get_options ~consume () with
    | Error _ as error -> error
    | Ok { value = (); _ } -> Ok ()

  let ranged_download_to_file conn ~bucket ~key
      ~(options : Awskit_s3.Transfer.download_options)
      ~(get_options : Awskit_s3.Object.Get.options) ?on_progress ~path ~file
      ~content_length ranges =
    let completed = ref 0L in
    let download_one spec =
      download_range_to_file conn ~bucket ~key ~get_options ~path ~file
        ~completed ~content_length ?on_progress spec
    in
    let rec loop parts ranges =
      let batch, rest =
        take_batch ~concurrency:options.Awskit_s3.Transfer.concurrency ranges
      in
      match batch with
      | [] -> Ok parts
      | _ ->
          let outcomes = joined_batch download_one batch in
          let* () = first_batch_error_or_unit outcomes in
          loop (parts + List.length batch) rest
    in
    loop 0 ranges

  let download_file conn ~bucket ~key ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_download_options options
    in
    let* () = Awskit_s3.Transfer.validate_download_options options in
    let* () = validate_download_target path options in
    let download_with_get ?total () =
      let bytes_transferred = ref 0L in
      let download_progress =
        tracked_byte_progress_callback on_progress ~direction:Transfer.Download
          ~phase:Transfer.Single_request ?total bytes_transferred
      in
      let* result =
        S3.Object.get conn ~bucket ~key ~options:options.get_options
          ~consume:(Reader.to_path ?on_progress:download_progress path)
          ()
      in
      Ok
        (Awskit_s3.Transfer.Get
           { info = get_info result; bytes_transferred = !bytes_transferred })
    in
    let head_options = head_options_of_get_options options.get_options in
    let* info = S3.Object.head conn ~bucket ~key ~options:head_options () in
    match info.content_length with
    | None -> download_with_get ()
    | Some content_length
      when Int64.compare content_length options.multipart_threshold < 0
           || Int64.equal content_length 0L ->
        download_with_get ~total:content_length ()
    | Some content_length ->
        let* ranges =
          Plan.download_range_seq ~content_length ~part_size:options.part_size
        in
        let get_options = ranged_get_options_of_head info options.get_options in
        with_temp_download path (fun temp_path file ->
            let* parts =
              ranged_download_to_file conn ~bucket ~key ~options ~get_options
                ?on_progress ~path:temp_path ~file ~content_length ranges
            in
            Ok
              (Awskit_s3.Transfer.Ranged
                 { info; parts; bytes_transferred = content_length }))
end
