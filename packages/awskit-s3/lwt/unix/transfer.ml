let buffer_size = 128 * 1024
let temp_counter = Atomic.make 0

let body_error action path exn =
  Awskit.Error.Producer.body
    (Fmt.str "failed to %s path %S: %s" action path (Printexc.to_string exn))

exception Callback_raised of exn

let notify_progress callback bytes =
  match callback with
  | None -> ()
  | Some f -> (
      try f bytes with
      | Lwt.Canceled -> raise Lwt.Canceled
      | Callback_raised _ as exn -> raise exn
      | exn -> raise (Callback_raised exn))

let body_error_or_fail action path = function
  | Lwt.Canceled -> Lwt.fail Lwt.Canceled
  | Callback_raised _ as exn -> Lwt.fail exn
  | exn -> Lwt.return_error (body_error action path exn)

let body_error_or_raise_callback action path = function
  | Callback_raised exn -> Lwt.fail exn
  | exn -> body_error_or_fail action path exn

let body_error_or_escape_callback action path = function
  | Callback_raised exn -> Awskit.Body.Request.raise_escaped_exn exn
  | exn -> body_error_or_fail action path exn

let raise_escaped_callback_or_fail = function
  | exn -> (
      match Awskit.Body.Request.escaped_exn exn with
      | Some escaped -> Lwt.fail escaped
      | None -> Lwt.fail exn)

let raise_callback_or_fail = function
  | Callback_raised exn -> Lwt.fail exn
  | exn -> Lwt.fail exn

let file_kind_to_string = function
  | Unix.S_REG -> "regular file"
  | Unix.S_DIR -> "directory"
  | Unix.S_CHR -> "character device"
  | Unix.S_BLK -> "block device"
  | Unix.S_LNK -> "symbolic link"
  | Unix.S_FIFO -> "fifo"
  | Unix.S_SOCK -> "socket"

let regular_file_length path =
  Lwt.catch
    (fun () ->
      Lwt.bind (Lwt_unix.LargeFile.stat path) (fun stat ->
          match stat.st_kind with
          | Unix.S_REG -> Lwt.return_ok stat.st_size
          | kind ->
              Lwt.return_error
                (Awskit.Error.Producer.validation ~field:"path"
                   (Fmt.str "expected regular file, got %s"
                      (file_kind_to_string kind)))))
    (body_error_or_raise_callback "stat upload" path)

let reject_existing_download_target path =
  Lwt.catch
    (fun () ->
      Lwt.bind (Lwt_unix.LargeFile.stat path) (fun _stat ->
          Lwt.return_error
            (Awskit.Error.Producer.validation ~field:"path"
               "download target already exists")))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok ()
      | exn -> body_error_or_raise_callback "stat download target" path exn)

let validate_download_target path
    (options : Awskit_s3.Transfer.download_options) =
  match options.overwrite with
  | Awskit_s3.Transfer.Replace -> Lwt.return_ok ()
  | Awskit_s3.Transfer.Error_if_exists -> reject_existing_download_target path

let temp_download_path path attempt =
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  let id = Atomic.fetch_and_add temp_counter 1 in
  Filename.concat dir
    (Fmt.str ".%s.awskit-download.%d.%08x.%d.tmp" base (Unix.getpid ()) id
       attempt)

let remove_temp_download path =
  Lwt.catch
    (fun () -> Lwt.bind (Lwt_unix.unlink path) (fun () -> Lwt.return_ok ()))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok ()
      | exn -> body_error_or_raise_callback "remove temporary download" path exn)

let close_temp_download path fd =
  Lwt.catch
    (fun () -> Lwt.bind (Lwt_unix.close fd) (fun () -> Lwt.return_ok ()))
    (body_error_or_raise_callback "close temporary download" path)

let cleanup_temp_download path error =
  Lwt.bind (remove_temp_download path) (function
    | Ok () -> Lwt.return_error error
    | Error cleanup_error ->
        Lwt.return_error
          (Awskit.Error.Producer.multiple [ error; cleanup_error ]
          |> Awskit.Error.Producer.with_context
               "download failed and temporary file cleanup also failed"))

let cleanup_temp_download_with_failures path errors =
  Lwt.bind (remove_temp_download path) (fun cleanup_result ->
      let errors =
        match cleanup_result with
        | Ok () -> errors
        | Error cleanup_error -> errors @ [ cleanup_error ]
      in
      Lwt.return_error
        (Awskit.Error.Producer.multiple errors
        |> Awskit.Error.Producer.with_context
             "download failed and temporary file cleanup also failed"))

let write_all fd bytes offset length =
  let rec loop offset remaining =
    if remaining = 0 then Lwt.return_ok ()
    else
      Lwt.bind (Lwt_unix.write fd bytes offset remaining) (function
        | 0 ->
            Lwt.return_error
              (Awskit.Error.Producer.body "download write made no progress")
        | written -> loop (offset + written) (remaining - written))
  in
  loop offset length

let reserve_temp_download_file path =
  let rec loop attempt =
    if attempt >= 100 then
      Lwt.return_error
        (Awskit.Error.Producer.body
           (Fmt.str "failed to reserve temporary download path for %S" path))
    else
      let temp_path = temp_download_path path attempt in
      Lwt.catch
        (fun () ->
          Lwt.bind
            (Lwt_unix.openfile temp_path
               [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ]
               0o600)
            (fun fd -> Lwt.return_ok (temp_path, fd)))
        (function
          | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (attempt + 1)
          | exn ->
              body_error_or_raise_callback "create temporary download" temp_path
                exn)
  in
  loop 0

let with_temp_download path f =
  Lwt.bind (reserve_temp_download_file path) (function
    | Error _ as error -> Lwt.return error
    | Ok (temp_path, fd) ->
        let close_and_cleanup error =
          Lwt.bind (close_temp_download temp_path fd) (function
            | Error close_error ->
                cleanup_temp_download_with_failures temp_path
                  [ error; close_error ]
            | Ok () -> cleanup_temp_download temp_path error)
        in
        let close_and_publish value =
          Lwt.bind (close_temp_download temp_path fd) (function
            | Error close_error -> cleanup_temp_download temp_path close_error
            | Ok () ->
                Lwt.catch
                  (fun () ->
                    Lwt.bind (Lwt_unix.chmod temp_path 0o600) (fun () ->
                        Lwt.bind (Lwt_unix.rename temp_path path) (fun () ->
                            Lwt.return_ok value)))
                  (fun exn ->
                    match exn with
                    | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                    | Callback_raised callback_exn -> Lwt.fail callback_exn
                    | exn ->
                        cleanup_temp_download temp_path
                          (body_error "rename download" path exn)))
        in
        let close_and_cleanup_then_fail exn =
          Lwt.bind (close_temp_download temp_path fd) (fun _ ->
              Lwt.bind (remove_temp_download temp_path) (fun _ -> Lwt.fail exn))
        in
        Lwt.catch
          (fun () ->
            Lwt.bind (f temp_path fd) (function
              | Error error -> close_and_cleanup error
              | Ok value -> close_and_publish value))
          (function
            | Lwt.Canceled -> close_and_cleanup_then_fail Lwt.Canceled
            | Callback_raised exn -> close_and_cleanup_then_fail exn
            | exn -> close_and_cleanup (body_error "write download" path exn)))

module Make_body_reader
    (Runtime : Awskit.Runtime.S with type 'a t = 'a Lwt.t)
    (S3 : sig
      module Body :
        Awskit_s3.Runtime_adapter.Request_body
          with type 'a io := 'a Lwt.t
           and type t = Runtime.request_body

      module Reader :
        Awskit_s3.Runtime_adapter.Response_reader
          with type 'a io := 'a Lwt.t
           and type t = Runtime.response_body_reader
    end) =
struct
  module Body = struct
    include S3.Body

    let descriptor ~content_length ~replayable =
      Awskit.Body.Request.descriptor_exn ~content_length
        ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable ()

    let copy_channel_to_writer ?on_progress channel writer =
      let bytes = Bytes.create buffer_size in
      let rec loop transferred =
        Lwt.bind (Lwt_io.read_into channel bytes 0 buffer_size) (function
          | 0 -> Lwt.return_ok ()
          | n ->
              Lwt.bind (Writer.write_subbytes writer bytes ~off:0 ~len:n)
                (function
                | Error _ as error -> Lwt.return error
                | Ok () ->
                    let transferred = Int64.add transferred (Int64.of_int n) in
                    notify_progress on_progress transferred;
                    loop transferred))
      in
      loop 0L

    let of_lwt_stream ~content_length stream =
      let write writer =
        Lwt.catch
          (fun () ->
            let rec loop () =
              Lwt.bind (Lwt_stream.get stream) (function
                | None -> Lwt.return_ok ()
                | Some chunk ->
                    Lwt.bind (Writer.write_string writer chunk) (function
                      | Error _ as error -> Lwt.return error
                      | Ok () -> loop ()))
            in
            loop ())
          (body_error_or_raise_callback "read upload stream" "<stream>")
      in
      of_stream ~content_length ~replayable:false ~write

    let of_channel ~content_length ?on_progress channel =
      let write writer =
        Lwt.catch
          (fun () -> copy_channel_to_writer ?on_progress channel writer)
          (body_error_or_escape_callback "read upload channel" "<channel>")
      in
      of_stream ~content_length ~replayable:false ~write

    let of_path ?on_progress path =
      Lwt.bind (regular_file_length path) (function
        | Error _ as error -> Lwt.return error
        | Ok content_length ->
            let write writer =
              Lwt.catch
                (fun () ->
                  Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel ->
                      copy_channel_to_writer ?on_progress channel writer))
                (body_error_or_escape_callback "read upload" path)
            in
            Lwt.return (of_stream ~content_length ~replayable:true ~write))
  end

  module Reader = struct
    include S3.Reader

    let copy_reader_to_channel ?on_progress channel reader =
      let bytes = Bytes.create buffer_size in
      let rec loop transferred =
        Lwt.bind
          (Runtime.Response_body.read reader bytes ~off:0 ~len:buffer_size)
          (function
          | Error _ as error -> Lwt.return error
          | Ok 0 -> Lwt.return_ok ()
          | Ok n ->
              Lwt.bind (Lwt_io.write_from_exactly channel bytes 0 n) (fun () ->
                  let transferred = Int64.add transferred (Int64.of_int n) in
                  notify_progress on_progress transferred;
                  loop transferred))
      in
      loop 0L

    let copy_reader_to_fd ?on_progress fd reader =
      let bytes = Bytes.create buffer_size in
      let rec loop transferred =
        Lwt.bind
          (Runtime.Response_body.read reader bytes ~off:0 ~len:buffer_size)
          (function
          | Error _ as error -> Lwt.return error
          | Ok 0 -> Lwt.return_ok ()
          | Ok n ->
              Lwt.bind (write_all fd bytes 0 n) (function
                | Error _ as error -> Lwt.return error
                | Ok () ->
                    let transferred = Int64.add transferred (Int64.of_int n) in
                    notify_progress on_progress transferred;
                    loop transferred))
      in
      loop 0L

    let to_channel ?on_progress channel reader =
      Lwt.catch
        (fun () -> copy_reader_to_channel ?on_progress channel reader)
        (body_error_or_raise_callback "write download channel" "<channel>")

    let to_path ?on_progress path reader =
      with_temp_download path (fun _temp_path fd ->
          Lwt.catch
            (fun () -> copy_reader_to_fd ?on_progress fd reader)
            (body_error_or_fail "write download" path))
  end
end

module Make
    (Runtime : Awskit.Runtime.S with type 'a t = 'a Lwt.t)
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
          (Awskit_s3.Object.Put.result, Awskit_s3.Error.t) result Lwt.t

        val get :
          t ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Object.Get.options ->
          consume:
            (Runtime.response_body_reader ->
            ('a, Awskit_s3.Error.t) result Lwt.t) ->
          unit ->
          ('a Awskit_s3.Object.Get.result, Awskit_s3.Error.t) result Lwt.t

        val head :
          t ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Object.Head.options ->
          unit ->
          (Awskit_s3.Object.Head.result, Awskit_s3.Error.t) result Lwt.t
      end

      module Multipart :
        Awskit_s3.Runtime_adapter.Multipart_operations
          with type client := t
           and type 'a io := 'a Lwt.t
           and type request_body := Runtime.request_body
    end)
    (Body : sig
      type t = Runtime.request_body

      val of_path :
        ?on_progress:(int64 -> unit) ->
        string ->
        (t, Awskit_s3.Error.t) result Lwt.t
    end)
    (Reader : sig
      type t = Runtime.response_body_reader

      val to_path :
        ?on_progress:(int64 -> unit) ->
        string ->
        t ->
        (unit, Awskit_s3.Error.t) result Lwt.t
    end) =
struct
  let ( let* ) result f =
    Lwt.bind result (function
      | Ok value -> f value
      | Error _ as error -> Lwt.return error)

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
      Awskit.Body.Request.descriptor_exn
        ~content_length:(Int64.of_int spec.length)
        ~payload_hash:Awskit.Body.Payload_hash.unsigned_payload ~replayable:true
        ()
    in
    let write writer =
      Lwt.catch
        (fun () ->
          Lwt.bind (Lwt_unix.openfile path [ Unix.O_RDONLY ] 0) (fun fd ->
              Lwt.finalize
                (fun () ->
                  Lwt.bind
                    (Lwt_unix.LargeFile.lseek fd spec.offset Unix.SEEK_SET)
                    (fun _ ->
                      let bytes = Bytes.create buffer_size in
                      let rec loop remaining =
                        if remaining = 0 then Lwt.return_ok ()
                        else
                          let len = min buffer_size remaining in
                          Lwt.bind (Lwt_unix.read fd bytes 0 len) (function
                            | 0 ->
                                Lwt.return_error
                                  (Awskit.Error.Producer.body
                                     (Fmt.str
                                        "unexpected end of file while reading \
                                         part %d from %S"
                                        (Awskit_s3.Multipart.Part_number.to_int
                                           spec.part_number)
                                        path))
                            | n ->
                                Lwt.bind
                                  (Runtime.Request_body.write_subbytes writer
                                     bytes ~off:0 ~len:n) (function
                                  | Error _ as error -> Lwt.return error
                                  | Ok () -> loop (remaining - n)))
                      in
                      loop spec.length))
                (fun () -> Lwt_unix.close fd)))
        (body_error_or_raise_callback "read multipart upload" path)
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
    let jobs =
      batch
      |> List.map (fun item ->
          let promise = try f item with exn -> Lwt.fail exn in
          let outcome =
            Lwt.catch
              (fun () -> Lwt.map (fun result -> Batch_result result) promise)
              (function
                | Lwt.Canceled -> Lwt.fail Lwt.Canceled
                | exn -> Lwt.return (Batch_raised exn))
          in
          (promise, outcome))
    in
    let outcomes = List.map snd jobs in
    let cancel_jobs () =
      List.iter
        (fun (promise, outcome) ->
          Lwt.cancel promise;
          Lwt.cancel outcome)
        jobs
    in
    let cancellation =
      let never_resolved () =
        let promise, _wakener = Lwt.task () in
        promise
      in
      outcomes
      |> List.map (fun outcome ->
          Lwt.catch
            (fun () -> Lwt.bind outcome (fun _ -> never_resolved ()))
            (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn))
      |> Lwt.pick
    in
    Lwt.finalize
      (fun () ->
        Lwt.bind
          (Lwt.pick
             [
               Lwt.map (fun outcomes -> `Outcomes outcomes) (Lwt.all outcomes);
               Lwt.map (fun () -> `Canceled) cancellation;
             ])
          (function
            | `Outcomes outcomes -> Lwt.return outcomes
            | `Canceled ->
                cancel_jobs ();
                Lwt.fail Lwt.Canceled))
      (fun () ->
        Lwt.cancel cancellation;
        Lwt.return_unit)

  let first_batch_error_or_parts outcomes =
    match
      List.find_map
        (function Batch_raised exn -> Some exn | Batch_result _ -> None)
        outcomes
    with
    | Some exn -> Lwt.fail exn
    | None ->
        let rec loop acc = function
          | [] -> Lwt.return_ok (List.rev acc)
          | Batch_result (Error error) :: _ -> Lwt.return_error error
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
    | Some exn -> Lwt.fail exn
    | None ->
        let rec loop = function
          | [] -> Lwt.return_ok ()
          | Batch_result (Error error) :: _ -> Lwt.return_error error
          | Batch_result (Ok ()) :: rest -> loop rest
          | Batch_raised _ :: _ -> assert false
        in
        loop outcomes

  let upload_part_from_path conn ~upload ~options ~path
      (spec : Plan.upload_part) =
    let body = range_body_of_path path spec in
    Lwt.bind
      (S3.Multipart.upload_part conn ~upload ~part_number:spec.part_number ~body
         ~options:options.Awskit_s3.Transfer.upload_part_options ()) (function
      | Error _ as error -> Lwt.return error
      | Ok uploaded -> Lwt.return_ok uploaded.part)

  let upload_missing_parts conn ~upload ~options ~path ?on_progress
      ~content_length specs =
    Lwt.catch
      (fun () ->
        let completed = ref 0L in
        let upload_one spec =
          Lwt.bind (upload_part_from_path conn ~upload ~options ~path spec)
            (function
            | Error _ as error -> Lwt.return error
            | Ok part ->
                completed := Int64.add !completed (Int64.of_int spec.length);
                notify_transfer_progress on_progress ~direction:Transfer.Upload
                  ~phase:Transfer.Part ~total:content_length
                  ~part_number:spec.part_number !completed;
                Lwt.return_ok part)
        in
        let rec loop acc specs =
          let batch, rest =
            take_batch ~concurrency:options.Awskit_s3.Transfer.concurrency specs
          in
          match batch with
          | [] -> Lwt.return_ok (List.rev acc)
          | _ ->
              Lwt.bind (joined_batch upload_one batch) (fun outcomes ->
                  Lwt.bind (first_batch_error_or_parts outcomes) (function
                    | Error _ as error -> Lwt.return error
                    | Ok parts -> loop (List.rev_append parts acc) rest))
        in
        loop [] specs)
      raise_callback_or_fail

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
    Lwt.bind
      (S3.Multipart.List_parts.parts conn ~upload
         ~options:options.Awskit_s3.Transfer.list_parts_options ~max_pages:1 ())
      (function
      | Error _ as error -> Lwt.return error
      | Ok _parts -> Lwt.return_ok ())

  let complete_multipart conn ~upload ~options ~bytes_transferred parts =
    let parts = sort_parts parts in
    match
      Awskit_s3.Multipart.Complete.Parts.of_list
        ~multipart_object_size:bytes_transferred parts
    with
    | Error _ as error -> Lwt.return error
    | Ok completion_parts ->
        Lwt.bind
          (S3.Multipart.complete_upload conn ~upload ~parts:completion_parts
             ~options:options.Awskit_s3.Transfer.complete_options ()) (function
          | Error _ as error -> Lwt.return error
          | Ok complete ->
              Lwt.return_ok
                {
                  Awskit_s3.Transfer.upload =
                    Awskit_s3.Multipart.Upload.as_caller_owned upload;
                  parts;
                  complete;
                  bytes_transferred;
                })

  let resume_multipart_upload_file conn ~upload ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_upload_options options
    in
    let* () = Lwt.return (Awskit_s3.Transfer.validate_upload_options options) in
    let* () =
      Lwt.return
        (Awskit_s3.Transfer.validate_upload_multipart_selection options)
    in
    let* content_length = regular_file_length path in
    let* specs =
      Lwt.return
        (Plan.upload_part_seq ~content_length ~part_size:options.part_size)
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
    let* () = Lwt.return (Awskit_s3.Transfer.validate_upload_options options) in
    let* () =
      Lwt.return
        (Awskit_s3.Transfer.validate_upload_multipart_selection options)
    in
    let* content_length = regular_file_length path in
    let* specs =
      Lwt.return
        (Plan.upload_part_seq ~content_length ~part_size:options.part_size)
    in
    let* created =
      S3.Multipart.create_upload conn ~bucket ~key
        ~options:options.create_options ()
    in
    let abort_and_return error =
      Lwt.bind
        (S3.Multipart.abort_upload conn ~upload:created.upload
           ?expected_bucket_owner:options.abort_expected_bucket_owner ())
        (function
        | Ok _ -> Lwt.return_error error
        | Error cleanup_error ->
            Lwt.return_error
              (Awskit.Error.Producer.multiple [ error; cleanup_error ]
              |> Awskit.Error.Producer.with_context
                   "multipart upload failed and abort also failed"))
    in
    let abort_cleanup_ignore_errors () =
      Lwt.catch
        (fun () ->
          Lwt.bind
            (Lwt.protected
               (S3.Multipart.abort_upload conn ~upload:created.upload
                  ?expected_bucket_owner:options.abort_expected_bucket_owner ()))
            (fun _ -> Lwt.return_unit))
        (fun _exn -> Lwt.return_unit)
    in
    let abort_then_fail exn =
      Lwt.bind (abort_cleanup_ignore_errors ()) (fun () -> Lwt.fail exn)
    in
    let upload_and_complete () =
      Lwt.bind
        (upload_missing_parts conn ~upload:created.upload ~options ~path
           ?on_progress ~content_length specs) (function
        | Error error -> abort_and_return error
        | Ok parts ->
            Lwt.bind
              (complete_multipart conn ~upload:created.upload ~options
                 ~bytes_transferred:content_length parts) (function
              | Ok _ as result -> Lwt.return result
              | Error error -> abort_and_return error))
    in
    Lwt.catch upload_and_complete abort_then_fail

  let upload_file conn ~bucket ~key ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_upload_options options
    in
    match Awskit_s3.Transfer.validate_upload_options options with
    | Error _ as error -> Lwt.return error
    | Ok () ->
        Lwt.bind (regular_file_length path) (function
          | Error _ as error -> Lwt.return error
          | Ok content_length -> (
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
                  Lwt.catch
                    (fun () ->
                      S3.Object.put conn ~bucket ~key
                        ~options:options.put_options ~body ())
                    raise_escaped_callback_or_fail
                in
                Lwt.return_ok
                  (Awskit_s3.Transfer.Put
                     { put = result; bytes_transferred = content_length })
              else
                match
                  Awskit_s3.Transfer.validate_upload_multipart_selection options
                with
                | Error _ as error -> Lwt.return error
                | Ok () ->
                    Lwt.bind
                      (multipart_upload_file conn ~bucket ~key ~options
                         ?on_progress ~path ()) (function
                      | Error _ as error -> Lwt.return error
                      | Ok result ->
                          Lwt.return_ok (Awskit_s3.Transfer.Multipart result))))

  let head_options_of_get_options (options : Awskit_s3.Object.Get.options) :
      Awskit_s3.Object.Head.options =
    {
      preconditions = options.preconditions;
      version_id = options.version_id;
      checksum_mode = options.checksum_mode;
      source_encryption = options.source_encryption;
      expected_bucket_owner = options.expected_bucket_owner;
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

  let download_range_to_fd conn ~bucket ~key
      ~(get_options : Awskit_s3.Object.Get.options) ~path ~fd ~write_mutex
      ~completed ~content_length ?on_progress (spec : Plan.download_range) =
    let get_options = { get_options with range = Some spec.range } in
    let consume reader =
      Lwt.catch
        (fun () ->
          let bytes = Bytes.create buffer_size in
          let rec loop position remaining =
            if remaining = 0 then Lwt.return_ok ()
            else
              let len = min buffer_size remaining in
              Lwt.bind (Runtime.Response_body.read reader bytes ~off:0 ~len)
                (function
                | Error _ as error -> Lwt.return error
                | Ok 0 ->
                    Lwt.return_error
                      (Awskit.Error.Producer.body
                         (Fmt.str
                            "unexpected end of response while downloading \
                             range %d"
                            spec.index))
                | Ok n ->
                    Lwt.bind
                      (Lwt_mutex.with_lock write_mutex (fun () ->
                           Lwt.bind
                             (Lwt_unix.LargeFile.lseek fd position Unix.SEEK_SET)
                             (fun _ -> write_all fd bytes 0 n)))
                      (function
                        | Error _ as error -> Lwt.return error
                        | Ok () ->
                            completed := Int64.add !completed (Int64.of_int n);
                            notify_transfer_progress on_progress
                              ~direction:Transfer.Download
                              ~phase:Transfer.Ranged_get ~total:content_length
                              !completed;
                            loop
                              (Int64.add position (Int64.of_int n))
                              (remaining - n)))
          in
          loop spec.offset spec.length)
        (body_error_or_fail "write download" path)
    in
    Lwt.bind (S3.Object.get conn ~bucket ~key ~options:get_options ~consume ())
      (function
      | Error _ as error -> Lwt.return error
      | Ok { value = (); _ } -> Lwt.return_ok ())

  let ranged_download_to_fd conn ~bucket ~key
      ~(options : Awskit_s3.Transfer.download_options)
      ~(get_options : Awskit_s3.Object.Get.options) ?on_progress ~path ~fd
      ~content_length ranges =
    let completed = ref 0L in
    let write_mutex = Lwt_mutex.create () in
    let download_one spec =
      download_range_to_fd conn ~bucket ~key ~get_options ~path ~fd ~write_mutex
        ~completed ~content_length ?on_progress spec
    in
    let rec loop parts ranges =
      let batch, rest = take_batch ~concurrency:options.concurrency ranges in
      match batch with
      | [] -> Lwt.return_ok parts
      | _ ->
          Lwt.bind (joined_batch download_one batch) (fun outcomes ->
              Lwt.bind (first_batch_error_or_unit outcomes) (function
                | Error _ as error -> Lwt.return error
                | Ok () -> loop (parts + List.length batch) rest))
    in
    loop 0 ranges

  let download_file conn ~bucket ~key ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_download_options options
    in
    let* () =
      Lwt.return (Awskit_s3.Transfer.validate_download_options options)
    in
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
      Lwt.return_ok
        (Awskit_s3.Transfer.Get
           { info = result.info; bytes_transferred = !bytes_transferred })
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
          Lwt.return
            (Plan.download_range_seq ~content_length
               ~part_size:options.part_size)
        in
        let get_options = ranged_get_options_of_head info options.get_options in
        with_temp_download path (fun temp_path fd ->
            let* parts =
              ranged_download_to_fd conn ~bucket ~key ~options ~get_options
                ?on_progress ~path:temp_path ~fd ~content_length ranges
            in
            Lwt.return_ok
              (Awskit_s3.Transfer.Ranged
                 { info; parts; bytes_transferred = content_length }))
end
