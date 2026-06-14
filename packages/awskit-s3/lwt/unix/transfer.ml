let buffer_size = 128 * 1024

let body_error action path exn =
  Awskit.Error.body
    (Fmt.str "failed to %s path %S: %s" action path (Printexc.to_string exn))

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
                (Awskit.Error.validation ~field:"path"
                   (Fmt.str "expected regular file, got %s"
                      (file_kind_to_string kind)))))
    (fun exn -> Lwt.return_error (body_error "stat upload" path exn))

let temp_download_path path attempt =
  let dir = Filename.dirname path in
  let base = Filename.basename path in
  Filename.concat dir
    (Fmt.str ".%s.awskit-download.%d.%d.tmp" base (Unix.getpid ()) attempt)

let remove_temp_download path =
  Lwt.catch
    (fun () -> Lwt.bind (Lwt_unix.unlink path) (fun () -> Lwt.return_ok ()))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_ok ()
      | exn ->
          Lwt.return_error (body_error "remove temporary download" path exn))

let close_temp_download path fd =
  Lwt.catch
    (fun () -> Lwt.bind (Lwt_unix.close fd) (fun () -> Lwt.return_ok ()))
    (fun exn ->
      Lwt.return_error (body_error "close temporary download" path exn))

let cleanup_temp_download path error =
  Lwt.bind (remove_temp_download path) (function
    | Ok () -> Lwt.return_error error
    | Error cleanup_error ->
        Lwt.return_error
          (Awskit.Error.body
             (Fmt.str "%a; additionally %a" Awskit.Error.pp error
                Awskit.Error.pp cleanup_error)))

let write_all fd bytes offset length =
  let rec loop offset remaining =
    if remaining = 0 then Lwt.return_ok ()
    else
      Lwt.bind (Lwt_unix.write fd bytes offset remaining) (function
        | 0 ->
            Lwt.return_error
              (Awskit.Error.body "download write made no progress")
        | written -> loop (offset + written) (remaining - written))
  in
  loop offset length

let reserve_temp_download_file path =
  let rec loop attempt =
    if attempt >= 100 then
      Lwt.return_error
        (Awskit.Error.body
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
              Lwt.return_error
                (body_error "create temporary download" temp_path exn))
  in
  loop 0

let with_temp_download path f =
  Lwt.bind (reserve_temp_download_file path) (function
    | Error _ as error -> Lwt.return error
    | Ok (temp_path, fd) ->
        let close_and_cleanup error =
          Lwt.bind (close_temp_download temp_path fd) (function
            | Error close_error -> cleanup_temp_download temp_path close_error
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
                    cleanup_temp_download temp_path
                      (body_error "rename download" path exn)))
        in
        Lwt.catch
          (fun () ->
            Lwt.bind (f temp_path fd) (function
              | Error error -> close_and_cleanup error
              | Ok value -> close_and_publish value))
          (fun exn -> close_and_cleanup (body_error "write download" path exn)))

module Make_body_reader
    (Runtime : Awskit_s3.RUNTIME with type 'a t = 'a Lwt.t)
    (S3 : sig
      module Body :
        Awskit_s3.BODY
          with type 'a io := 'a Lwt.t
           and type t = Runtime.request_body

      module Reader :
        Awskit_s3.READER
          with type 'a io := 'a Lwt.t
           and type t = Runtime.response_body_reader
    end) =
struct
  module Body = struct
    include S3.Body

    let descriptor ~content_length ~replayable =
      {
        Awskit.Body.Request.content_length = Some content_length;
        payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
        replayable;
      }

    let copy_channel_to_writer ?on_progress channel writer =
      let bytes = Bytes.create buffer_size in
      let rec loop transferred =
        Lwt.bind (Lwt_io.read_into channel bytes 0 buffer_size) (function
          | 0 -> Lwt.return_ok ()
          | n ->
              let chunk = Bytes.sub_string bytes 0 n in
              Lwt.bind (Runtime.Request_body.write_string writer chunk)
                (function
                | Error _ as error -> Lwt.return error
                | Ok () ->
                    let transferred = Int64.add transferred (Int64.of_int n) in
                    Option.iter (fun f -> f transferred) on_progress;
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
                    Lwt.bind (Runtime.Request_body.write_string writer chunk)
                      (function
                      | Error _ as error -> Lwt.return error
                      | Ok () -> loop ()))
            in
            loop ())
          (fun exn ->
            Lwt.return_error (body_error "read upload stream" "<stream>" exn))
      in
      Runtime.Request_body.of_stream
        (descriptor ~content_length ~replayable:false)
        ~write

    let of_channel ~content_length ?on_progress channel =
      let write writer =
        Lwt.catch
          (fun () -> copy_channel_to_writer ?on_progress channel writer)
          (fun exn ->
            Lwt.return_error (body_error "read upload channel" "<channel>" exn))
      in
      Runtime.Request_body.of_stream
        (descriptor ~content_length ~replayable:false)
        ~write

    let of_path ?on_progress path =
      Lwt.bind (regular_file_length path) (function
        | Error _ as error -> Lwt.return error
        | Ok content_length ->
            let write writer =
              Lwt.catch
                (fun () ->
                  Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel ->
                      copy_channel_to_writer ?on_progress channel writer))
                (fun exn ->
                  Lwt.return_error (body_error "read upload" path exn))
            in
            Lwt.return_ok
              (Runtime.Request_body.of_stream
                 (descriptor ~content_length ~replayable:true)
                 ~write))
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
                  Option.iter (fun f -> f transferred) on_progress;
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
                    Option.iter (fun f -> f transferred) on_progress;
                    loop transferred))
      in
      loop 0L

    let to_channel ?on_progress channel reader =
      Lwt.catch
        (fun () -> copy_reader_to_channel ?on_progress channel reader)
        (fun exn ->
          Lwt.return_error (body_error "write download channel" "<channel>" exn))

    let to_path ?on_progress path reader =
      with_temp_download path (fun _temp_path fd ->
          Lwt.catch
            (fun () -> copy_reader_to_fd ?on_progress fd reader)
            (fun exn -> Lwt.return_error (body_error "write download" path exn)))
  end
end

module Make
    (Runtime : Awskit_s3.RUNTIME with type 'a t = 'a Lwt.t)
    (S3 : sig
      module Object : sig
        val put :
          Runtime.connection ->
          bucket:string ->
          key:string ->
          ?options:Awskit_s3.Put_object.options ->
          body:Runtime.request_body ->
          unit ->
          (Awskit_s3.Put_object.result, Awskit_s3.Error.t) result Lwt.t

        val get :
          Runtime.connection ->
          bucket:string ->
          key:string ->
          ?options:Awskit_s3.Get_object.options ->
          consume:
            (Runtime.response_body_reader ->
            ('a, Awskit_s3.Error.t) result Lwt.t) ->
          unit ->
          (Awskit_s3.Get_object.result * 'a, Awskit_s3.Error.t) result Lwt.t

        val head :
          Runtime.connection ->
          bucket:string ->
          key:string ->
          ?options:Awskit_s3.Head_object.options ->
          unit ->
          (Awskit_s3.Head_object.result, Awskit_s3.Error.t) result Lwt.t
      end

      module Multipart :
        Awskit_s3.MULTIPART
          with type connection := Runtime.connection
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
  type part_spec = { part_number : int; offset : int64; length : int }
  type range_spec = { index : int; offset : int64; length : int }

  let ( let* ) result f =
    Lwt.bind result (function
      | Ok value -> f value
      | Error _ as error -> Lwt.return error)

  let bounded_specs ~content_length ~part_size ~empty_error ~make =
    if Int64.equal content_length 0L then
      match empty_error with None -> Ok [] | Some error -> Error error
    else
      let rec loop index offset acc =
        if Int64.compare offset content_length >= 0 then Ok (List.rev acc)
        else
          let remaining = Int64.sub content_length offset in
          let length = min part_size (Int64.to_int remaining) in
          loop (index + 1)
            (Int64.add offset (Int64.of_int length))
            (make index offset length :: acc)
      in
      loop 1 0L []

  let multipart_specs ~content_length ~part_size =
    let empty_error =
      Awskit.Error.validation ~field:"path"
        "multipart file upload requires a non-empty file"
    in
    match
      Awskit_s3.Transfer.validate_multipart_part_count ~content_length
        ~part_size
    with
    | Error _ as error -> error
    | Ok () ->
        bounded_specs ~content_length ~part_size ~empty_error:(Some empty_error)
          ~make:(fun part_number offset length ->
            { part_number; offset; length })

  let range_specs ~content_length ~part_size =
    match
      Awskit_s3.Transfer.validate_multipart_part_count ~content_length
        ~part_size
    with
    | Error _ as error -> error
    | Ok () ->
        bounded_specs ~content_length ~part_size ~empty_error:None
          ~make:(fun index offset length -> { index; offset; length })

  let range_body_of_path path (spec : part_spec) =
    let descriptor =
      {
        Awskit.Body.Request.content_length = Some (Int64.of_int spec.length);
        payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
        replayable = true;
      }
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
                                  (Awskit.Error.body
                                     (Fmt.str
                                        "unexpected end of file while reading \
                                         part %d from %S"
                                        spec.part_number path))
                            | n ->
                                let chunk = Bytes.sub_string bytes 0 n in
                                Lwt.bind
                                  (Runtime.Request_body.write_string writer
                                     chunk) (function
                                  | Error _ as error -> Lwt.return error
                                  | Ok () -> loop (remaining - n)))
                      in
                      loop spec.length))
                (fun () -> Lwt_unix.close fd)))
        (fun exn ->
          Lwt.return_error (body_error "read multipart upload" path exn))
    in
    Runtime.Request_body.of_stream descriptor ~write

  let split_batch ~concurrency specs =
    let rec loop remaining acc = function
      | rest when remaining = 0 -> (List.rev acc, rest)
      | [] -> (List.rev acc, [])
      | spec :: rest -> loop (remaining - 1) (spec :: acc) rest
    in
    loop concurrency [] specs

  let first_error_or_parts results =
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | Error error :: _ -> Error error
      | Ok part :: rest -> loop (part :: acc) rest
    in
    loop [] results

  let first_error_or_unit results =
    let rec loop = function
      | [] -> Ok ()
      | Error error :: _ -> Error error
      | Ok () :: rest -> loop rest
    in
    loop results

  let upload_part_from_path conn ~bucket ~key ~upload_id ~options ~path
      (spec : part_spec) =
    let body = range_body_of_path path spec in
    Lwt.bind
      (S3.Multipart.upload_part conn ~bucket ~key ~upload_id
         ~part_number:spec.part_number ~body
         ~options:options.Awskit_s3.Transfer.upload_part_options ()) (function
      | Error _ as error -> Lwt.return error
      | Ok uploaded -> Lwt.return_ok uploaded.part)

  let upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
      ?on_progress ~initial_completed specs =
    let completed = ref initial_completed in
    Option.iter
      (fun f -> if Int64.compare !completed 0L > 0 then f !completed)
      on_progress;
    let upload_one spec =
      Lwt.bind
        (upload_part_from_path conn ~bucket ~key ~upload_id ~options ~path spec)
        (function
        | Error _ as error -> Lwt.return error
        | Ok part ->
            completed := Int64.add !completed (Int64.of_int spec.length);
            Option.iter (fun f -> f !completed) on_progress;
            Lwt.return_ok part)
    in
    let rec loop acc specs =
      match specs with
      | [] -> Lwt.return_ok (List.rev acc)
      | _ ->
          let batch, rest =
            split_batch ~concurrency:options.Awskit_s3.Transfer.concurrency
              specs
          in
          Lwt.bind (Lwt_list.map_p upload_one batch) (fun results ->
              match first_error_or_parts results with
              | Error _ as error -> Lwt.return error
              | Ok parts -> loop (List.rev_append parts acc) rest)
    in
    loop [] specs

  let sort_parts parts =
    List.sort
      (fun (left : Awskit_s3.Multipart.Part.t) right ->
        compare left.part_number right.part_number)
      parts

  let completed_bytes specs parts =
    parts
    |> List.fold_left
         (fun total (part : Awskit_s3.Multipart.Part.t) ->
           match
             List.find_opt
               (fun spec -> spec.part_number = part.part_number)
               specs
           with
           | None -> total
           | Some spec -> Int64.add total (Int64.of_int spec.length))
         0L

  let matching_uploaded_parts conn ~bucket ~key ~upload_id ~options specs =
    Lwt.bind
      (S3.Multipart.List_parts.parts conn ~bucket ~key ~upload_id
         ~options:options.Awskit_s3.Transfer.list_parts_options ()) (function
      | Error _ as error -> Lwt.return error
      | Ok uploaded ->
          let part_for_spec spec =
            match
              List.find_opt
                (fun (part : Awskit_s3.List_parts.part_info) ->
                  part.part_number = spec.part_number)
                uploaded
            with
            | None -> None
            | Some part -> (
                match (part.etag, part.size) with
                | Some etag, Some size
                  when Int64.equal size (Int64.of_int spec.length) ->
                    Awskit_s3.Multipart.Part.create
                      ~part_number:spec.part_number ~etag ()
                    |> Result.to_option
                | _ -> None)
          in
          Lwt.return_ok (List.filter_map part_for_spec specs))

  let remaining_specs specs uploaded_parts =
    specs
    |> List.filter (fun spec ->
        not
          (List.exists
             (fun (part : Awskit_s3.Multipart.Part.t) ->
               part.part_number = spec.part_number)
             uploaded_parts))

  let complete_multipart conn ~bucket ~key ~upload_id ~options upload parts =
    let parts = sort_parts parts in
    Lwt.bind
      (S3.Multipart.complete_upload conn ~bucket ~key ~upload_id parts
         ~options:options.Awskit_s3.Transfer.complete_options) (function
      | Error _ as error -> Lwt.return error
      | Ok complete ->
          Lwt.return_ok { Awskit_s3.Transfer.upload; parts; complete })

  let resume_multipart_upload_file conn ~bucket ~key ~upload_id ?options
      ?on_progress ~path () =
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
      Lwt.return (multipart_specs ~content_length ~part_size:options.part_size)
    in
    let* upload =
      Lwt.return (Awskit_s3.Multipart.Upload.create ~bucket ~key ~upload_id)
    in
    let* uploaded_parts =
      matching_uploaded_parts conn ~bucket ~key ~upload_id ~options specs
    in
    let initial_completed = completed_bytes specs uploaded_parts in
    let missing = remaining_specs specs uploaded_parts in
    let* uploaded_now =
      upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
        ?on_progress ~initial_completed missing
    in
    complete_multipart conn ~bucket ~key ~upload_id ~options upload
      (uploaded_parts @ uploaded_now)

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
      Lwt.return (multipart_specs ~content_length ~part_size:options.part_size)
    in
    let* created =
      S3.Multipart.create_upload conn ~bucket ~key
        ~options:options.create_options ()
    in
    let upload_id = created.upload.upload_id in
    let abort_and_return error =
      Lwt.bind
        (S3.Multipart.abort_upload conn ~bucket ~key ~upload_id
           ~options:options.abort_options ()) (fun _ -> Lwt.return_error error)
    in
    Lwt.bind
      (upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
         ?on_progress ~initial_completed:0L specs) (function
      | Error error -> abort_and_return error
      | Ok parts ->
          Lwt.bind
            (complete_multipart conn ~bucket ~key ~upload_id ~options
               created.upload parts) (function
            | Ok _ as result -> Lwt.return result
            | Error error -> abort_and_return error))

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
              if Int64.compare content_length options.multipart_threshold < 0
              then
                let* body = Body.of_path ?on_progress path in
                let* result =
                  S3.Object.put conn ~bucket ~key ~options:options.put_options
                    ~body ()
                in
                Lwt.return_ok (Awskit_s3.Transfer.Put result)
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

  let head_options_of_get_options (options : Awskit_s3.Get_object.options) :
      Awskit_s3.Head_object.options =
    {
      preconditions = options.preconditions;
      version_id = options.version_id;
      checksum_mode = options.checksum_mode;
      expected_bucket_owner = options.expected_bucket_owner;
    }

  let download_range_to_fd conn ~bucket ~key ~options ~path ~fd ~write_mutex
      ~completed ?on_progress spec =
    let finish =
      Int64.add spec.offset (Int64.of_int spec.length) |> Int64.pred
    in
    let range = Awskit_s3.Range.bytes_exn ~start:spec.offset ~finish in
    let get_options =
      { options.Awskit_s3.Transfer.get_options with range = Some range }
    in
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
                      (Awskit.Error.body
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
                            Option.iter (fun f -> f !completed) on_progress;
                            loop
                              (Int64.add position (Int64.of_int n))
                              (remaining - n)))
          in
          loop spec.offset spec.length)
        (fun exn -> Lwt.return_error (body_error "write download" path exn))
    in
    Lwt.bind (S3.Object.get conn ~bucket ~key ~options:get_options ~consume ())
      (function
      | Error _ as error -> Lwt.return error
      | Ok (_, ()) -> Lwt.return_ok ())

  let ranged_download_to_fd conn ~bucket ~key ~options ?on_progress ~path ~fd
      ranges =
    let completed = ref 0L in
    let write_mutex = Lwt_mutex.create () in
    let download_one spec =
      download_range_to_fd conn ~bucket ~key ~options ~path ~fd ~write_mutex
        ~completed ?on_progress spec
    in
    let rec loop ranges =
      match ranges with
      | [] -> Lwt.return_ok ()
      | _ ->
          let batch, rest =
            split_batch ~concurrency:options.concurrency ranges
          in
          Lwt.bind (Lwt_list.map_p download_one batch) (fun results ->
              match first_error_or_unit results with
              | Error _ as error -> Lwt.return error
              | Ok () -> loop rest)
    in
    loop ranges

  let download_file conn ~bucket ~key ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_download_options options
    in
    let* () =
      Lwt.return (Awskit_s3.Transfer.validate_download_options options)
    in
    let download_with_get () =
      let* result, () =
        S3.Object.get conn ~bucket ~key ~options:options.get_options
          ~consume:(Reader.to_path ?on_progress path)
          ()
      in
      Lwt.return_ok (Awskit_s3.Transfer.Get result)
    in
    let head_options = head_options_of_get_options options.get_options in
    let* info = S3.Object.head conn ~bucket ~key ~options:head_options () in
    match info.content_length with
    | None -> download_with_get ()
    | Some content_length
      when Int64.compare content_length options.multipart_threshold < 0
           || Int64.equal content_length 0L ->
        download_with_get ()
    | Some content_length ->
        let* ranges =
          Lwt.return (range_specs ~content_length ~part_size:options.part_size)
        in
        with_temp_download path (fun temp_path fd ->
            let* () =
              ranged_download_to_fd conn ~bucket ~key ~options ?on_progress
                ~path:temp_path ~fd ranges
            in
            Lwt.return_ok
              (Awskit_s3.Transfer.Ranged { info; parts = List.length ranges }))
end
