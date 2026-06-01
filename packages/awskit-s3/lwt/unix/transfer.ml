module Make
    (Runtime : Awskit_s3.RUNTIME with type 'a t = 'a Lwt.t)
    (S3 : sig
      module Object :
        Awskit_s3.OBJECT
          with type connection := Runtime.connection
           and type 'a io := 'a Lwt.t
           and type request_body := Runtime.request_body
           and type response_body_reader := Runtime.response_body_reader

      module Multipart :
        Awskit_s3.MULTIPART
          with type connection := Runtime.connection
           and type 'a io := 'a Lwt.t
           and type request_body := Runtime.request_body
    end) =
struct
  let buffer_size = 128 * 1024

  type part_spec = { part_number : int; offset : int64; length : int }

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

  let request_body_of_path ?on_progress path =
    Lwt.bind (regular_file_length path) (function
      | Error _ as error -> Lwt.return error
      | Ok content_length ->
          let descriptor =
            {
              Awskit.Body.Request.content_length = Some content_length;
              payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
              replayable = true;
            }
          in
          let write writer =
            Lwt.catch
              (fun () ->
                Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel ->
                    let bytes = Bytes.create buffer_size in
                    let rec loop transferred =
                      Lwt.bind (Lwt_io.read_into channel bytes 0 buffer_size)
                        (function
                        | 0 -> Lwt.return_ok ()
                        | n ->
                            let chunk = Bytes.sub_string bytes 0 n in
                            Lwt.bind
                              (Runtime.Request_body.write_string writer chunk)
                              (function
                              | Error _ as error -> Lwt.return error
                              | Ok () ->
                                  let transferred =
                                    Int64.add transferred (Int64.of_int n)
                                  in
                                  Option.iter
                                    (fun f -> f transferred)
                                    on_progress;
                                  loop transferred))
                    in
                    loop 0L))
              (fun exn -> Lwt.return_error (body_error "read upload" path exn))
          in
          Lwt.return_ok (Runtime.Request_body.of_stream descriptor ~write))

  let upload_file conn ~bucket ~key ?options ?on_progress ~path () =
    Lwt.bind (request_body_of_path ?on_progress path) (function
      | Error _ as error -> Lwt.return error
      | Ok body -> S3.Object.put conn ~bucket ~key ?options ~body ())

  let validate_concurrency concurrency =
    if concurrency <= 0 then
      Error
        (Awskit.Error.validation ~field:"concurrency"
           "concurrency must be positive")
    else Ok ()

  let multipart_specs ~content_length ~part_size =
    if Int64.equal content_length 0L then
      Error
        (Awskit.Error.validation ~field:"path"
           "multipart file upload requires a non-empty file")
    else
      let part_size64 = Int64.of_int part_size in
      let part_count =
        Int64.div
          (Int64.add content_length (Int64.pred part_size64))
          part_size64
      in
      if
        Int64.compare part_count
          (Int64.of_int Awskit_s3.Multipart.Managed.max_parts)
        > 0
      then
        Error
          (Awskit.Error.validation ~field:"part_count"
             "multipart file upload would exceed 10000 parts")
      else
        let rec loop part_number offset acc =
          if Int64.compare offset content_length >= 0 then Ok (List.rev acc)
          else
            let remaining = Int64.sub content_length offset in
            let length = min part_size (Int64.to_int remaining) in
            loop (part_number + 1)
              (Int64.add offset (Int64.of_int length))
              ({ part_number; offset; length } :: acc)
        in
        loop 1 0L []

  let range_body_of_path path spec =
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

  let upload_part_from_path conn ~bucket ~key ~upload_id ~options ~path spec =
    let body = range_body_of_path path spec in
    Lwt.bind
      (S3.Multipart.upload_part conn ~bucket ~key ~upload_id
         ~part_number:spec.part_number ~body
         ~options:options.Awskit_s3.Multipart.Managed.upload_part_options ())
      (function
      | Error _ as error -> Lwt.return error
      | Ok uploaded -> Lwt.return_ok uploaded.part)

  let upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
      ~concurrency ?on_progress ~initial_completed specs =
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
          let batch, rest = split_batch ~concurrency specs in
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

  let matching_uploaded_parts conn ~bucket ~key ~upload_id specs =
    Lwt.bind (S3.Multipart.Paginator.parts conn ~bucket ~key ~upload_id ())
      (function
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
                      ~part_number:spec.part_number ~etag
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

  let complete_multipart conn ~bucket ~key ~upload_id upload parts =
    let parts = sort_parts parts in
    Lwt.bind (S3.Multipart.complete conn ~bucket ~key ~upload_id parts)
      (function
      | Error _ as error -> Lwt.return error
      | Ok complete ->
          Lwt.return_ok { Awskit_s3.Multipart.Managed.upload; parts; complete })

  let resume_multipart_upload_file conn ~bucket ~key ~upload_id ?options
      ?(concurrency = 4) ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Multipart.Managed.default_options options
    in
    match validate_concurrency concurrency with
    | Error _ as error -> Lwt.return error
    | Ok () -> (
        match Awskit_s3.Multipart.Managed.validate_options options with
        | Error _ as error -> Lwt.return error
        | Ok () ->
            Lwt.bind (regular_file_length path) (function
              | Error _ as error -> Lwt.return error
              | Ok content_length -> (
                  match
                    multipart_specs ~content_length ~part_size:options.part_size
                  with
                  | Error _ as error -> Lwt.return error
                  | Ok specs -> (
                      match
                        Awskit_s3.Multipart.Upload.create ~bucket ~key
                          ~upload_id
                      with
                      | Error _ as error -> Lwt.return error
                      | Ok upload ->
                          Lwt.bind
                            (matching_uploaded_parts conn ~bucket ~key
                               ~upload_id specs) (function
                            | Error _ as error -> Lwt.return error
                            | Ok uploaded_parts ->
                                let initial_completed =
                                  completed_bytes specs uploaded_parts
                                in
                                let missing =
                                  remaining_specs specs uploaded_parts
                                in
                                Lwt.bind
                                  (upload_missing_parts conn ~bucket ~key
                                     ~upload_id ~options ~path ~concurrency
                                     ?on_progress ~initial_completed missing)
                                  (function
                                  | Error _ as error -> Lwt.return error
                                  | Ok uploaded_now ->
                                      complete_multipart conn ~bucket ~key
                                        ~upload_id upload
                                        (uploaded_parts @ uploaded_now)))))))

  let upload_multipart_file conn ~bucket ~key ?options ?(concurrency = 4)
      ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Multipart.Managed.default_options options
    in
    match validate_concurrency concurrency with
    | Error _ as error -> Lwt.return error
    | Ok () -> (
        match Awskit_s3.Multipart.Managed.validate_options options with
        | Error _ as error -> Lwt.return error
        | Ok () ->
            Lwt.bind (regular_file_length path) (function
              | Error _ as error -> Lwt.return error
              | Ok content_length -> (
                  match
                    multipart_specs ~content_length ~part_size:options.part_size
                  with
                  | Error _ as error -> Lwt.return error
                  | Ok specs ->
                      Lwt.bind
                        (S3.Multipart.create conn ~bucket ~key
                           ~options:options.create_options ()) (function
                        | Error _ as error -> Lwt.return error
                        | Ok created ->
                            let upload_id = created.upload.upload_id in
                            let abort_and_return error =
                              Lwt.bind
                                (S3.Multipart.abort conn ~bucket ~key ~upload_id)
                                (fun _ -> Lwt.return_error error)
                            in
                            Lwt.bind
                              (upload_missing_parts conn ~bucket ~key ~upload_id
                                 ~options ~path ~concurrency ?on_progress
                                 ~initial_completed:0L specs) (function
                              | Error error -> abort_and_return error
                              | Ok parts ->
                                  Lwt.bind
                                    (complete_multipart conn ~bucket ~key
                                       ~upload_id created.upload parts)
                                    (function
                                    | Ok _ as result -> Lwt.return result
                                    | Error error -> abort_and_return error)))))
        )

  let download_file conn ~bucket ~key ?options ?on_progress ~path () =
    let consume reader =
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Output ~perm:0o600 path (fun channel ->
              let bytes = Bytes.create buffer_size in
              let rec loop transferred =
                Lwt.bind
                  (Runtime.Response_body.read reader bytes ~off:0
                     ~len:buffer_size) (function
                  | Error _ as error -> Lwt.return error
                  | Ok 0 -> Lwt.return_ok ()
                  | Ok n ->
                      Lwt.bind (Lwt_io.write_from_exactly channel bytes 0 n)
                        (fun () ->
                          let transferred =
                            Int64.add transferred (Int64.of_int n)
                          in
                          Option.iter (fun f -> f transferred) on_progress;
                          loop transferred))
              in
              loop 0L))
        (fun exn -> Lwt.return_error (body_error "write download" path exn))
    in
    Lwt.bind (S3.Object.get conn ~bucket ~key ?options ~consume ()) (function
      | Error _ as error -> Lwt.return error
      | Ok (info, ()) -> Lwt.return_ok info)
end
