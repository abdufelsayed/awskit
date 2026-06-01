module Make
    (Runtime : Awskit_s3.RUNTIME with type 'a t = 'a)
    (S3 : sig
      module Object :
        Awskit_s3.OBJECT
          with type connection := Runtime.connection
           and type 'a io := 'a
           and type request_body := Runtime.request_body
           and type response_body_reader := Runtime.response_body_reader

      module Multipart :
        Awskit_s3.MULTIPART
          with type connection := Runtime.connection
           and type 'a io := 'a
           and type request_body := Runtime.request_body
    end) =
struct
  let buffer_size = 128 * 1024

  type part_spec = { part_number : int; offset : int64; length : int }

  let ( let* ) result f =
    match result with Ok value -> f value | Error _ as error -> error

  let body_error action path exn =
    Awskit.Error.body
      (Fmt.str "failed to %s path %a: %s" action Eio.Path.pp path
         (Printexc.to_string exn))

  let regular_file_length path =
    try
      let stat = Eio.Path.stat ~follow:true path in
      match stat.kind with
      | `Regular_file -> Ok (Optint.Int63.to_int64 stat.size)
      | kind ->
          Error
            (Awskit.Error.validation ~field:"path"
               (Fmt.str "expected regular file, got %a" Eio.File.Stat.pp_kind
                  kind))
    with exn -> Error (body_error "stat upload" path exn)

  let request_body_of_path ?on_progress path =
    match regular_file_length path with
    | Error _ as error -> error
    | Ok content_length ->
        let descriptor =
          {
            Awskit.Body.Request.content_length = Some content_length;
            payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
            replayable = true;
          }
        in
        let write writer =
          try
            Eio.Path.with_open_in path (fun file ->
                let bytes = Bytes.create buffer_size in
                let cstruct = Cstruct.of_bytes bytes in
                let rec loop transferred =
                  match Eio.Flow.single_read file cstruct with
                  | n -> (
                      let chunk = Bytes.sub_string bytes 0 n in
                      match Runtime.Request_body.write_string writer chunk with
                      | Error _ as error -> error
                      | Ok () ->
                          let transferred =
                            Int64.add transferred (Int64.of_int n)
                          in
                          Option.iter (fun f -> f transferred) on_progress;
                          loop transferred)
                  | exception End_of_file -> Ok ()
                in
                loop 0L)
          with exn -> Error (body_error "read upload" path exn)
        in
        Ok (Runtime.Request_body.of_stream descriptor ~write)

  let upload_file conn ~bucket ~key ?options ?on_progress ~path () =
    match request_body_of_path ?on_progress path with
    | Error _ as error -> error
    | Ok body -> S3.Object.put conn ~bucket ~key ?options ~body ()

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
      try
        Eio.Path.with_open_in path (fun file ->
            ignore (Eio.File.seek file (Optint.Int63.of_int64 spec.offset) `Set);
            let bytes = Bytes.create buffer_size in
            let rec loop remaining =
              if remaining = 0 then Ok ()
              else
                let len = min buffer_size remaining in
                let cstruct = Cstruct.of_bytes ~len bytes in
                match Eio.Flow.single_read file cstruct with
                | n -> (
                    let chunk = Bytes.sub_string bytes 0 n in
                    match Runtime.Request_body.write_string writer chunk with
                    | Error _ as error -> error
                    | Ok () -> loop (remaining - n))
                | exception End_of_file ->
                    Error
                      (Awskit.Error.body
                         (Fmt.str
                            "unexpected end of file while reading part %d from \
                             %a"
                            spec.part_number Eio.Path.pp path))
            in
            loop spec.length)
      with exn -> Error (body_error "read multipart upload" path exn)
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
    match
      S3.Multipart.upload_part conn ~bucket ~key ~upload_id
        ~part_number:spec.part_number ~body
        ~options:options.Awskit_s3.Multipart.Managed.upload_part_options ()
    with
    | Error _ as error -> error
    | Ok uploaded -> Ok uploaded.part

  let upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
      ~concurrency ?on_progress ~initial_completed specs =
    let completed = ref initial_completed in
    Option.iter
      (fun f -> if Int64.compare !completed 0L > 0 then f !completed)
      on_progress;
    let upload_one spec =
      match
        upload_part_from_path conn ~bucket ~key ~upload_id ~options ~path spec
      with
      | Error _ as error -> error
      | Ok part ->
          completed := Int64.add !completed (Int64.of_int spec.length);
          Option.iter (fun f -> f !completed) on_progress;
          Ok part
    in
    let rec loop acc specs =
      match specs with
      | [] -> Ok (List.rev acc)
      | _ -> (
          let batch, rest = split_batch ~concurrency specs in
          let results =
            Eio.Switch.run @@ fun sw ->
            batch
            |> List.map (fun spec ->
                Eio.Fiber.fork_promise ~sw (fun () -> upload_one spec))
            |> List.map Eio.Promise.await_exn
          in
          match first_error_or_parts results with
          | Error _ as error -> error
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
    match S3.Multipart.List_parts.parts conn ~bucket ~key ~upload_id () with
    | Error _ as error -> error
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
                  Awskit_s3.Multipart.Part.create ~part_number:spec.part_number
                    ~etag
                  |> Result.to_option
              | _ -> None)
        in
        Ok (List.filter_map part_for_spec specs)

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
    match S3.Multipart.complete conn ~bucket ~key ~upload_id parts with
    | Error _ as error -> error
    | Ok complete -> Ok { Awskit_s3.Multipart.Managed.upload; parts; complete }

  let resume_multipart_upload_file conn ~bucket ~key ~upload_id ?options
      ?(concurrency = 4) ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Multipart.Managed.default_options options
    in
    let* () = validate_concurrency concurrency in
    let* () = Awskit_s3.Multipart.Managed.validate_options options in
    let* content_length = regular_file_length path in
    let* specs = multipart_specs ~content_length ~part_size:options.part_size in
    let* upload = Awskit_s3.Multipart.Upload.create ~bucket ~key ~upload_id in
    let* uploaded_parts =
      matching_uploaded_parts conn ~bucket ~key ~upload_id specs
    in
    let initial_completed = completed_bytes specs uploaded_parts in
    let missing = remaining_specs specs uploaded_parts in
    let* uploaded_now =
      upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
        ~concurrency ?on_progress ~initial_completed missing
    in
    complete_multipart conn ~bucket ~key ~upload_id upload
      (uploaded_parts @ uploaded_now)

  let upload_multipart_file conn ~bucket ~key ?options ?(concurrency = 4)
      ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Multipart.Managed.default_options options
    in
    let* () = validate_concurrency concurrency in
    let* () = Awskit_s3.Multipart.Managed.validate_options options in
    let* content_length = regular_file_length path in
    let* specs = multipart_specs ~content_length ~part_size:options.part_size in
    let* created =
      S3.Multipart.create conn ~bucket ~key ~options:options.create_options ()
    in
    let upload_id = created.upload.upload_id in
    let abort_and_return error =
      ignore (S3.Multipart.abort conn ~bucket ~key ~upload_id);
      Error error
    in
    match
      upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
        ~concurrency ?on_progress ~initial_completed:0L specs
    with
    | Error error -> abort_and_return error
    | Ok parts -> (
        match
          complete_multipart conn ~bucket ~key ~upload_id created.upload parts
        with
        | Ok _ as result -> result
        | Error error -> abort_and_return error)

  let download_file conn ~bucket ~key ?options ?on_progress ~path () =
    let consume reader =
      try
        Eio.Path.with_open_out ~create:(`Or_truncate 0o600) path (fun file ->
            let bytes = Bytes.create buffer_size in
            let rec loop transferred =
              match
                Runtime.Response_body.read reader bytes ~off:0 ~len:buffer_size
              with
              | Error _ as error -> error
              | Ok 0 -> Ok ()
              | Ok n ->
                  Eio.Flow.write file [ Cstruct.of_bytes ~len:n bytes ];
                  let transferred = Int64.add transferred (Int64.of_int n) in
                  Option.iter (fun f -> f transferred) on_progress;
                  loop transferred
            in
            loop 0L)
      with exn -> Error (body_error "write download" path exn)
    in
    match S3.Object.get conn ~bucket ~key ?options ~consume () with
    | Error _ as error -> error
    | Ok (info, ()) -> Ok info
end
