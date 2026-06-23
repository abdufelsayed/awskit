let buffer_size = 128 * 1024
let temp_counter = Atomic.make 0

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let body_error action path exn =
  Awskit.Error.Internal.body
    (Fmt.str "failed to %s path %a: %s" action Eio.Path.pp path
       (Printexc.to_string exn))

let target_error action target exn =
  Awskit.Error.Internal.body
    (Fmt.str "failed to %s %s: %s" action target (Printexc.to_string exn))

let regular_file_length path =
  try
    let stat = Eio.Path.stat ~follow:true path in
    match stat.kind with
    | `Regular_file -> Ok (Optint.Int63.to_int64 stat.size)
    | kind ->
        Error
          (Awskit.Error.Internal.validation ~field:"path"
             (Fmt.str "expected regular file, got %a" Eio.File.Stat.pp_kind kind))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (body_error "stat upload" path exn)

let descriptor ~content_length ~replayable =
  {
    Awskit.Body.Request.content_length = Some content_length;
    payload_hash = Awskit.Body.Payload_hash.unsigned_payload;
    replayable;
  }

let temp_download_path path attempt =
  match Eio.Path.split path with
  | Some (dir, base) ->
      let id = Atomic.fetch_and_add temp_counter 1 in
      Ok
        Eio.Path.(
          dir / Fmt.str ".%s.awskit-download.%08x.%d.tmp" base id attempt)
  | None ->
      Error
        (Awskit.Error.Internal.validation ~field:"path"
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
        (Awskit.Error.Internal.multiple [ error; cleanup_error ]
        |> Awskit.Error.Internal.with_context
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
        (Awskit.Error.Internal.body
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
        | Error close_error -> cleanup_temp_download temp_path close_error
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
      | exception exn ->
          close_and_cleanup (body_error "write download" path exn)
      | Error error -> close_and_cleanup error
      | Ok value -> close_and_publish value)

module Make_body_reader
    (Runtime : Awskit_s3.RUNTIME with type 'a t = 'a)
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
            let chunk = Cstruct.to_string (Cstruct.sub cstruct 0 n) in
            match Runtime.Request_body.write_string writer chunk with
            | Error _ as error -> error
            | Ok () ->
                let transferred = Int64.add transferred (Int64.of_int n) in
                Option.iter (fun f -> f transferred) on_progress;
                loop transferred)
        | exception End_of_file -> Ok ()
      in
      loop 0L

    let of_flow ~content_length ?on_progress flow =
      let write writer =
        try copy_flow_to_writer ?on_progress flow writer with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn -> Error (target_error "read upload flow" "<flow>" exn)
      in
      Runtime.Request_body.of_stream
        (descriptor ~content_length ~replayable:false)
        ~write

    let of_path ?on_progress path =
      let* content_length = regular_file_length path in
      let write writer =
        try
          Eio.Path.with_open_in path (fun file ->
              copy_flow_to_writer ?on_progress file writer)
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | exn -> Error (body_error "read upload" path exn)
      in
      Ok
        (Runtime.Request_body.of_stream
           (descriptor ~content_length ~replayable:true)
           ~write)
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
            Option.iter (fun f -> f transferred) on_progress;
            loop transferred
      in
      loop 0L

    let to_flow ?on_progress flow reader =
      try copy_reader_to_flow ?on_progress flow reader with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> Error (target_error "write download flow" "<flow>" exn)

    let to_path ?on_progress path reader =
      with_temp_download path (fun _temp_path file ->
          try copy_reader_to_flow ?on_progress file reader with
          | Eio.Cancel.Cancelled _ as exn -> raise exn
          | exn -> Error (body_error "write download" path exn))
  end
end

module Make
    (Runtime : Awskit_s3.RUNTIME with type 'a t = 'a)
    (S3 : sig
      module Object : sig
        val put :
          Runtime.connection ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Put_object.options ->
          body:Runtime.request_body ->
          unit ->
          (Awskit_s3.Put_object.result, Awskit_s3.Error.t) result

        val get :
          Runtime.connection ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Get_object.options ->
          consume:
            (Runtime.response_body_reader -> ('a, Awskit_s3.Error.t) result) ->
          unit ->
          ('a Awskit_s3.Get_object.result, Awskit_s3.Error.t) result

        val head :
          Runtime.connection ->
          bucket:Awskit_s3.Bucket_name.t ->
          key:Awskit_s3.Object_key.t ->
          ?options:Awskit_s3.Head_object.options ->
          unit ->
          (Awskit_s3.Head_object.result, Awskit_s3.Error.t) result
      end

      module Multipart :
        Awskit_s3.MULTIPART
          with type connection := Runtime.connection
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
  type part_spec = { part_number : int; offset : int64; length : int }
  type range_spec = { index : int; offset : int64; length : int }

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
      Awskit.Error.Internal.validation ~field:"path"
        "multipart file upload requires a non-empty file"
    in
    let* () =
      Awskit_s3.Transfer.validate_multipart_part_count ~content_length
        ~part_size
    in
    bounded_specs ~content_length ~part_size ~empty_error:(Some empty_error)
      ~make:(fun part_number offset length -> { part_number; offset; length })

  let range_specs ~content_length ~part_size =
    let* () =
      Awskit_s3.Transfer.validate_multipart_part_count ~content_length
        ~part_size
    in
    bounded_specs ~content_length ~part_size ~empty_error:None
      ~make:(fun index offset length -> { index; offset; length })

  let range_body_of_path path (spec : part_spec) =
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
                | n -> (
                    let chunk =
                      Cstruct.to_string (Cstruct.sub read_buffer 0 n)
                    in
                    match Runtime.Request_body.write_string writer chunk with
                    | Error _ as error -> error
                    | Ok () -> loop (remaining - n))
                | exception End_of_file ->
                    Error
                      (Awskit.Error.Internal.body
                         (Fmt.str
                            "unexpected end of file while reading part %d from \
                             %a"
                            spec.part_number Eio.Path.pp path))
            in
            loop spec.length)
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> Error (body_error "read multipart upload" path exn)
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
    match
      S3.Multipart.upload_part conn ~bucket ~key ~upload_id
        ~part_number:spec.part_number ~body
        ~options:options.Awskit_s3.Transfer.upload_part_options ()
    with
    | Error _ as error -> error
    | Ok uploaded -> Ok uploaded.part

  let upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
      ?on_progress ~initial_completed specs =
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
      | _ ->
          let batch, rest =
            split_batch ~concurrency:options.Awskit_s3.Transfer.concurrency
              specs
          in
          let results =
            Eio.Switch.run @@ fun sw ->
            batch
            |> List.map (fun spec ->
                Eio.Fiber.fork_promise ~sw (fun () -> upload_one spec))
            |> List.map Eio.Promise.await_exn
          in
          let* parts = first_error_or_parts results in
          loop (List.rev_append parts acc) rest
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
    match
      S3.Multipart.List_parts.parts conn ~bucket ~key ~upload_id
        ~options:options.Awskit_s3.Transfer.list_parts_options ()
    with
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
                    ~etag ()
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

  let complete_multipart conn ~bucket ~key ~upload_id ~options upload parts =
    let parts = sort_parts parts in
    match
      S3.Multipart.complete_upload conn ~bucket ~key ~upload_id parts
        ~options:options.Awskit_s3.Transfer.complete_options
    with
    | Error _ as error -> error
    | Ok complete -> Ok { Awskit_s3.Transfer.upload; parts; complete }

  let resume_multipart_upload_file conn ~bucket ~key ~upload_id ?options
      ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_upload_options options
    in
    let* () = Awskit_s3.Transfer.validate_upload_options options in
    let* () = Awskit_s3.Transfer.validate_upload_multipart_selection options in
    let* content_length = regular_file_length path in
    let* specs = multipart_specs ~content_length ~part_size:options.part_size in
    let* upload =
      Awskit_s3.Multipart.Upload.create
        ~bucket:(Awskit_s3.Bucket_name.to_string bucket)
        ~key:(Awskit_s3.Object_key.to_string key)
        ~upload_id
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
    let* () = Awskit_s3.Transfer.validate_upload_options options in
    let* () = Awskit_s3.Transfer.validate_upload_multipart_selection options in
    let* content_length = regular_file_length path in
    let* specs = multipart_specs ~content_length ~part_size:options.part_size in
    let* created =
      S3.Multipart.create_upload conn ~bucket ~key
        ~options:options.create_options ()
    in
    let upload_id = created.upload.upload_id in
    let abort_and_return error =
      match
        S3.Multipart.abort_upload conn ~bucket ~key ~upload_id
          ~options:options.abort_options ()
      with
      | Ok _ -> Error error
      | Error cleanup_error ->
          Error
            (Awskit.Error.Internal.multiple [ error; cleanup_error ]
            |> Awskit.Error.Internal.with_context
                 "multipart upload failed and abort also failed")
    in
    let abort_cleanup_ignore_errors () =
      Eio.Cancel.protect (fun () ->
          match
            S3.Multipart.abort_upload conn ~bucket ~key ~upload_id
              ~options:options.abort_options ()
          with
          | Ok _ | Error _ -> ()
          | exception _ -> ())
    in
    let abort_then_raise exn =
      abort_cleanup_ignore_errors ();
      raise exn
    in
    let upload_and_complete () =
      match
        upload_missing_parts conn ~bucket ~key ~upload_id ~options ~path
          ?on_progress ~initial_completed:0L specs
      with
      | Error error -> Error error
      | Ok parts -> (
          match
            complete_multipart conn ~bucket ~key ~upload_id ~options
              created.upload parts
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
    if Int64.compare content_length options.multipart_threshold < 0 then
      let* body = Body.of_path ?on_progress path in
      let* result =
        S3.Object.put conn ~bucket ~key ~options:options.put_options ~body ()
      in
      Ok (Awskit_s3.Transfer.Put result)
    else
      let* () =
        Awskit_s3.Transfer.validate_upload_multipart_selection options
      in
      let* result =
        multipart_upload_file conn ~bucket ~key ~options ?on_progress ~path ()
      in
      Ok (Awskit_s3.Transfer.Multipart result)

  let head_options_of_get_options (options : Awskit_s3.Get_object.options) :
      Awskit_s3.Head_object.options =
    {
      preconditions = options.preconditions;
      version_id = options.version_id;
      checksum_mode = options.checksum_mode;
      expected_bucket_owner = options.expected_bucket_owner;
    }

  let get_info (result : _ Awskit_s3.Get_object.result) :
      Awskit_s3.Get_object.info =
    {
      etag = result.etag;
      content_type = result.content_type;
      content_length = result.content_length;
      last_modified = result.last_modified;
      metadata = result.metadata;
      storage_class = result.storage_class;
      version_id = result.version_id;
      checksum = result.checksum;
      server_side_encryption = result.server_side_encryption;
      response = result.response;
    }

  let ranged_get_options_of_head (info : Awskit_s3.Head_object.result)
      (get_options : Awskit_s3.Get_object.options) :
      Awskit_s3.Get_object.options =
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

  let download_range_to_file conn ~bucket ~key ~options ~path ~file ~completed
      ?on_progress spec =
    let finish =
      Int64.add spec.offset (Int64.of_int spec.length) |> Int64.pred
    in
    let range = Awskit_s3.Range.bytes_exn ~start:spec.offset ~finish in
    let get_options =
      { options.Awskit_s3.Transfer.get_options with range = Some range }
    in
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
                  (Awskit.Error.Internal.body
                     (Fmt.str
                        "unexpected end of response while downloading range %d"
                        spec.index))
            | Ok n ->
                Eio.File.pwrite_all file
                  ~file_offset:(Optint.Int63.of_int64 position)
                  [ Cstruct.of_bytes ~len:n bytes ];
                completed := Int64.add !completed (Int64.of_int n);
                Option.iter (fun f -> f !completed) on_progress;
                loop (Int64.add position (Int64.of_int n)) (remaining - n)
        in
        loop spec.offset spec.length
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> Error (body_error "write download" path exn)
    in
    match S3.Object.get conn ~bucket ~key ~options:get_options ~consume () with
    | Error _ as error -> error
    | Ok { value = (); _ } -> Ok ()

  let ranged_download_to_file conn ~bucket ~key ~options ?on_progress ~path
      ~file ranges =
    let completed = ref 0L in
    let download_one spec =
      download_range_to_file conn ~bucket ~key ~options ~path ~file ~completed
        ?on_progress spec
    in
    let rec loop ranges =
      match ranges with
      | [] -> Ok ()
      | _ ->
          let batch, rest =
            split_batch ~concurrency:options.Awskit_s3.Transfer.concurrency
              ranges
          in
          let results =
            Eio.Switch.run @@ fun sw ->
            batch
            |> List.map (fun spec ->
                Eio.Fiber.fork_promise ~sw (fun () -> download_one spec))
            |> List.map Eio.Promise.await_exn
          in
          let* () = first_error_or_unit results in
          loop rest
    in
    loop ranges

  let download_file conn ~bucket ~key ?options ?on_progress ~path () =
    let options =
      Option.value ~default:Awskit_s3.Transfer.default_download_options options
    in
    let* () = Awskit_s3.Transfer.validate_download_options options in
    let download_with_get () =
      let* result =
        S3.Object.get conn ~bucket ~key ~options:options.get_options
          ~consume:(Reader.to_path ?on_progress path)
          ()
      in
      Ok (Awskit_s3.Transfer.Get (get_info result))
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
          range_specs ~content_length ~part_size:options.part_size
        in
        let get_options = ranged_get_options_of_head info options.get_options in
        let options = { options with get_options } in
        with_temp_download path (fun temp_path file ->
            let* () =
              ranged_download_to_file conn ~bucket ~key ~options ?on_progress
                ~path:temp_path ~file ranges
            in
            Ok (Awskit_s3.Transfer.Ranged { info; parts = List.length ranges }))
end
