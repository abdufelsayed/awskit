open Simulator_support
open Simulator_headers
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_checksum
open Simulator_runtime
module Bucket_name = Awskit_s3.Bucket_name
module Multipart_model = Awskit_s3.Multipart
module Object_key = Awskit_s3.Object_key
module Object_model = Awskit_s3.Object

module Multipart = struct
  type connection = t
  type 'a io = 'a
  type request_body = Runtime.request_body

  let validate_opt f = function None -> Ok () | Some value -> f value

  let validate_create_options (options : Multipart_model.Create.options) =
    validate_opt
      (fun (checksum : Multipart_model.Create.Checksum.t) ->
        validate_supported_algorithm checksum.algorithm)
      options.checksum

  let upload_handle_bucket upload =
    Multipart_model.Upload.bucket upload |> Bucket_name.to_string

  let upload_handle_key upload =
    Multipart_model.Upload.key upload |> Object_key.to_string

  let upload_handle_id upload = Multipart_model.Upload.upload_id upload

  let completed_part_number part =
    Multipart_model.Part.part_number part |> Multipart_model.Part_number.to_int

  let complete_min_part_size = 5 * 1024 * 1024

  let created_upload_of_strings ~bucket ~key ~upload_id =
    let* bucket = Bucket_name.of_string bucket in
    let* key = Object_key.of_string key in
    Ok (Multipart_model.Upload.Runtime_adapter.created ~bucket ~key ~upload_id)

  let create_upload conn ~bucket ~key ?options () =
    let options =
      Option.value ~default:Multipart_model.Create.default_options options
    in
    let return_error error =
      Error (with_operation `Create_multipart_upload ~bucket ~key error)
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> return_error error
        | Ok bucket_state -> (
            match validate_create_options options with
            | Error error -> return_error error
            | Ok () -> (
                match
                  operation_fault conn `Create_multipart_upload bucket
                    (Some key)
                with
                | Some error -> return_error error
                | None -> (
                    let upload_id = next_upload_id conn in
                    match created_upload_of_strings ~bucket ~key ~upload_id with
                    | Error error -> return_error error
                    | Ok upload ->
                        Hashtbl.replace bucket_state.multipart_uploads
                          (upload_key upload_id)
                          {
                            upload;
                            content_type = options.content_type;
                            metadata = options.metadata;
                            storage_class = options.storage_class;
                            tags = options.tags;
                            checksum = options.checksum;
                            parts = Hashtbl.create 17;
                            created_at = now conn;
                          };
                        Ok
                          {
                            Multipart_model.Create.upload;
                            response = response 200;
                          }))))

  let request_body_result body =
    let descriptor = Runtime.Request_body.descriptor body in
    match descriptor.content_length with
    | None ->
        Error
          (Awskit.Error.Producer.validation ~field:"content_length"
             "S3 multipart uploads require a known content length")
    | Some _ -> (
        match Awskit.Body.Request.validate_descriptor descriptor with
        | Error error -> Error error
        | Ok () -> Runtime.request_body_result body)

  let create_checksum_type (checksum : Multipart_model.Create.Checksum.t) =
    match checksum.checksum_type with
    | Some checksum_type -> checksum_type
    | None -> (
        match checksum.algorithm with
        | Object_model.Checksum.Algorithm.Crc64nvme ->
            Object_model.Checksum.Type.Full_object
        | Object_model.Checksum.Algorithm.Crc32
        | Object_model.Checksum.Algorithm.Crc32c
        | Object_model.Checksum.Algorithm.Sha1
        | Object_model.Checksum.Algorithm.Sha256
        | Object_model.Checksum.Algorithm.Sha512
        | Object_model.Checksum.Algorithm.Md5
        | Object_model.Checksum.Algorithm.Xxhash64
        | Object_model.Checksum.Algorithm.Xxhash3
        | Object_model.Checksum.Algorithm.Xxhash128 ->
            Object_model.Checksum.Type.Composite)

  let validate_upload_part_policy (upload : multipart_upload)
      (options : Multipart_model.Upload_part.options) =
    let invalid_request message =
      Error (service ~status:400 ~code:"InvalidRequest" ~message ())
    in
    let bad_digest message =
      Error (service ~status:400 ~code:"BadDigest" ~message ())
    in
    match upload.checksum with
    | None -> Ok ()
    | Some (created : Multipart_model.Create.Checksum.t) -> (
        match options.Multipart_model.Upload_part.checksum with
        | Some (supplied : Object_model.Checksum.value)
          when supplied.algorithm <> created.algorithm ->
            bad_digest "part checksum algorithm does not match upload"
        | None
          when create_checksum_type created
               = Object_model.Checksum.Type.Composite ->
            invalid_request "composite multipart uploads require part checksums"
        | Some _ | None -> Ok ())

  let upload_part conn ~upload ~part_number ~body ?options () =
    let options =
      Option.value ~default:Multipart_model.Upload_part.default_options options
    in
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    let part_number_int = Multipart_model.Part_number.to_int part_number in
    let return_error error =
      Error (with_operation `Upload_part ~bucket ~key error)
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match require_multipart_upload conn ~bucket ~key ~upload_id with
        | Error error -> return_error error
        | Ok (_bucket_state, stored_upload) -> (
            match validate_upload_part_policy stored_upload options with
            | Error error -> return_error error
            | Ok () -> (
                match operation_fault conn `Upload_part bucket (Some key) with
                | Some error -> return_error error
                | None -> (
                    match request_body_result body with
                    | Error error -> return_error error
                    | Ok body -> (
                        let etag = etag body in
                        match checksum_for_value ~body options.checksum with
                        | Error error -> return_error error
                        | Ok checksum ->
                            let part =
                              {
                                part_number = part_number_int;
                                body;
                                etag;
                                checksum;
                                last_modified = now conn;
                              }
                            in
                            Hashtbl.replace stored_upload.parts part_number_int
                              part;
                            let size = Int64.of_int (String.length body) in
                            let part =
                              Multipart_model.Part.create_exn ~part_number ~etag
                                ?checksum:options.checksum ~size ()
                            in
                            Ok
                              {
                                Multipart_model.Upload_part.part;
                                checksum;
                                response =
                                  response 200
                                    ~headers:
                                      (("etag", Object_model.Etag.to_string etag)
                                      :: checksum_response_headers checksum);
                              })))))

  let validate_complete_parts upload ~multipart_object_size parts =
    let invalid_part_size () =
      invalid ~field:"parts" "non-final multipart parts must be at least 5 MiB"
    in
    let invalid_object_size () =
      invalid ~field:"multipart_object_size"
        "multipart object size does not match completed part sizes"
    in
    let invalid_checksum message =
      Error (service ~status:400 ~code:"InvalidPart" ~message ())
    in
    let checksum_value_equal (left : Object_model.Checksum.value)
        (right : Object_model.Checksum.observed_value) =
      match right.algorithm with
      | Object_model.Checksum.Algorithm.Known algorithm ->
          left.algorithm = algorithm && String.equal left.value right.value
      | Object_model.Checksum.Algorithm.Unknown _ -> false
    in
    let validate_part_checksum (stored : stored_part)
        (part : Multipart_model.Part.t) =
      match
        ( stored.checksum.Object_model.Checksum.values,
          Multipart_model.Part.checksum part )
      with
      | [], None -> Ok ()
      | [], Some _ ->
          invalid_checksum "multipart part checksum was not uploaded"
      | _ :: _, None -> invalid_checksum "multipart part checksum is missing"
      | values, Some checksum ->
          if List.exists (checksum_value_equal checksum) values then Ok ()
          else invalid_checksum "multipart part checksum does not match"
    in
    let rec loop total = function
      | [] -> Ok total
      | (part : Multipart_model.Part.t) :: rest -> (
          let part_number = completed_part_number part in
          match Hashtbl.find_opt upload.parts part_number with
          | None ->
              Error
                (service ~status:400 ~code:"InvalidPart"
                   ~message:"multipart part is missing" ())
          | Some stored
            when not
                   (Object_model.Etag.equal stored.etag
                      (Multipart_model.Part.etag part)) ->
              Error
                (service ~status:400 ~code:"InvalidPart"
                   ~message:"multipart part etag does not match" ())
          | Some stored -> (
              match validate_part_checksum stored part with
              | Error _ as error -> error
              | Ok () ->
                  let has_more_parts =
                    match rest with [] -> false | _ :: _ -> true
                  in
                  if
                    has_more_parts
                    && String.length stored.body < complete_min_part_size
                  then invalid_part_size ()
                  else
                    let total =
                      Int64.add total (Int64.of_int (String.length stored.body))
                    in
                    loop total rest))
    in
    match loop 0L parts with
    | Error _ as error -> error
    | Ok total -> (
        match multipart_object_size with
        | Some expected ->
            if Int64.equal expected total then Ok () else invalid_object_size ()
        | None -> Ok ())

  let validate_completion_policy (upload : multipart_upload)
      (options : Multipart_model.Complete.options) parts =
    let bad_digest message =
      Error (service ~status:400 ~code:"BadDigest" ~message ())
    in
    match upload.checksum with
    | None -> Ok ()
    | Some (created : Multipart_model.Create.Checksum.t) -> (
        let completion_algorithm =
          Option.map
            (fun (checksum : Object_model.Checksum.value) -> checksum.algorithm)
            options.Multipart_model.Complete.checksum
        in
        let part_algorithm =
          List.find_map
            (fun part ->
              Option.map
                (fun (checksum : Object_model.Checksum.value) ->
                  checksum.algorithm)
                (Multipart_model.Part.checksum part))
            parts
        in
        let differs_from_created = function
          | Some algorithm -> algorithm <> created.algorithm
          | None -> false
        in
        if differs_from_created completion_algorithm then
          bad_digest "completion checksum algorithm does not match upload"
        else if differs_from_created part_algorithm then
          bad_digest "part checksum algorithm does not match upload"
        else
          match (created.checksum_type, options.checksum_type) with
          | Some created_type, Some completed_type
            when created_type <> completed_type ->
              bad_digest "completion checksum type does not match upload"
          | _ -> Ok ())

  let complete_upload conn ~upload ?options ~parts () =
    let options =
      Option.value ~default:Multipart_model.Complete.default_options options
    in
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    let part_values = Multipart_model.Complete.Parts.to_list parts in
    let multipart_object_size =
      Multipart_model.Complete.Parts.multipart_object_size parts
    in
    let return_error error =
      Error (with_operation `Complete_multipart_upload ~bucket ~key error)
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match require_multipart_upload conn ~bucket ~key ~upload_id with
        | Error error -> return_error error
        | Ok (bucket_state, upload) -> (
            match validate_completion_policy upload options part_values with
            | Error error -> return_error error
            | Ok () -> (
                match
                  validate_complete_parts upload ~multipart_object_size
                    part_values
                with
                | Error error -> return_error error
                | Ok () -> (
                    match
                      operation_fault conn `Complete_multipart_upload bucket
                        (Some key)
                    with
                    | Some error -> return_error error
                    | None -> (
                        match
                          ensure_write_preconditions
                            (current_object bucket_state key)
                            options.preconditions
                        with
                        | Error error -> return_error error
                        | Ok () -> (
                            let part_bodies =
                              part_values
                              |> List.map
                                   (fun (part : Multipart_model.Part.t) ->
                                     (Hashtbl.find upload.parts
                                        (completed_part_number part))
                                       .body)
                            in
                            let body = String.concat "" part_bodies in
                            let etag = multipart_etag part_bodies in
                            match checksum_for_value ~body options.checksum with
                            | Error error -> return_error error
                            | Ok supplied_checksum -> (
                                match
                                  match supplied_checksum.values with
                                  | [] ->
                                      checksum_for_algorithm ~body
                                        (Option.map
                                           (fun (checksum :
                                                  Multipart_model.Create
                                                  .Checksum
                                                  .t) -> checksum.algorithm)
                                           upload.checksum)
                                  | _ -> Ok supplied_checksum
                                with
                                | Error error -> return_error error
                                | Ok checksum ->
                                    let checksum =
                                      {
                                        checksum with
                                        checksum_type =
                                          Option.map
                                            (fun checksum_type ->
                                              Object_model.Checksum.Type.Known
                                                checksum_type)
                                            (match options.checksum_type with
                                            | Some _ as value -> value
                                            | None ->
                                                Option.bind upload.checksum
                                                  (fun
                                                    (checksum :
                                                      Multipart_model.Create
                                                      .Checksum
                                                      .t)
                                                  -> checksum.checksum_type));
                                      }
                                    in
                                    let obj =
                                      {
                                        body;
                                        etag;
                                        version_id = None;
                                        content_type = upload.content_type;
                                        metadata = upload.metadata;
                                        storage_class = upload.storage_class;
                                        tags = upload.tags;
                                        checksum;
                                        last_modified = now conn;
                                      }
                                    in
                                    let obj =
                                      store_object conn bucket_state key obj
                                    in
                                    Hashtbl.remove
                                      bucket_state.multipart_uploads
                                      (upload_key upload_id);
                                    Ok
                                      {
                                        Multipart_model.Complete.etag =
                                          Some etag;
                                        version_id = obj.version_id;
                                        checksum;
                                        response =
                                          response 200
                                            ~headers:
                                              (version_headers obj.version_id
                                              @ checksum_response_headers
                                                  checksum);
                                      })))))))

  let abort_upload conn ~upload ?expected_bucket_owner:_ () =
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    let return_error error =
      Error (with_operation `Abort_multipart_upload ~bucket ~key error)
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match require_multipart_upload conn ~bucket ~key ~upload_id with
        | Error error -> return_error error
        | Ok (bucket_state, _upload) -> (
            match
              operation_fault conn `Abort_multipart_upload bucket (Some key)
            with
            | Some error -> return_error error
            | None ->
                Hashtbl.remove bucket_state.multipart_uploads
                  (upload_key upload_id);
                Ok (response 204)))

  let list_parts conn ~upload ?options () =
    let options =
      Option.value ~default:Multipart_model.List_parts.default_options options
    in
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    let return_error error =
      Error (with_operation `List_parts ~bucket ~key error)
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match require_multipart_upload conn ~bucket ~key ~upload_id with
        | Error error -> return_error error
        | Ok (_bucket_state, upload) -> (
            match operation_fault conn `List_parts bucket (Some key) with
            | Some error -> return_error error
            | None ->
                let max_parts =
                  Option.value ~default:(config (store conn)).max_list_keys
                    options.max_parts
                in
                let all =
                  Simulator_state.parts upload
                  |> List.filter (fun part ->
                      match options.part_number_marker with
                      | None -> true
                      | Some marker ->
                          part.part_number
                          > Multipart_model.Part_number_marker.to_int marker)
                in
                let selected =
                  all |> List.to_seq |> Seq.take max_parts |> List.of_seq
                in
                let is_truncated = List.length all > List.length selected in
                let next_part_number_marker =
                  if not is_truncated then None
                  else
                    match List.rev selected with
                    | [] -> None
                    | part :: _ ->
                        Some
                          (Multipart_model.Part_number_marker.of_int_exn
                             part.part_number)
                in
                let parts =
                  List.map
                    (fun part ->
                      {
                        Multipart_model.List_parts.part_number =
                          Multipart_model.Part_number.of_int_exn
                            part.part_number;
                        etag = Some part.etag;
                        size = Some (Int64.of_int (String.length part.body));
                        last_modified = Some part.last_modified;
                        checksum = part.checksum;
                      })
                    selected
                in
                Ok
                  {
                    Multipart_model.List_parts.parts;
                    is_truncated;
                    next_part_number_marker;
                    checksum_type =
                      Option.map
                        (fun checksum_type ->
                          Object_model.Checksum.Type.Known checksum_type)
                        (Option.bind upload.checksum
                           (fun
                             (checksum : Multipart_model.Create.Checksum.t) ->
                             checksum.checksum_type));
                    response = response 200;
                  }))

  module List_parts = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : Multipart_model.List_parts.options)
        part_number_marker =
      Multipart_model.List_parts.options ?max_parts:base.max_parts
        ?part_number_marker ?expected_bucket_owner:base.expected_bucket_owner ()

    let fold_pages conn ~upload ?options ?max_pages ~init ~f () =
      match validate_max_pages max_pages with
      | Error error -> Error error
      | Ok () ->
          let base =
            Option.value ~default:Multipart_model.List_parts.default_options
              options
          in
          let rec loop part_number_marker page_count acc =
            match options_for_page base part_number_marker with
            | Error error -> Error error
            | Ok options -> (
                match list_parts conn ~upload ~options () with
                | Error error -> Error error
                | Ok page -> (
                    match f acc page with
                    | Error error -> Error error
                    | Ok acc -> (
                        let page_count = page_count + 1 in
                        if not page.is_truncated then Ok acc
                        else
                          match max_pages with
                          | Some max_pages when page_count >= max_pages ->
                              Ok acc
                          | _ -> (
                              match page.next_part_number_marker with
                              | Some marker -> loop (Some marker) page_count acc
                              | None ->
                                  Error
                                    (decode
                                       "truncated list-parts response missing \
                                        NextPartNumberMarker")))))
          in
          loop base.part_number_marker 0 init

    let pages conn ~upload ?options ?max_pages () =
      fold_pages conn ~upload ?options ?max_pages ~init:[]
        ~f:(fun pages page -> Ok (page :: pages))
        ()
      |> Result.map List.rev

    let parts conn ~upload ?options ?max_pages () =
      fold_pages conn ~upload ?options ?max_pages ~init:[]
        ~f:(fun parts (page : Multipart_model.List_parts.page) ->
          Ok (List.rev_append page.parts parts))
        ()
      |> Result.map List.rev
  end
end
