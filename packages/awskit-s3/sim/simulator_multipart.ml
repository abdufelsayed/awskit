open Awskit_s3
open Simulator_support
open Simulator_headers
open Simulator_state
open Simulator_error
open Simulator_store
open Simulator_checksum
open Simulator_runtime
module Multipart_model = Multipart
module Object_model = Object

module Multipart = struct
  type connection = t
  type 'a io = 'a
  type request_body = Runtime.request_body

  let validate_opt f = function None -> Ok () | Some value -> f value

  let validate_create_options (options : Create_multipart_upload.options) =
    let* () = validate_metadata options.metadata in
    let* () = validate_tags options.tags in
    let* () =
      validate_opt validate_checksum_algorithm options.checksum_algorithm
    in
    validate_opt validate_checksum_type options.checksum_type

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
    Ok (Multipart_model.Upload.created ~bucket ~key ~upload_id)

  let create_upload conn ~bucket ~key ?options () =
    let options =
      Option.value ~default:Create_multipart_upload.default_options options
    in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_bucket conn bucket with
        | Error error -> Error error
        | Ok bucket_state -> (
            match validate_create_options options with
            | Error error -> Error error
            | Ok () -> (
                match
                  operation_fault conn `Create_multipart_upload bucket
                    (Some key)
                with
                | Some error -> Error error
                | None -> (
                    let upload_id = next_upload_id conn in
                    match created_upload_of_strings ~bucket ~key ~upload_id with
                    | Error error -> Error error
                    | Ok upload ->
                        Hashtbl.replace bucket_state.multipart_uploads
                          (upload_key upload_id)
                          {
                            upload;
                            content_type = options.content_type;
                            metadata = options.metadata;
                            storage_class = options.storage_class;
                            tags = options.tags;
                            checksum_algorithm = options.checksum_algorithm;
                            checksum_type = options.checksum_type;
                            parts = Hashtbl.create 17;
                            created_at = now conn;
                          };
                        Ok
                          {
                            Create_multipart_upload.upload;
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

  let upload_part conn ~upload ~part_number ~body ?options () =
    let options = Option.value ~default:Upload_part.default_options options in
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    let part_number_int = Multipart_model.Part_number.to_int part_number in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match validate_opt validate_checksum_value options.checksum with
        | Error error -> Error error
        | Ok () -> (
            match require_multipart_upload conn ~bucket ~key ~upload_id with
            | Error error -> Error error
            | Ok (_bucket_state, stored_upload) -> (
                match operation_fault conn `Upload_part bucket (Some key) with
                | Some error -> Error error
                | None -> (
                    match request_body_result body with
                    | Error error -> Error error
                    | Ok body ->
                        let etag = etag body in
                        let checksum = checksum_for_value options.checksum in
                        let part =
                          {
                            part_number = part_number_int;
                            body;
                            etag;
                            checksum;
                            last_modified = now conn;
                          }
                        in
                        Hashtbl.replace stored_upload.parts part_number_int part;
                        let size = Int64.of_int (String.length body) in
                        let part =
                          Multipart_model.Part.create_exn ~part_number ~etag
                            ?checksum:options.checksum ~size ()
                        in
                        Ok
                          {
                            Upload_part.part;
                            checksum;
                            response =
                              response 200
                                ~headers:
                                  (("etag", Object.Etag.to_string etag)
                                  :: checksum_response_headers checksum);
                          }))))

  let validate_complete_options (options : Complete_multipart_upload.options) =
    Complete_multipart_upload.options
      ?expected_bucket_owner:options.expected_bucket_owner
      ?checksum:options.checksum ?checksum_type:options.checksum_type
      ?multipart_object_size:options.multipart_object_size ()
    |> Result.map ignore

  let validate_complete_parts upload options parts =
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
    let checksum_value_equal (left : Object_model.Checksum.value) right =
      left.algorithm = right.Object_model.Checksum.algorithm
      && String.equal left.value right.value
    in
    let validate_part_checksum stored (part : Multipart_model.Part.t) =
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
    let rec loop previous total = function
      | [] -> Ok total
      | (part : Multipart_model.Part.t) :: rest -> (
          let part_number = completed_part_number part in
          match previous with
          | Some previous when part_number <= previous ->
              invalid ~field:"part_number" "parts must be sorted by part_number"
          | _ -> (
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
                      if
                        rest <> []
                        && String.length stored.body < complete_min_part_size
                      then invalid_part_size ()
                      else
                        let total =
                          Int64.add total
                            (Int64.of_int (String.length stored.body))
                        in
                        loop (Some part_number) total rest)))
    in
    match parts with
    | [] ->
        Error
          (Awskit.Error.Producer.validation ~field:"parts"
             "complete requires at least one part")
    | parts -> (
        match loop None 0L parts with
        | Error _ as error -> error
        | Ok total -> (
            match options.Complete_multipart_upload.multipart_object_size with
            | Some expected ->
                if Int64.equal expected total then Ok ()
                else invalid_object_size ()
            | _ -> Ok ()))

  let complete_upload conn ~upload ?options ~parts () =
    let options =
      Option.value ~default:Complete_multipart_upload.default_options options
    in
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match validate_complete_options options with
        | Error error -> Error error
        | Ok () -> (
            match require_multipart_upload conn ~bucket ~key ~upload_id with
            | Error error -> Error error
            | Ok (bucket_state, upload) -> (
                match validate_complete_parts upload options parts with
                | Error error -> Error error
                | Ok () -> (
                    match
                      operation_fault conn `Complete_multipart_upload bucket
                        (Some key)
                    with
                    | Some error -> Error error
                    | None ->
                        let body =
                          parts
                          |> List.map (fun (part : Multipart_model.Part.t) ->
                              (Hashtbl.find upload.parts
                                 (completed_part_number part))
                                .body)
                          |> String.concat ""
                        in
                        let etag = etag body in
                        let checksum =
                          match checksum_for_value options.checksum with
                          | { Object.Checksum.values = []; _ } ->
                              let checksum =
                                checksum_for_algorithm ~body
                                  upload.checksum_algorithm
                              in
                              {
                                checksum with
                                checksum_type =
                                  (match options.checksum_type with
                                  | Some _ as value -> value
                                  | None -> upload.checksum_type);
                              }
                          | checksum ->
                              {
                                checksum with
                                checksum_type =
                                  (match options.checksum_type with
                                  | Some _ as value -> value
                                  | None -> upload.checksum_type);
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
                        let obj = store_object conn bucket_state key obj in
                        Hashtbl.remove bucket_state.multipart_uploads
                          (upload_key upload_id);
                        Ok
                          {
                            Complete_multipart_upload.etag = Some etag;
                            version_id = obj.version_id;
                            checksum;
                            response =
                              response 200
                                ~headers:
                                  (version_headers obj.version_id
                                  @ checksum_response_headers checksum);
                          }))))

  let abort_upload conn ~upload ?options:_ () =
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match require_multipart_upload conn ~bucket ~key ~upload_id with
        | Error error -> Error error
        | Ok (bucket_state, _upload) -> (
            match
              operation_fault conn `Abort_multipart_upload bucket (Some key)
            with
            | Some error -> Error error
            | None ->
                Hashtbl.remove bucket_state.multipart_uploads
                  (upload_key upload_id);
                Ok { Abort_multipart_upload.response = response 204 }))

  let validate_list_parts_options (options : List_parts.options) =
    match options.max_parts with
    | Some value when value <= 0 ->
        invalid ~field:"max_parts" "max_parts must be greater than zero"
    | Some value when value > 1000 ->
        invalid ~field:"max_parts" "max_parts must be at most 1000"
    | _ -> Ok ()

  let list_parts conn ~upload ?options () =
    let options = Option.value ~default:List_parts.default_options options in
    let bucket = upload_handle_bucket upload in
    let key = upload_handle_key upload in
    let upload_id = upload_handle_id upload in
    match validate_bucket_key bucket key with
    | Error error -> Error error
    | Ok () -> (
        match validate_list_parts_options options with
        | Error error -> Error error
        | Ok () -> (
            match require_multipart_upload conn ~bucket ~key ~upload_id with
            | Error error -> Error error
            | Ok (_bucket_state, upload) -> (
                match operation_fault conn `List_parts bucket (Some key) with
                | Some error -> Error error
                | None ->
                    let max_parts =
                      Option.value ~default:(config (store conn)).max_list_keys
                        options.max_parts
                    in
                    let all =
                      Hashtbl.to_seq_values upload.parts
                      |> List.of_seq
                      |> List.sort (fun a b ->
                          Int.compare a.part_number b.part_number)
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
                            List_parts.part_number =
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
                        List_parts.parts;
                        is_truncated;
                        next_part_number_marker;
                        checksum_type = upload.checksum_type;
                        response = response 200;
                      })))

  module List_parts = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : List_parts.options) part_number_marker =
      { base with List_parts.part_number_marker }

    let fold_pages conn ~upload ?options ?max_pages ~init ~f () =
      match validate_max_pages max_pages with
      | Error error -> Error error
      | Ok () ->
          let base = Option.value ~default:List_parts.default_options options in
          let rec loop part_number_marker page_count acc =
            let options = options_for_page base part_number_marker in
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
                      | Some max_pages when page_count >= max_pages -> Ok acc
                      | _ -> (
                          match page.next_part_number_marker with
                          | Some marker -> loop (Some marker) page_count acc
                          | None ->
                              Error
                                (decode
                                   "truncated list-parts response missing \
                                    NextPartNumberMarker"))))
          in
          loop base.part_number_marker 0 init

    let pages conn ~upload ?options ?max_pages () =
      fold_pages conn ~upload ?options ?max_pages ~init:[]
        ~f:(fun pages page -> Ok (page :: pages))
        ()
      |> Result.map List.rev

    let parts conn ~upload ?options ?max_pages () =
      fold_pages conn ~upload ?options ?max_pages ~init:[]
        ~f:(fun parts (page : List_parts.page) ->
          Ok (List.rev_append page.parts parts))
        ()
      |> Result.map List.rev
  end
end
