open Common
open Headers
open Response
open Multipart_xml
module Create_multipart_upload = Multipart.Create
module Upload_part = Multipart.Upload_part
module Complete_multipart_upload = Multipart.Complete
module Abort_multipart_upload = Multipart.Abort
module List_parts = Multipart.List_parts

module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t
  type nonrec request_body = request_body

  let part_number_of_string_opt value =
    match int_of_string_opt value with
    | Some value when value > 0 && value <= 10_000 -> Some value
    | _ -> None

  let validate_opt f = function None -> Ok () | Some value -> f value

  let return_result return_error return_ok = function
    | Ok value -> return_ok value
    | Error error -> return_error error

  let with_operation_result return_error return_ok response =
    let* result = response in
    return_result return_error return_ok result

  let bucket_string = Bucket_name.to_string
  let key_string = Object_key.to_string

  let create_upload conn ~bucket ~key ?options () =
    let bucket = bucket_string bucket in
    let key = key_string key in
    let options =
      Option.value ~default:Create_multipart_upload.default_options options
    in
    let return_error =
      return_s3_error return_error ~operation:"CreateMultipartUpload" ~bucket
        ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_metadata options.metadata with
        | Error error -> return_error error
        | Ok () -> (
            match validate_tags options.tags with
            | Error error -> return_error error
            | Ok () -> (
                match
                  ( validate_opt validate_checksum_algorithm
                      options.checksum_algorithm,
                    validate_opt validate_checksum_type options.checksum_type )
                with
                | Error error, _ | _, Error error -> return_error error
                | Ok (), Ok () -> (
                    let headers =
                      Metadata_headers.to_headers options.metadata
                      @ checksum_algorithm_header options.checksum_algorithm
                      @ checksum_type_header options.checksum_type
                      @ encryption_request_headers
                          options.server_side_encryption
                      |> add_opt_content_type_header "content-type"
                           options.content_type
                      |> add_opt_header "x-amz-storage-class"
                           (Option.map Storage_class.to_string
                              options.storage_class)
                      |> add_opt_header "x-amz-tagging"
                           (tags_header options.tags)
                      |> add_opt_account_id_header "x-amz-expected-bucket-owner"
                           options.expected_bucket_owner
                    in
                    match object_request conn ~bucket ~key with
                    | Error error -> return_error error
                    | Ok request ->
                        with_operation_result return_error return_ok
                          (with_empty_response conn ~method_:`POST ~request
                             ~query:[ ("uploads", []) ]
                             ~headers
                             ~f:(fun response body ->
                               let* body =
                                 read_response_body body ~max_size:1_048_576L
                               in
                               match body with
                               | Error error -> return_error error
                               | Ok body -> (
                                   match
                                     Xml.decode_root body
                                       ~name:"InitiateMultipartUploadResult"
                                   with
                                   | Error error -> return_error error
                                   | Ok nodes -> (
                                       match
                                         Xml.child_text "UploadId" nodes
                                       with
                                       | None ->
                                           return_error
                                             (decode "missing UploadId")
                                       | Some upload_id -> (
                                           match
                                             Multipart.Upload_id.of_string
                                               upload_id
                                           with
                                           | Error error -> return_error error
                                           | Ok upload_id ->
                                               return_ok
                                                 {
                                                   Create_multipart_upload
                                                   .upload =
                                                     Multipart.Upload.create_exn
                                                       ~bucket ~key ~upload_id;
                                                   response;
                                                 })))))))))

  let upload_part conn ~bucket ~key ~upload_id ~part_number ~body ?options () =
    let bucket = bucket_string bucket in
    let key = key_string key in
    let options = Option.value ~default:Upload_part.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"UploadPart" ~bucket ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let etag = Object.Etag.of_string_exn "unused" in
        match Multipart.Part.create ~part_number ~etag () with
        | Error error -> return_error error
        | Ok _ -> (
            match validate_opt validate_checksum_value options.checksum with
            | Error error -> return_error error
            | Ok () -> (
                let descriptor = R.Request_body.descriptor body in
                match descriptor.content_length with
                | None ->
                    return_error
                      (Awskit.Error.Internal.validation ~field:"content_length"
                         "S3 multipart uploads require a known content length")
                | Some content_length -> (
                    match
                      Awskit.Body.Request.validate_descriptor descriptor
                    with
                    | Error error -> return_error error
                    | Ok () -> (
                        let headers =
                          ("content-length", Int64.to_string content_length)
                          :: checksum_value_headers options.checksum
                          |> add_opt_account_id_header
                               "x-amz-expected-bucket-owner"
                               options.expected_bucket_owner
                        in
                        let query =
                          [
                            ("partNumber", [ string_of_int part_number ]);
                            ( "uploadId",
                              [ Multipart.Upload_id.to_string upload_id ] );
                          ]
                        in
                        match object_request conn ~bucket ~key with
                        | Error error -> return_error error
                        | Ok request ->
                            with_operation_result return_error return_ok
                              (with_response conn ~method_:`PUT ~request ~query
                                 ~headers ~payload_hash:descriptor.payload_hash
                                 body ~f:(fun response body ->
                                   let* discarded =
                                     discard_response_body body
                                   in
                                   match discarded with
                                   | Error error -> return_error error
                                   | Ok () -> (
                                       match response_etag response with
                                       | Error error -> return_error error
                                       | Ok None ->
                                           return_error
                                             (decode
                                                "missing multipart part etag")
                                       | Ok (Some etag) -> (
                                           match
                                             Multipart.Part.create ~part_number
                                               ~etag ?checksum:options.checksum
                                               ()
                                           with
                                           | Error error -> return_error error
                                           | Ok part ->
                                               return_ok
                                                 {
                                                   Upload_part.part;
                                                   checksum =
                                                     response_checksum response;
                                                   response;
                                                 })))))))))

  let complete_upload conn ~bucket ~key ~upload_id ?options parts =
    let bucket = bucket_string bucket in
    let key = key_string key in
    let options =
      Option.value ~default:Complete_multipart_upload.default_options options
    in
    let return_error =
      return_s3_error return_error ~operation:"CompleteMultipartUpload" ~bucket
        ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match
          ( validate_opt validate_checksum_value options.checksum,
            validate_opt validate_checksum_type options.checksum_type )
        with
        | Error error, _ | _, Error error -> return_error error
        | Ok (), Ok () -> (
            let rec validate previous = function
              | [] -> Ok ()
              | (part : Multipart.Part.t) :: rest ->
                  if part.part_number <= 0 then
                    invalid ~field:"part_number" "part number must be positive"
                  else if
                    match previous with
                    | Some prev -> part.part_number <= prev
                    | None -> false
                  then
                    invalid ~field:"part_number"
                      "parts must be sorted by part_number"
                  else validate (Some part.part_number) rest
            in
            match parts with
            | [] ->
                return_error
                  (Awskit.Error.Internal.validation ~field:"parts"
                     "complete requires at least one part")
            | parts -> (
                match validate None parts with
                | Error error -> return_error error
                | Ok () -> (
                    let part_xml (part : Multipart.Part.t) =
                      let children =
                        [
                          Xml.text "PartNumber" (string_of_int part.part_number);
                          Xml.text "ETag" (Object.Etag.to_string part.etag);
                        ]
                      in
                      let children =
                        match part.checksum with
                        | None -> children
                        | Some checksum -> (
                            match checksum_xml_name checksum.algorithm with
                            | None -> children
                            | Some name ->
                                children @ [ Xml.text name checksum.value ])
                      in
                      Xml.el "Part" children
                    in
                    let body =
                      Xml.el "CompleteMultipartUpload" (List.map part_xml parts)
                      |> Xml.to_string
                    in
                    let upload = R.Request_body.of_string body in
                    match object_request conn ~bucket ~key with
                    | Error error -> return_error error
                    | Ok request ->
                        with_operation_result return_error return_ok
                          (with_response conn ~method_:`POST ~request
                             ~query:
                               [
                                 ( "uploadId",
                                   [ Multipart.Upload_id.to_string upload_id ]
                                 );
                               ]
                             ~headers:
                               ([ ("content-type", "application/xml") ]
                                @ checksum_value_headers options.checksum
                                @ checksum_type_header options.checksum_type
                                @ multipart_object_size_header
                                    options.multipart_object_size
                               |> add_opt_account_id_header
                                    "x-amz-expected-bucket-owner"
                                    options.expected_bucket_owner)
                             ~payload_hash:
                               (R.Request_body.descriptor upload).payload_hash
                             upload
                             ~f:(fun response body ->
                               let* body =
                                 read_response_body body ~max_size:1_048_576L
                               in
                               match body with
                               | Error error -> return_error error
                               | Ok body -> (
                                   match complete_result response body with
                                   | Error error -> return_error error
                                   | Ok result -> return_ok result)))))))

  let abort_upload conn ~bucket ~key ~upload_id ?options () =
    let bucket = bucket_string bucket in
    let key = key_string key in
    let options =
      Option.value ~default:Abort_multipart_upload.default_options options
    in
    let return_error =
      return_s3_error return_error ~operation:"AbortMultipartUpload" ~bucket
        ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`DELETE ~request
                 ~query:
                   [ ("uploadId", [ Multipart.Upload_id.to_string upload_id ]) ]
                 ~headers:
                   ([]
                   |> add_opt_account_id_header "x-amz-expected-bucket-owner"
                        options.expected_bucket_owner)
                 ~f:(fun response body ->
                   let* discarded = discard_response_body body in
                   match discarded with
                   | Error error -> return_error error
                   | Ok () -> return_ok response)))

  let validate_list_parts_options (options : List_parts.options) =
    match options.max_parts with
    | Some value when value <= 0 ->
        invalid ~field:"max_parts" "max_parts must be greater than zero"
    | _ -> (
        match options.part_number_marker with
        | Some value when value < 0 ->
            invalid ~field:"part_number_marker"
              "part_number_marker must be non-negative"
        | _ -> Ok ())

  let list_parts conn ~bucket ~key ~upload_id ?options () =
    let bucket = bucket_string bucket in
    let key = key_string key in
    let options = Option.value ~default:List_parts.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"ListParts" ~bucket ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_list_parts_options options with
        | Error error -> return_error error
        | Ok () -> (
            match object_request conn ~bucket ~key with
            | Error error -> return_error error
            | Ok request ->
                let add name = function
                  | None -> []
                  | Some value -> [ (name, [ string_of_int value ]) ]
                in
                let query =
                  [ ("uploadId", [ Multipart.Upload_id.to_string upload_id ]) ]
                  @ add "max-parts" options.max_parts
                  @ add "part-number-marker" options.part_number_marker
                in
                with_operation_result return_error return_ok
                  (with_empty_response conn ~method_:`GET ~request ~query
                     ~headers:
                       ([]
                       |> add_opt_account_id_header
                            "x-amz-expected-bucket-owner"
                            options.expected_bucket_owner)
                     ~f:(fun response body ->
                       let* body =
                         read_response_body body ~max_size:1_048_576L
                       in
                       match body with
                       | Error error -> return_error error
                       | Ok body -> (
                           match
                             Xml.decode_root body ~name:"ListPartsResult"
                           with
                           | Error error -> return_error error
                           | Ok nodes -> (
                               match
                                 Xml.children_result "Part" nodes
                                   ~f:(fun index nodes ->
                                     let path =
                                       Fmt.str "ListPartsResult.Part[%d]" index
                                     in
                                     match
                                       Xml.optional_child_parse ~path
                                         "PartNumber" part_number_of_string_opt
                                         nodes
                                     with
                                     | Error _ as error -> error
                                     | Ok part_number -> (
                                         match
                                           Xml.optional_child_result ~path
                                             "ETag" Object.Etag.of_string nodes
                                         with
                                         | Error _ as error -> error
                                         | Ok etag -> (
                                             match
                                               Xml.optional_child_parse ~path
                                                 "Size"
                                                 non_negative_int64_of_string_opt
                                                 nodes
                                             with
                                             | Error _ as error -> error
                                             | Ok size -> (
                                                 match
                                                   Xml.optional_child_parse
                                                     ~path "LastModified"
                                                     ptime_of_string nodes
                                                 with
                                                 | Error _ as error -> error
                                                 | Ok last_modified -> (
                                                     match part_number with
                                                     | None ->
                                                         Xml.decode_field_error
                                                           ~path
                                                           "missing required \
                                                            <PartNumber>"
                                                     | Some part_number ->
                                                         Ok
                                                           {
                                                             List_parts
                                                             .part_number;
                                                             etag;
                                                             size;
                                                             last_modified;
                                                             checksum =
                                                               checksum_response_from_xml
                                                                 nodes;
                                                           })))))
                               with
                               | Error error -> return_error error
                               | Ok parts -> (
                                   match
                                     ( Xml.optional_child_parse
                                         ~path:"ListPartsResult" "IsTruncated"
                                         parse_bool nodes,
                                       Xml.optional_child_parse
                                         ~path:"ListPartsResult"
                                         "NextPartNumberMarker"
                                         non_negative_int_of_string_opt nodes )
                                   with
                                   | Error error, _ | _, Error error ->
                                       return_error error
                                   | Ok is_truncated, Ok next_part_number_marker
                                     ->
                                       return_ok
                                         {
                                           List_parts.parts;
                                           is_truncated =
                                             Option.value ~default:false
                                               is_truncated;
                                           next_part_number_marker;
                                           checksum_type =
                                             Option.map
                                               Object.Checksum.Type.of_string
                                               (Xml.child_text "ChecksumType"
                                                  nodes);
                                           response;
                                         })))))))

  module List_parts = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : List_parts.options) part_number_marker =
      { base with List_parts.part_number_marker }

    let fold_pages conn ~bucket ~key ~upload_id ?options ?max_pages ~init ~f ()
        =
      let return_context_error =
        return_s3_error return_error ~operation:"ListParts"
          ~bucket:(bucket_string bucket) ~key:(key_string key)
      in
      match validate_max_pages max_pages with
      | Error error -> return_context_error error
      | Ok () ->
          let base = Option.value ~default:List_parts.default_options options in
          let rec loop part_number_marker page_count acc =
            let options = options_for_page base part_number_marker in
            let* page = list_parts conn ~bucket ~key ~upload_id ~options () in
            match page with
            | Error error -> return_error error
            | Ok page -> (
                let* next_acc = f acc page in
                match next_acc with
                | Error error -> return_context_error error
                | Ok acc -> (
                    let page_count = page_count + 1 in
                    if not page.is_truncated then return_ok acc
                    else
                      match max_pages with
                      | Some max_pages when page_count >= max_pages ->
                          return_ok acc
                      | _ -> (
                          match page.next_part_number_marker with
                          | Some marker -> loop (Some marker) page_count acc
                          | None ->
                              return_context_error
                                (decode
                                   "truncated list-parts response missing \
                                    NextPartNumberMarker"))))
          in
          loop base.part_number_marker 0 init

    let pages conn ~bucket ~key ~upload_id ?options ?max_pages () =
      let f pages page = return_ok (page :: pages) in
      let* result =
        fold_pages conn ~bucket ~key ~upload_id ?options ?max_pages ~init:[] ~f
          ()
      in
      return (Result.map List.rev result)

    let parts conn ~bucket ~key ~upload_id ?options ?max_pages () =
      let f parts (page : List_parts.page) =
        return_ok (List.rev_append page.parts parts)
      in
      let* result =
        fold_pages conn ~bucket ~key ~upload_id ?options ?max_pages ~init:[] ~f
          ()
      in
      return (Result.map List.rev result)
  end
end
