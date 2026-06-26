open Headers
open Response
open Multipart_xml
module Metadata_headers = S3_metadata_headers
module Xml = S3_xml
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
    match S3_parse.int_of_string_opt value with
    | Some value -> Result.to_option (Multipart.Part_number.of_int value)
    | _ -> None

  let part_number_marker_of_string_opt value =
    match S3_parse.int_of_string_opt value with
    | Some value -> Result.to_option (Multipart.Part_number_marker.of_int value)
    | _ -> None

  let validate_opt f = function None -> Ok () | Some value -> f value

  let return_result return_error return_ok = function
    | Ok value -> return_ok value
    | Error error -> return_error error

  let with_operation_result return_error return_ok response =
    let* result = response in
    return_result return_error return_ok result

  let upload_bucket upload =
    Multipart.Upload.bucket upload |> Bucket_name.to_string

  let upload_key upload = Multipart.Upload.key upload |> Object_key.to_string
  let upload_id upload = Multipart.Upload.upload_id upload
  let complete_min_part_size = 5_242_880L

  let validate_create_options (options : Create_multipart_upload.options) =
    match S3_validation.validate_metadata options.metadata with
    | Error _ as error -> error
    | Ok () -> (
        match S3_validation.validate_tags options.tags with
        | Error _ as error -> error
        | Ok () -> (
            match validate_opt validate_storage_class options.storage_class with
            | Error _ as error -> error
            | Ok () -> (
                match
                  ( validate_opt validate_checksum_algorithm
                      options.checksum_algorithm,
                    validate_opt validate_checksum_type options.checksum_type )
                with
                | Error error, _ | _, Error error -> Error error
                | Ok (), Ok () ->
                    validate_destination_encryption options.encryption)))

  let validate_complete_options (options : Complete_multipart_upload.options) =
    Complete_multipart_upload.options
      ?expected_bucket_owner:options.expected_bucket_owner
      ?checksum:options.checksum ?checksum_type:options.checksum_type
      ?customer_key:options.customer_key
      ?multipart_object_size:options.multipart_object_size ()
    |> Result.map ignore

  let validate_complete_parts ?multipart_object_size parts =
    let invalid_part_size () =
      S3_error_context.invalid ~field:"parts"
        "non-final multipart parts must be at least 5 MiB"
    in
    let invalid_object_size () =
      S3_error_context.invalid ~field:"multipart_object_size"
        "multipart object size does not match completed part sizes"
    in
    let rec loop previous total all_sizes_known = function
      | [] -> (
          match multipart_object_size with
          | Some expected
            when all_sizes_known && not (Int64.equal expected total) ->
              invalid_object_size ()
          | _ -> Ok ())
      | (part : Multipart.Part.t) :: rest -> (
          let part_number =
            Multipart.Part.part_number part |> Multipart.Part_number.to_int
          in
          if
            match previous with
            | Some prev -> part_number <= prev
            | None -> false
          then
            S3_error_context.invalid ~field:"part_number"
              "parts must be sorted by part_number"
          else
            match (rest, Multipart.Part.size part) with
            | _ :: _, Some size
              when Int64.compare size complete_min_part_size < 0 ->
                invalid_part_size ()
            | _ ->
                let total, all_sizes_known =
                  match Multipart.Part.size part with
                  | None -> (total, false)
                  | Some size -> (Int64.add total size, all_sizes_known)
                in
                loop (Some part_number) total all_sizes_known rest)
    in
    match parts with
    | [] ->
        S3_error_context.invalid ~field:"parts"
          "complete requires at least one part"
    | parts -> loop None 0L true parts

  let create_upload_result ~typed_bucket ~typed_key response body =
    match Xml.decode_root body ~name:"InitiateMultipartUploadResult" with
    | Error _ as error -> error
    | Ok nodes -> (
        match Xml.child_text "UploadId" nodes with
        | None -> Error (S3_error_context.decode "missing UploadId")
        | Some upload_id -> (
            match Multipart.Upload_id.of_string upload_id with
            | Error _ as error -> error
            | Ok upload_id ->
                Ok
                  {
                    Create_multipart_upload.upload =
                      Multipart.Upload.created ~bucket:typed_bucket
                        ~key:typed_key ~upload_id;
                    response;
                  }))

  let create_upload conn ~bucket ~key ?options () =
    let typed_bucket = bucket in
    let typed_key = key in
    let bucket = Bucket_name.to_string typed_bucket in
    let key = Object_key.to_string typed_key in
    let options =
      Option.value ~default:Create_multipart_upload.default_options options
    in
    let return_error =
      S3_error_context.return_s3_error return_error
        ~operation:"CreateMultipartUpload" ~bucket ~key
    in
    match S3_validation.validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_create_options options with
        | Error error -> return_error error
        | Ok () -> (
            let headers =
              Metadata_headers.to_headers options.metadata
              @ checksum_algorithm_header options.checksum_algorithm
              @ checksum_type_header options.checksum_type
              @ destination_encryption_headers options.encryption
              |> add_opt_content_type_header "content-type" options.content_type
              |> add_opt_header "x-amz-storage-class"
                   (Option.map Storage_class.to_string options.storage_class)
              |> add_opt_header "x-amz-tagging" (tags_header options.tags)
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
                       | Ok body ->
                           return_result return_error return_ok
                             (create_upload_result ~typed_bucket ~typed_key
                                response body)))))

  let upload_part conn ~upload ~part_number ~body ?options () =
    let bucket = upload_bucket upload in
    let key = upload_key upload in
    let upload_id = upload_id upload in
    let options = Option.value ~default:Upload_part.default_options options in
    let return_error =
      S3_error_context.return_s3_error return_error ~operation:"UploadPart"
        ~bucket ~key
    in
    let handle_response ~content_length response body =
      let* discarded = discard_response_body body in
      match discarded with
      | Error error -> return_error error
      | Ok () -> (
          match response_etag response with
          | Error error -> return_error error
          | Ok None ->
              return_error
                (S3_error_context.decode "missing multipart part etag")
          | Ok (Some etag) -> (
              let checksum = response_checksum response in
              let part_checksum =
                match checksum.values with
                | [] -> options.checksum
                | value :: _ -> Some value
              in
              match
                Multipart.Part.create ~part_number ~etag ?checksum:part_checksum
                  ~size:content_length ()
              with
              | Error error -> return_error error
              | Ok part -> return_ok { Upload_part.part; checksum; response }))
    in
    let send ~content_length (descriptor : Awskit.Body.Request.descriptor) =
      let headers =
        ("content-length", Int64.to_string content_length)
        :: checksum_value_headers options.checksum
        @ customer_key_headers options.customer_key
        |> add_opt_account_id_header "x-amz-expected-bucket-owner"
             options.expected_bucket_owner
      in
      let query =
        [
          ( "partNumber",
            [ part_number |> Multipart.Part_number.to_int |> string_of_int ] );
          ("uploadId", [ Multipart.Upload_id.to_string upload_id ]);
        ]
      in
      match object_request conn ~bucket ~key with
      | Error error -> return_error error
      | Ok request ->
          with_operation_result return_error return_ok
            (with_response conn ~method_:`PUT ~request ~query ~headers
               ~payload_hash:descriptor.payload_hash body
               ~f:(handle_response ~content_length))
    in
    match S3_validation.validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_opt validate_checksum_value options.checksum with
        | Error error -> return_error error
        | Ok () -> (
            let descriptor = R.Request_body.descriptor body in
            match descriptor.content_length with
            | None ->
                return_error
                  (Awskit.Error.Producer.validation ~field:"content_length"
                     "S3 multipart uploads require a known content length")
            | Some content_length -> (
                match Awskit.Body.Request.validate_descriptor descriptor with
                | Error error -> return_error error
                | Ok () -> send ~content_length descriptor)))

  let complete_upload conn ~upload ?options ~parts () =
    let bucket = upload_bucket upload in
    let key = upload_key upload in
    let upload_id = upload_id upload in
    let options =
      Option.value ~default:Complete_multipart_upload.default_options options
    in
    let return_error =
      S3_error_context.return_s3_error return_error
        ~operation:"CompleteMultipartUpload" ~bucket ~key
    in
    match S3_validation.validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_complete_options options with
        | Error error -> return_error error
        | Ok () -> (
            match
              validate_complete_parts
                ?multipart_object_size:options.multipart_object_size parts
            with
            | Error error -> return_error error
            | Ok () -> (
                let part_xml (part : Multipart.Part.t) =
                  let part_number =
                    Multipart.Part.part_number part
                    |> Multipart.Part_number.to_int
                  in
                  let children =
                    [
                      Xml.text "PartNumber" (string_of_int part_number);
                      Xml.text "ETag"
                        (Object.Etag.to_string (Multipart.Part.etag part));
                    ]
                  in
                  let children =
                    match Multipart.Part.checksum part with
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
                      (with_retryable_embedded_response conn ~method_:`POST
                         ~request
                         ~query:
                           [
                             ( "uploadId",
                               [ Multipart.Upload_id.to_string upload_id ] );
                           ]
                         ~headers:
                           ([ ("content-type", "application/xml") ]
                            @ checksum_value_headers options.checksum
                            @ checksum_type_header options.checksum_type
                            @ customer_key_headers options.customer_key
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
                               | Ok result -> return_ok result))))))

  let abort_upload conn ~upload ?options () =
    let bucket = upload_bucket upload in
    let key = upload_key upload in
    let upload_id = upload_id upload in
    let options =
      Option.value ~default:Abort_multipart_upload.default_options options
    in
    let return_error =
      S3_error_context.return_s3_error return_error
        ~operation:"AbortMultipartUpload" ~bucket ~key
    in
    match S3_validation.validate_bucket_key bucket key with
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
                   | Ok () -> return_ok { Abort_multipart_upload.response })))

  let validate_list_parts_options (options : List_parts.options) =
    List_parts.options ?max_parts:options.max_parts
      ?part_number_marker:options.part_number_marker
      ?expected_bucket_owner:options.expected_bucket_owner ()
    |> Result.map ignore

  let list_parts conn ~upload ?options () =
    let bucket = upload_bucket upload in
    let key = upload_key upload in
    let upload_id = upload_id upload in
    let options = Option.value ~default:List_parts.default_options options in
    let return_error =
      S3_error_context.return_s3_error return_error ~operation:"ListParts"
        ~bucket ~key
    in
    match S3_validation.validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_list_parts_options options with
        | Error error -> return_error error
        | Ok () -> (
            match object_request conn ~bucket ~key with
            | Error error -> return_error error
            | Ok request ->
                let add_int name = function
                  | None -> []
                  | Some value -> [ (name, [ string_of_int value ]) ]
                in
                let add_part_number_marker name = function
                  | None -> []
                  | Some value ->
                      [
                        ( name,
                          [
                            value
                            |> Multipart.Part_number_marker.to_int
                            |> string_of_int;
                          ] );
                      ]
                in
                let query =
                  [ ("uploadId", [ Multipart.Upload_id.to_string upload_id ]) ]
                  @ add_int "max-parts" options.max_parts
                  @ add_part_number_marker "part-number-marker"
                      options.part_number_marker
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
                                                 S3_parse
                                                 .non_negative_int64_of_string_opt
                                                 nodes
                                             with
                                             | Error _ as error -> error
                                             | Ok size -> (
                                                 match
                                                   Xml.optional_child_parse
                                                     ~path "LastModified"
                                                     S3_time.of_string nodes
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
                                         part_number_marker_of_string_opt nodes
                                     )
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
          S3_error_context.invalid ~field:"max_pages"
            "max_pages must be greater than zero"

    let max_pages_exceeded max_pages =
      Awskit.Error.Producer.validation ~field:"max_pages"
        (Fmt.str "ListParts collection exceeded max_pages bound (%d)" max_pages)

    let options_for_page (base : List_parts.options) part_number_marker =
      { base with List_parts.part_number_marker }

    let fold_pages conn ~upload ?options ?max_pages ~init ~f () =
      let return_context_error =
        S3_error_context.return_s3_error return_error ~operation:"ListParts"
          ~bucket:(upload_bucket upload) ~key:(upload_key upload)
      in
      match validate_max_pages max_pages with
      | Error error -> return_context_error error
      | Ok () ->
          let base = Option.value ~default:List_parts.default_options options in
          let rec loop part_number_marker page_count acc =
            let options = options_for_page base part_number_marker in
            let* page = list_parts conn ~upload ~options () in
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
                                (S3_error_context.decode
                                   "truncated list-parts response missing \
                                    NextPartNumberMarker"))))
          in
          loop base.part_number_marker 0 init

    let collect_pages conn ~upload ?options ?max_pages ~init ~f () =
      let return_context_error =
        S3_error_context.return_s3_error return_error ~operation:"ListParts"
          ~bucket:(upload_bucket upload) ~key:(upload_key upload)
      in
      match validate_max_pages max_pages with
      | Error error -> return_context_error error
      | Ok () ->
          let base = Option.value ~default:List_parts.default_options options in
          let rec loop part_number_marker page_count acc =
            let options = options_for_page base part_number_marker in
            let* page = list_parts conn ~upload ~options () in
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
                          return_context_error (max_pages_exceeded max_pages)
                      | _ -> (
                          match page.next_part_number_marker with
                          | Some marker -> loop (Some marker) page_count acc
                          | None ->
                              return_context_error
                                (S3_error_context.decode
                                   "truncated list-parts response missing \
                                    NextPartNumberMarker"))))
          in
          loop base.part_number_marker 0 init

    let pages conn ~upload ?options ?max_pages () =
      let f pages page = return_ok (page :: pages) in
      let* result =
        collect_pages conn ~upload ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)

    let parts conn ~upload ?options ?max_pages () =
      let f parts (page : List_parts.page) =
        return_ok (List.rev_append page.parts parts)
      in
      let* result =
        collect_pages conn ~upload ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)
  end
end
