open Common
open Headers
open Response
module Put_object = Object.Put
module Get_object = Object.Get
module Head_object = Object.Head
module Delete_object = Object.Delete
module Delete_objects = Object.Delete_many
module Copy_object = Object.Copy
module List_objects_v2 = Object.List
module List_object_versions = Object.Versions

module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind
  let validate_opt f = function None -> Ok () | Some value -> f value

  type nonrec connection = connection
  type 'a io = 'a R.t
  type nonrec request_body = request_body
  type nonrec response_body_reader = response_body_reader

  let return_result return_error return_ok = function
    | Ok value -> return_ok value
    | Error error -> return_error error

  let validate_put_options (options : Put_object.options) =
    match validate_metadata options.metadata with
    | Error _ as error -> error
    | Ok () -> (
        match validate_tags options.tags with
        | Error _ as error -> error
        | Ok () -> (
            match validate_opt validate_checksum_value options.checksum with
            | Error _ as error -> error
            | Ok () ->
                validate_common_headers ?content_type:options.content_type
                  ?cache_control:options.cache_control
                  ?content_encoding:options.content_encoding
                  ?content_disposition:options.content_disposition ()))

  let put conn ~bucket ~key ?options ~body () =
    let options = Option.value ~default:Put_object.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"PutObject" ~bucket ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_put_options options with
        | Error error -> return_error error
        | Ok () -> (
            let descriptor = R.Request_body.descriptor body in
            match descriptor.content_length with
            | None ->
                return_error
                  (Awskit.Error.validation ~field:"content_length"
                     "S3 uploads require a known content length before SigV4 \
                      chunked streaming")
            | Some content_length -> (
                match Awskit.Body.Request.validate_descriptor descriptor with
                | Error error -> return_error error
                | Ok () -> (
                    let headers =
                      [ ("content-length", Int64.to_string content_length) ]
                      @ Metadata_headers.to_headers options.metadata
                      @ write_precondition_headers options.preconditions
                      @ checksum_value_headers options.checksum
                      @ encryption_request_headers
                          options.server_side_encryption
                      |> add_opt_header "content-type" options.content_type
                      |> add_opt_header "cache-control" options.cache_control
                      |> add_opt_header "content-encoding"
                           options.content_encoding
                      |> add_opt_header "content-disposition"
                           options.content_disposition
                      |> add_opt_header "x-amz-storage-class"
                           (Option.map Storage_class.to_string
                              options.storage_class)
                      |> add_opt_header "x-amz-tagging"
                           (tags_header options.tags)
                      |> add_opt_header "x-amz-expected-bucket-owner"
                           options.expected_bucket_owner
                    in
                    match object_request conn ~bucket ~key with
                    | Error error -> return_error error
                    | Ok request ->
                        with_response conn ~method_:`PUT ~request ~query:[]
                          ~headers ~payload_hash:descriptor.payload_hash body
                          ~f:(fun response body ->
                            let* discarded = discard_response_body body in
                            match discarded with
                            | Error error -> return_error error
                            | Ok () ->
                                return_result return_error return_ok
                                  (put_result response))))))

  let get conn ~bucket ~key ?options ~consume () =
    let options = Option.value ~default:Get_object.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"GetObject" ~bucket ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let headers =
          read_precondition_headers options.preconditions
          @ checksum_mode_header options.checksum_mode
          |> add_opt_header "range" (Option.map Range.to_header options.range)
          |> add_opt_header "x-amz-expected-bucket-owner"
               options.expected_bucket_owner
        in
        let query =
          match options.version_id with
          | None -> []
          | Some version_id ->
              [ ("versionId", [ Object.Version_id.to_string version_id ]) ]
        in
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`GET ~request ~query ~headers
              ~f:(fun response body ->
                match object_info response with
                | Error error -> return_error error
                | Ok info ->
                    let* consumed = R.Response_body.with_reader body ~consume in
                    return_result return_error return_ok
                      (Result.map (fun value -> (info, value)) consumed)))

  let find conn ~bucket ~key ?options ~consume () =
    let options = Option.value ~default:Get_object.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"GetObject" ~bucket ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let headers =
          read_precondition_headers options.preconditions
          @ checksum_mode_header options.checksum_mode
          |> add_opt_header "range" (Option.map Range.to_header options.range)
          |> add_opt_header "x-amz-expected-bucket-owner"
               options.expected_bucket_owner
        in
        let query =
          match options.version_id with
          | None -> []
          | Some version_id ->
              [ ("versionId", [ Object.Version_id.to_string version_id ]) ]
        in
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request -> (
            let* result =
              with_empty_response conn ~method_:`GET ~request ~query ~headers
                ~f:(fun response body ->
                  match object_info response with
                  | Error error -> return_ok (Error error)
                  | Ok info ->
                      let* consumed =
                        R.Response_body.with_reader body ~consume
                      in
                      return_ok
                        (Result.map (fun value -> (info, value)) consumed))
            in
            match result with
            | Ok (Ok value) -> return_ok (Some value)
            | Ok (Error error) -> return_error error
            | Error error when Error.is_no_such_key error -> return_ok None
            | Error error -> return_error error))

  let head conn ~bucket ~key ?options () =
    let options = Option.value ~default:Head_object.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"HeadObject" ~bucket ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let headers =
          read_precondition_headers options.preconditions
          @ checksum_mode_header options.checksum_mode
          |> add_opt_header "x-amz-expected-bucket-owner"
               options.expected_bucket_owner
        in
        let query =
          match options.version_id with
          | None -> []
          | Some version_id ->
              [ ("versionId", [ Object.Version_id.to_string version_id ]) ]
        in
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`HEAD ~request ~query ~headers
              ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () ->
                    return_result return_error return_ok (object_info response))
        )

  let is_head_object_missing error =
    Error.is_no_such_key error
    || Error.service_code error = None
       && Awskit.Error.service_status error = Some 404

  let find_metadata conn ~bucket ~key ?options () =
    let* result = head conn ~bucket ~key ?options () in
    match result with
    | Ok value -> return_ok (Some value)
    | Error error when is_head_object_missing error -> return_ok None
    | Error error -> return_error error

  let exists conn ~bucket ~key =
    let* result = head conn ~bucket ~key () in
    match result with
    | Ok _ -> return_ok true
    | Error error when Error.is_not_found error -> return_ok false
    | Error error -> return_error error

  let delete conn ~bucket ~key ?options () =
    let options = Option.value ~default:Delete_object.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"DeleteObject" ~bucket ~key
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let headers =
          delete_precondition_headers options.preconditions
          |> add_opt_header "x-amz-expected-bucket-owner"
               options.expected_bucket_owner
        in
        let query =
          match options.version_id with
          | None -> []
          | Some version_id ->
              [ ("versionId", [ Object.Version_id.to_string version_id ]) ]
        in
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`DELETE ~request ~query ~headers
              ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () ->
                    return_result return_error return_ok
                      (delete_result response)))

  let delete_objects conn ~bucket ~objects ?options () =
    let options =
      Option.value ~default:Delete_objects.default_options options
    in
    let return_error =
      return_s3_error return_error ~operation:"DeleteObjects" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match Object_delete_xml.validate_objects objects with
        | Error error -> return_error error
        | Ok () -> (
            let body = Object_delete_xml.body objects in
            let headers =
              [
                ("content-md5", content_md5 body);
                ("content-type", "application/xml");
              ]
              |> add_opt_header "x-amz-expected-bucket-owner"
                   options.expected_bucket_owner
            in
            let upload = R.Request_body.of_string body in
            match
              bucket_request conn ~bucket ~suffix:"/" ~signing_suffix:"/"
            with
            | Error error -> return_error error
            | Ok request ->
                with_response conn ~method_:`POST ~request
                  ~query:[ ("delete", []) ]
                  ~headers
                  ~payload_hash:(R.Request_body.descriptor upload).payload_hash
                  upload
                  ~f:(fun response response_body ->
                    let* body =
                      read_response_body response_body ~max_size:1_048_576L
                    in
                    match body with
                    | Error error -> return_error error
                    | Ok body ->
                        return_result return_error return_ok
                          (Object_delete_xml.parse_result ~response body))))

  let copy conn ~source_bucket ~source_key ~destination_bucket ~destination_key
      ?options () =
    let options = Option.value ~default:Copy_object.default_options options in
    let source_error =
      return_s3_error return_error ~operation:"CopyObject" ~bucket:source_bucket
        ~key:source_key
    in
    let return_error =
      return_s3_error return_error ~operation:"CopyObject"
        ~bucket:destination_bucket ~key:destination_key
    in
    match validate_bucket_key source_bucket source_key with
    | Error error -> source_error error
    | Ok () -> (
        match validate_bucket_key destination_bucket destination_key with
        | Error error -> return_error error
        | Ok () -> (
            let copy_source =
              Awskit.Signing.uri_encode ~encode_slash:false
                (Fmt.str "/%s/%s" source_bucket source_key)
            in
            let copy_source =
              match options.source_version_id with
              | None -> copy_source
              | Some version_id ->
                  Fmt.str "%s?versionId=%s" copy_source
                    (Awskit.Signing.uri_encode ~encode_slash:true
                       (Object.Version_id.to_string version_id))
            in
            match
              validate_opt validate_checksum_algorithm
                options.checksum_algorithm
            with
            | Error error -> return_error error
            | Ok () -> (
                let headers =
                  ("x-amz-copy-source", copy_source)
                  :: copy_source_precondition_headers
                       options.source_preconditions
                  @ checksum_algorithm_header options.checksum_algorithm
                  @ encryption_request_headers options.server_side_encryption
                in
                let headers =
                  match options.metadata_directive with
                  | None -> headers
                  | Some `Copy ->
                      ("x-amz-metadata-directive", "COPY") :: headers
                  | Some (`Replace metadata) ->
                      ("x-amz-metadata-directive", "REPLACE")
                      :: Metadata_headers.to_headers metadata
                      @ headers
                in
                let headers =
                  headers
                  |> add_opt_header "x-amz-storage-class"
                       (Option.map Storage_class.to_string options.storage_class)
                  |> add_opt_header "x-amz-expected-bucket-owner"
                       options.expected_bucket_owner
                  |> add_opt_header "x-amz-source-expected-bucket-owner"
                       options.source_expected_bucket_owner
                in
                match
                  object_request conn ~bucket:destination_bucket
                    ~key:destination_key
                with
                | Error error -> return_error error
                | Ok request ->
                    with_empty_response conn ~method_:`PUT ~request ~query:[]
                      ~headers ~f:(fun response body ->
                        let* body =
                          read_response_body body ~max_size:1_048_576L
                        in
                        match body with
                        | Error error -> return_error error
                        | Ok body ->
                            return_result return_error return_ok
                              (copy_result response body)))))

  let list_versions conn ~bucket ?options () =
    let options =
      Option.value ~default:List_object_versions.default_options options
    in
    let return_error =
      return_s3_error return_error ~operation:"ListObjectVersions" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        let add name = function
          | None -> []
          | Some value -> [ (name, [ value ]) ]
        in
        let query =
          [ ("versions", []) ]
          @ add "prefix" options.prefix
          @ add "delimiter" options.delimiter
          @ add "max-keys" (Option.map string_of_int options.max_keys)
          @ add "key-marker" options.key_marker
          @ add "version-id-marker"
              (Option.map Object.Version_id.to_string options.version_id_marker)
        in
        match bucket_request conn ~bucket ~suffix:"/" ~signing_suffix:"/" with
        | Error error -> return_error error
        | Ok request ->
            let headers =
              []
              |> add_opt_header "x-amz-expected-bucket-owner"
                   options.expected_bucket_owner
            in
            with_empty_response conn ~method_:`GET ~request ~query ~headers
              ~f:(fun response body ->
                let* body = read_response_body body ~max_size:4_194_304L in
                match body with
                | Error error -> return_error error
                | Ok body ->
                    return_result return_error return_ok
                      (Object_versions_xml.parse_page ~response body)))

  let list conn ~bucket ?options () =
    let options =
      Option.value ~default:List_objects_v2.default_options options
    in
    let return_error =
      return_s3_error return_error ~operation:"ListObjectsV2" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        let add name = function
          | None -> []
          | Some value -> [ (name, [ value ]) ]
        in
        let query =
          [ ("list-type", [ "2" ]) ]
          @ add "prefix" options.prefix
          @ add "delimiter" options.delimiter
          @ add "max-keys" (Option.map string_of_int options.max_keys)
          @ add "start-after" options.start_after
          @ add "continuation-token" options.continuation_token
        in
        match bucket_request conn ~bucket ~suffix:"/" ~signing_suffix:"/" with
        | Error error -> return_error error
        | Ok request ->
            let headers =
              []
              |> add_opt_header "x-amz-expected-bucket-owner"
                   options.expected_bucket_owner
            in
            with_empty_response conn ~method_:`GET ~request ~query ~headers
              ~f:(fun response body ->
                let* body = read_response_body body ~max_size:4_194_304L in
                match body with
                | Error error -> return_error error
                | Ok body ->
                    return_result return_error return_ok
                      (Object_list_xml.parse_page ~response body)))

  let list_keys conn ~bucket ?options () =
    let* page = list conn ~bucket ?options () in
    return
      (Result.map
         (fun (page : List_objects_v2.page) ->
           List.map
             (fun (o : List_objects_v2.object_summary) -> o.key)
             page.objects)
         page)

  module List_objects_v2 = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : List_objects_v2.options) continuation_token =
      {
        base with
        List_objects_v2.continuation_token;
        start_after =
          (match continuation_token with
          | None -> base.start_after
          | Some _ -> None);
      }

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      let return_context_error =
        return_s3_error return_error ~operation:"ListObjectsV2" ~bucket
      in
      match validate_max_pages max_pages with
      | Error error -> return_context_error error
      | Ok () ->
          let base =
            Option.value ~default:List_objects_v2.default_options options
          in
          let rec loop continuation_token page_count acc =
            let options = options_for_page base continuation_token in
            let* page = list conn ~bucket ~options () in
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
                          match page.next_continuation_token with
                          | Some token -> loop (Some token) page_count acc
                          | None ->
                              return_context_error
                                (decode
                                   "truncated list response missing \
                                    NextContinuationToken"))))
          in
          loop base.continuation_token 0 init

    let pages conn ~bucket ?options ?max_pages () =
      let f pages page = return_ok (page :: pages) in
      let* result =
        fold_pages conn ~bucket ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)

    let objects conn ~bucket ?options ?max_pages () =
      let f objects (page : List_objects_v2.page) =
        return_ok (List.rev_append page.objects objects)
      in
      let* result =
        fold_pages conn ~bucket ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)

    let keys conn ~bucket ?options ?max_pages () =
      let f keys (page : List_objects_v2.page) =
        let page_keys =
          List.map
            (fun (object_ : List_objects_v2.object_summary) -> object_.key)
            page.objects
        in
        return_ok (List.rev_append page_keys keys)
      in
      let* result =
        fold_pages conn ~bucket ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)
  end

  module List_object_versions = struct
    let validate_max_pages = function
      | None -> Ok ()
      | Some value when value > 0 -> Ok ()
      | Some _ ->
          invalid ~field:"max_pages" "max_pages must be greater than zero"

    let options_for_page (base : List_object_versions.options) page =
      {
        base with
        List_object_versions.key_marker =
          page.List_object_versions.next_key_marker;
        version_id_marker = page.next_version_id_marker;
      }

    let fold_pages conn ~bucket ?options ?max_pages ~init ~f () =
      let return_context_error =
        return_s3_error return_error ~operation:"ListObjectVersions" ~bucket
      in
      match validate_max_pages max_pages with
      | Error error -> return_context_error error
      | Ok () ->
          let base =
            Option.value ~default:List_object_versions.default_options options
          in
          let rec loop options page_count acc =
            let* page = list_versions conn ~bucket ~options () in
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
                          match page.next_key_marker with
                          | Some _ ->
                              loop (options_for_page base page) page_count acc
                          | None ->
                              return_context_error
                                (decode
                                   "truncated version listing response missing \
                                    NextKeyMarker"))))
          in
          loop base 0 init

    let pages conn ~bucket ?options ?max_pages () =
      let f pages page = return_ok (page :: pages) in
      let* result =
        fold_pages conn ~bucket ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)

    let object_versions conn ~bucket ?options ?max_pages () =
      let f versions (page : List_object_versions.page) =
        return_ok (List.rev_append page.versions versions)
      in
      let* result =
        fold_pages conn ~bucket ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)

    let delete_markers conn ~bucket ?options ?max_pages () =
      let f markers (page : List_object_versions.page) =
        return_ok (List.rev_append page.delete_markers markers)
      in
      let* result =
        fold_pages conn ~bucket ?options ?max_pages ~init:[] ~f ()
      in
      return (Result.map List.rev result)
  end

  module Tagging = Object_tagging_request.Make (C)
end
