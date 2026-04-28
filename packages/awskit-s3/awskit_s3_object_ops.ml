open Awskit_s3_core

module Make (C : Awskit_s3_operation_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t
  type nonrec upload_body = upload_body
  type nonrec download_reader = download_reader

  let validate_put_options (options : Object.Put.options) =
    match validate_metadata options.metadata with
    | Error _ as error -> error
    | Ok () -> (
        match validate_tags options.tags with
        | Error _ as error -> error
        | Ok () ->
            validate_common_headers ?content_type:options.content_type
              ?cache_control:options.cache_control
              ?content_encoding:options.content_encoding
              ?content_disposition:options.content_disposition ())

  let put conn ~bucket ~key ?options ~body () =
    let options = Option.value ~default:Object.Put.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_put_options options with
        | Error error -> return_error error
        | Ok () -> (
            let descriptor = R.upload_descriptor body in
            match descriptor.content_length with
            | None ->
                return_error
                  (Awskit.Error.validation ~field:"content_length"
                     "S3 uploads require a known content length before SigV4 \
                      chunked streaming")
            | Some content_length -> (
                let headers =
                  [ ("content-length", Int64.to_string content_length) ]
                  @ Metadata_headers.to_headers options.metadata
                  @ write_precondition_headers options.preconditions
                  @ checksum_request_headers options.checksum
                  @ encryption_request_headers options.server_side_encryption
                  |> add_opt_header "content-type" options.content_type
                  |> add_opt_header "cache-control" options.cache_control
                  |> add_opt_header "content-encoding" options.content_encoding
                  |> add_opt_header "content-disposition"
                       options.content_disposition
                  |> add_opt_header "x-amz-storage-class"
                       (Option.map Storage_class.to_string options.storage_class)
                  |> add_opt_header "x-amz-tagging" (tags_header options.tags)
                in
                match object_request conn ~bucket ~key with
                | Error error -> return_error error
                | Ok request -> (
                    let* result =
                      call conn ~method_:`PUT ~request ~query:[] ~headers
                        ~payload_hash:descriptor.payload_hash body
                    in
                    match result with
                    | Error error -> return_error error
                    | Ok (response, body) -> (
                        let* discarded = discard_download_body body in
                        match discarded with
                        | Error error -> return_error error
                        | Ok () -> return (put_result response))))))

  let get conn ~bucket ~key ?options ~consume () =
    let options = Option.value ~default:Object.Get.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let headers =
          read_precondition_headers options.preconditions
          |> add_opt_header "range" (Option.map Range.to_header options.range)
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
              call_empty conn ~method_:`GET ~request ~query ~headers
            in
            match result with
            | Error error -> return_error error
            | Ok (response, body) ->
                if Awskit.Response.is_success response then
                  match object_info response with
                  | Error error -> return_error error
                  | Ok info ->
                      let* consumed = R.with_download_body body ~consume in
                      return (Result.map (fun value -> (info, value)) consumed)
                else error_response response body))

  let head conn ~bucket ~key ?options () =
    let options = Option.value ~default:Object.Head.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let headers = read_precondition_headers options.preconditions in
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
              call_empty conn ~method_:`HEAD ~request ~query ~headers
            in
            match result with
            | Error error -> return_error error
            | Ok (response, body) -> (
                let* discarded = discard_download_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return (object_info response))))

  let exists conn ~bucket ~key =
    let* result = head conn ~bucket ~key () in
    match result with
    | Ok _ -> return_ok true
    | Error error when Error.is_not_found error -> return_ok false
    | Error error -> return_error error

  let delete conn ~bucket ~key ?options () =
    let options = Option.value ~default:Object.Delete.default_options options in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let headers = delete_precondition_headers options.preconditions in
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
              call_empty conn ~method_:`DELETE ~request ~query ~headers
            in
            match result with
            | Error error -> return_error error
            | Ok (response, body) -> (
                let* discarded = discard_download_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return (delete_result response))))

  let delete_many conn ~bucket ~objects =
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match Awskit_s3_object_delete_xml.validate_objects objects with
        | Error error -> return_error error
        | Ok () -> (
            let body = Awskit_s3_object_delete_xml.body objects in
            let headers =
              [
                ("content-md5", content_md5 body);
                ("content-type", "application/xml");
              ]
            in
            let upload = R.string_body body in
            match
              bucket_request conn ~bucket ~suffix:"/" ~signing_suffix:"/"
            with
            | Error error -> return_error error
            | Ok request -> (
                let* result =
                  call conn ~method_:`POST ~request
                    ~query:[ ("delete", []) ]
                    ~headers
                    ~payload_hash:(R.upload_descriptor upload).payload_hash
                    upload
                in
                match result with
                | Error error -> return_error error
                | Ok (response, response_body) -> (
                    if not (Awskit.Response.is_success response) then
                      error_response response response_body
                    else
                      let* body =
                        read_download_body response_body ~max_size:1_048_576L
                      in
                      match body with
                      | Error error -> return_error error
                      | Ok body ->
                          return
                            (Awskit_s3_object_delete_xml.parse_result
                               ~request:response body)))))

  let copy conn ~src_bucket ~src_key ~dst_bucket ~dst_key ?options () =
    let options = Option.value ~default:Object.Copy.default_options options in
    match validate_bucket_key src_bucket src_key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_bucket_key dst_bucket dst_key with
        | Error error -> return_error error
        | Ok () -> (
            let copy_source =
              Awskit.Signing.uri_encode ~encode_slash:false
                (Fmt.str "/%s/%s" src_bucket src_key)
            in
            let headers =
              ("x-amz-copy-source", copy_source)
              :: copy_source_precondition_headers options.source_preconditions
              @ checksum_request_headers options.checksum
              @ encryption_request_headers options.server_side_encryption
            in
            let headers =
              match options.metadata with
              | None -> headers
              | Some `Copy -> ("x-amz-metadata-directive", "COPY") :: headers
              | Some (`Replace metadata) ->
                  ("x-amz-metadata-directive", "REPLACE")
                  :: Metadata_headers.to_headers metadata
                  @ headers
            in
            let headers =
              headers
              |> add_opt_header "x-amz-storage-class"
                   (Option.map Storage_class.to_string options.storage_class)
              |> add_opt_header "x-amz-tagging"
                   (Option.bind options.tags tags_header)
            in
            match object_request conn ~bucket:dst_bucket ~key:dst_key with
            | Error error -> return_error error
            | Ok request -> (
                let* result =
                  call_empty conn ~method_:`PUT ~request ~query:[] ~headers
                in
                match result with
                | Error error -> return_error error
                | Ok (response, body) -> (
                    if not (Awskit.Response.is_success response) then
                      error_response response body
                    else
                      let* body =
                        read_download_body body ~max_size:1_048_576L
                      in
                      match body with
                      | Error error -> return_error error
                      | Ok body -> return (copy_result response body)))))

  let list conn ~bucket ?options () =
    let options = Option.value ~default:Object.List.default_options options in
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
        | Ok request -> (
            let* result =
              call_empty conn ~method_:`GET ~request ~query ~headers:[]
            in
            match result with
            | Error error -> return_error error
            | Ok (response, body) -> (
                if not (Awskit.Response.is_success response) then
                  error_response response body
                else
                  let* body = read_download_body body ~max_size:4_194_304L in
                  match body with
                  | Error error -> return_error error
                  | Ok body ->
                      return
                        (Awskit_s3_object_list_xml.parse_page ~request:response
                           body))))

  let list_keys conn ~bucket ?options () =
    let* page = list conn ~bucket ?options () in
    return
      (Result.map
         (fun (page : Object.List.page) ->
           List.map (fun (o : Object.List.object_summary) -> o.key) page.objects)
         page)

  module Buffer = struct
    let put_string conn ~bucket ~key ?options body =
      put conn ~bucket ~key ?options ~body:(R.string_body body) ()

    let put_bytes conn ~bucket ~key ?options body =
      put conn ~bucket ~key ?options ~body:(R.bytes_body body) ()

    let get_string conn ~bucket ~key ~max_size ?options () =
      let consume reader = read_body reader ~max_size in
      get conn ~bucket ~key ?options ~consume ()

    let get_bytes conn ~bucket ~key ~max_size ?options () =
      let* result = get_string conn ~bucket ~key ~max_size ?options () in
      return
        (Result.map (fun (info, body) -> (info, Bytes.of_string body)) result)
  end

  module Tagging = struct
    let get conn ~bucket ~key =
      match validate_bucket_key bucket key with
      | Error error -> return_error error
      | Ok () -> (
          match object_request conn ~bucket ~key with
          | Error error -> return_error error
          | Ok request -> (
              let* result =
                call_empty conn ~method_:`GET ~request
                  ~query:[ ("tagging", []) ]
                  ~headers:[]
              in
              match result with
              | Error error -> return_error error
              | Ok (response, body) -> (
                  if not (Awskit.Response.is_success response) then
                    error_response response body
                  else
                    let* body = read_download_body body ~max_size:1_048_576L in
                    match body with
                    | Error error -> return_error error
                    | Ok body ->
                        return
                          (Result.map
                             (fun tags ->
                               { Object.Tagging.tags; request = response })
                             (parse_tags body)))))

    let put conn ~bucket ~key tags =
      match validate_bucket_key bucket key with
      | Error error -> return_error error
      | Ok () -> (
          match validate_tags tags with
          | Error error -> return_error error
          | Ok () -> (
              let body = xml_tags tags in
              let upload = R.string_body body in
              let headers =
                [
                  ("content-md5", content_md5 body);
                  ("content-type", "application/xml");
                ]
              in
              match object_request conn ~bucket ~key with
              | Error error -> return_error error
              | Ok request -> (
                  let* result =
                    call conn ~method_:`PUT ~request
                      ~query:[ ("tagging", []) ]
                      ~headers
                      ~payload_hash:(R.upload_descriptor upload).payload_hash
                      upload
                  in
                  match result with
                  | Error error -> return_error error
                  | Ok (response, body) -> (
                      let* discarded = discard_download_body body in
                      match discarded with
                      | Error error -> return_error error
                      | Ok () -> return_ok response))))

    let delete conn ~bucket ~key =
      match validate_bucket_key bucket key with
      | Error error -> return_error error
      | Ok () -> (
          match object_request conn ~bucket ~key with
          | Error error -> return_error error
          | Ok request -> (
              let* result =
                call_empty conn ~method_:`DELETE ~request
                  ~query:[ ("tagging", []) ]
                  ~headers:[]
              in
              match result with
              | Error error -> return_error error
              | Ok (response, body) -> (
                  let* discarded = discard_download_body body in
                  match discarded with
                  | Error error -> return_error error
                  | Ok () -> return_ok response)))
  end
end
