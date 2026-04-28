open Awskit_s3_core

module Make (C : Awskit_s3_operation_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t
  type nonrec upload_body = upload_body

  let create conn ~bucket ~key ?options () =
    let options =
      Option.value ~default:Public_multipart.Create.default_options options
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
                let headers =
                  Metadata_headers.to_headers options.metadata
                  @ checksum_request_headers options.checksum
                  @ encryption_request_headers options.server_side_encryption
                  |> add_opt_header "content-type" options.content_type
                  |> add_opt_header "x-amz-storage-class"
                       (Option.map Storage_class.to_string options.storage_class)
                  |> add_opt_header "x-amz-tagging" (tags_header options.tags)
                in
                match object_request conn ~bucket ~key with
                | Error error -> return_error error
                | Ok request -> (
                    let* result =
                      call_empty conn ~method_:`POST ~request
                        ~query:[ ("uploads", []) ]
                        ~headers
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
                          | Ok body -> (
                              match
                                Xml.decode_root body
                                  ~name:"InitiateMultipartUploadResult"
                              with
                              | Error error -> return_error error
                              | Ok nodes -> (
                                  match Xml.child_text "UploadId" nodes with
                                  | None ->
                                      return_error (decode "missing UploadId")
                                  | Some upload_id -> (
                                      match
                                        Public_multipart.Upload_id.of_string
                                          upload_id
                                      with
                                      | Error error -> return_error error
                                      | Ok upload_id ->
                                          return_ok
                                            {
                                              Public_multipart.Create.upload =
                                                { bucket; key; upload_id };
                                              request = response;
                                            }))))))))

  let upload_part conn ~bucket ~key ~upload_id ~part_number ~body ?options () =
    let options =
      Option.value ~default:Public_multipart.Upload_part.default_options options
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match Public_multipart.Part.create ~part_number ~etag:"unused" with
        | Error error -> return_error error
        | Ok _ -> (
            let descriptor = R.upload_descriptor body in
            match descriptor.content_length with
            | None ->
                return_error
                  (Awskit.Error.validation ~field:"content_length"
                     "S3 multipart uploads require a known content length")
            | Some content_length -> (
                let headers =
                  ("content-length", Int64.to_string content_length)
                  :: checksum_request_headers options.checksum
                in
                let query =
                  [
                    ("partNumber", [ string_of_int part_number ]);
                    ( "uploadId",
                      [ Public_multipart.Upload_id.to_string upload_id ] );
                  ]
                in
                match object_request conn ~bucket ~key with
                | Error error -> return_error error
                | Ok request -> (
                    let* result =
                      call conn ~method_:`PUT ~request ~query ~headers
                        ~payload_hash:descriptor.payload_hash body
                    in
                    match result with
                    | Error error -> return_error error
                    | Ok (response, body) -> (
                        let* discarded = discard_download_body body in
                        match discarded with
                        | Error error -> return_error error
                        | Ok () -> (
                            match response_etag response with
                            | Error error -> return_error error
                            | Ok None ->
                                return_error
                                  (decode "missing multipart part etag")
                            | Ok (Some etag) -> (
                                match
                                  Public_multipart.Part.create ~part_number
                                    ~etag
                                with
                                | Error error -> return_error error
                                | Ok part ->
                                    return_ok
                                      {
                                        Public_multipart.Upload_part.part;
                                        checksum = response_checksum response;
                                        request = response;
                                      })))))))

  let complete conn ~bucket ~key ~upload_id parts =
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        let rec validate previous = function
          | [] -> Ok ()
          | (part : Public_multipart.Part.t) :: rest ->
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
              (Awskit.Error.validation ~field:"parts"
                 "complete requires at least one part")
        | parts -> (
            match validate None parts with
            | Error error -> return_error error
            | Ok () -> (
                let body =
                  Xml.el "CompleteMultipartUpload"
                    (List.map
                       (fun (part : Public_multipart.Part.t) ->
                         Xml.el "Part"
                           [
                             Xml.text "PartNumber"
                               (string_of_int part.part_number);
                             Xml.text "ETag"
                               (Public_object.Etag.to_string part.etag);
                           ])
                       parts)
                  |> Xml.to_string
                in
                let upload = R.string_body body in
                match object_request conn ~bucket ~key with
                | Error error -> return_error error
                | Ok request -> (
                    let* result =
                      call conn ~method_:`POST ~request
                        ~query:
                          [
                            ( "uploadId",
                              [ Public_multipart.Upload_id.to_string upload_id ]
                            );
                          ]
                        ~headers:[ ("content-type", "application/xml") ]
                        ~payload_hash:(R.upload_descriptor upload).payload_hash
                        upload
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
                          | Ok body -> (
                              let etag =
                                match
                                  Xml.decode_root body
                                    ~name:"CompleteMultipartUploadResult"
                                with
                                | Ok nodes -> Xml.child_text "ETag" nodes
                                | Error _ -> None
                              in
                              let etag =
                                option_map_result Public_object.Etag.of_string
                                  etag
                              in
                              match etag with
                              | Error error -> return_error error
                              | Ok etag -> (
                                  let* version_id =
                                    return (response_version response)
                                  in
                                  match version_id with
                                  | Error error -> return_error error
                                  | Ok version_id ->
                                      return_ok
                                        {
                                          Public_multipart.Complete.etag;
                                          version_id;
                                          checksum = response_checksum response;
                                          request = response;
                                        })))))))

  let abort conn ~bucket ~key ~upload_id =
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request -> (
            let* result =
              call_empty conn ~method_:`DELETE ~request
                ~query:
                  [
                    ( "uploadId",
                      [ Public_multipart.Upload_id.to_string upload_id ] );
                  ]
                ~headers:[]
            in
            match result with
            | Error error -> return_error error
            | Ok (response, body) -> (
                let* discarded = discard_download_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return_ok response)))

  let list_parts conn ~bucket ~key ~upload_id =
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request -> (
            let* result =
              call_empty conn ~method_:`GET ~request
                ~query:
                  [
                    ( "uploadId",
                      [ Public_multipart.Upload_id.to_string upload_id ] );
                  ]
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
                  | Ok body -> (
                      match Xml.decode_root body ~name:"ListPartsResult" with
                      | Error error -> return_error error
                      | Ok nodes ->
                          let parts =
                            Xml.children "Part" nodes
                            |> List.filter_map (fun nodes ->
                                match
                                  Option.bind
                                    (Xml.child_text "PartNumber" nodes)
                                    int_of_string_opt
                                with
                                | None -> None
                                | Some part_number ->
                                    Some
                                      {
                                        Public_multipart.List_parts.part_number;
                                        etag =
                                          Option.bind
                                            (Xml.child_text "ETag" nodes)
                                            (fun v ->
                                              Result.to_option
                                                (Public_object.Etag.of_string v));
                                        size =
                                          Option.bind
                                            (Xml.child_text "Size" nodes)
                                            int64_of_string_opt;
                                        last_modified =
                                          Option.bind
                                            (Xml.child_text "LastModified" nodes)
                                            ptime_of_string;
                                        checksum = None;
                                      })
                          in
                          return_ok
                            {
                              Public_multipart.List_parts.parts;
                              is_truncated =
                                Option.value ~default:false
                                  (Option.bind
                                     (Xml.child_text "IsTruncated" nodes)
                                     parse_bool);
                              next_part_number_marker =
                                Option.bind
                                  (Xml.child_text "NextPartNumberMarker" nodes)
                                  int_of_string_opt;
                              request = response;
                            }))))
end
