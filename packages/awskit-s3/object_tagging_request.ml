open Headers
open Tagging_xml

module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind

  let return_result return_error return_ok = function
    | Ok value -> return_ok value
    | Error error -> return_error error

  let with_operation_result return_error return_ok response =
    let* result = response in
    return_result return_error return_ok result

  let owner_headers options =
    []
    |> add_opt_account_id_header "x-amz-expected-bucket-owner"
         options.Object.Tagging.expected_bucket_owner

  let get conn ~bucket ~key ?options () =
    let bucket = Bucket_name.to_string bucket in
    let key = Object_key.to_string key in
    let options =
      Option.value ~default:Object.Tagging.default_options options
    in
    let return_error =
      S3_error_context.return_s3_error return_error
        ~operation:"GetObjectTagging" ~bucket ~key
    in
    match S3_validation.validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`GET ~request
                 ~query:[ ("tagging", []) ]
                 ~headers:(owner_headers options)
                 ~f:(fun response body ->
                   let* body = read_response_body body ~max_size:1_048_576L in
                   match body with
                   | Error error -> return_error error
                   | Ok body ->
                       return_result return_error return_ok
                         (Result.map
                            (fun tags -> { Object.Tagging.tags; response })
                            (parse_tags body)))))

  let put conn ~bucket ~key ?options ~tags () =
    let bucket = Bucket_name.to_string bucket in
    let key = Object_key.to_string key in
    let options =
      Option.value ~default:Object.Tagging.default_options options
    in
    let return_error =
      S3_error_context.return_s3_error return_error
        ~operation:"PutObjectTagging" ~bucket ~key
    in
    match S3_validation.validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match S3_validation.validate_tags tags with
        | Error error -> return_error error
        | Ok () -> (
            let body = xml_tags tags in
            let upload = R.Request_body.of_string body in
            let headers =
              [
                ("content-md5", content_md5 body);
                ("content-type", "application/xml");
              ]
              @ owner_headers options
            in
            match object_request conn ~bucket ~key with
            | Error error -> return_error error
            | Ok request ->
                with_operation_result return_error return_ok
                  (with_response conn ~method_:`PUT ~request
                     ~query:[ ("tagging", []) ]
                     ~headers
                     ~payload_hash:
                       (R.Request_body.descriptor upload).payload_hash upload
                     ~f:(fun response body ->
                       let* discarded = discard_response_body body in
                       match discarded with
                       | Error error -> return_error error
                       | Ok () -> return_ok response))))

  let delete conn ~bucket ~key ?options () =
    let bucket = Bucket_name.to_string bucket in
    let key = Object_key.to_string key in
    let options =
      Option.value ~default:Object.Tagging.default_options options
    in
    let return_error =
      S3_error_context.return_s3_error return_error
        ~operation:"DeleteObjectTagging" ~bucket ~key
    in
    match S3_validation.validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`DELETE ~request
                 ~query:[ ("tagging", []) ]
                 ~headers:(owner_headers options)
                 ~f:(fun response body ->
                   let* discarded = discard_response_body body in
                   match discarded with
                   | Error error -> return_error error
                   | Ok () -> return_ok response)))
end
