open Common
open Headers
open Tagging_xml

module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind

  let owner_headers options =
    []
    |> add_opt_header "x-amz-expected-bucket-owner"
         options.Object.Tagging.expected_bucket_owner

  let get conn ~bucket ~key ?options () =
    let options =
      Option.value ~default:Object.Tagging.default_options options
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`GET ~request
              ~query:[ ("tagging", []) ]
              ~headers:(owner_headers options)
              ~f:(fun response body ->
                let* body = read_response_body body ~max_size:1_048_576L in
                match body with
                | Error error -> return_error error
                | Ok body ->
                    return
                      (Result.map
                         (fun tags -> { Object.Tagging.tags; response })
                         (parse_tags body))))

  let put conn ~bucket ~key ?options tags =
    let options =
      Option.value ~default:Object.Tagging.default_options options
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match validate_tags tags with
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
                with_response conn ~method_:`PUT ~request
                  ~query:[ ("tagging", []) ]
                  ~headers
                  ~payload_hash:(R.Request_body.descriptor upload).payload_hash
                  upload
                  ~f:(fun response body ->
                    let* discarded = discard_response_body body in
                    match discarded with
                    | Error error -> return_error error
                    | Ok () -> return_ok response)))

  let delete conn ~bucket ~key ?options () =
    let options =
      Option.value ~default:Object.Tagging.default_options options
    in
    match validate_bucket_key bucket key with
    | Error error -> return_error error
    | Ok () -> (
        match object_request conn ~bucket ~key with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`DELETE ~request
              ~query:[ ("tagging", []) ]
              ~headers:(owner_headers options)
              ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return_ok response))
end
