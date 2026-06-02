open Core

module Make (C : Operation_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t

  module Bucket_xml = Bucket_config_xml

  let bucket_root_request conn bucket =
    bucket_request conn ~bucket ~suffix:"/" ~signing_suffix:"/"

  let content_md5_header body = ("content-md5", content_md5 body)

  let expected_owner_headers expected_bucket_owner =
    [] |> add_opt_header "x-amz-expected-bucket-owner" expected_bucket_owner

  let get_xml ?expected_bucket_owner conn ~bucket ~subresource ~max_size ~parse
      =
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`GET ~request
              ~query:[ (subresource, []) ]
              ~headers:(expected_owner_headers expected_bucket_owner)
              ~f:(fun response body ->
                let* body = read_response_body body ~max_size in
                match body with
                | Error error -> return_error error
                | Ok body -> return (parse body response)))

  let put_xml ?expected_bucket_owner conn ~bucket ~subresource ~body =
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        let upload = R.Request_body.of_string body in
        let headers =
          [ content_md5_header body; ("content-type", "application/xml") ]
          |> add_opt_header "x-amz-expected-bucket-owner" expected_bucket_owner
        in
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_response conn ~method_:`PUT ~request
              ~query:[ (subresource, []) ]
              ~headers
              ~payload_hash:(R.Request_body.descriptor upload).payload_hash
              upload
              ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return_ok response))

  let delete_subresource ?expected_bucket_owner conn ~bucket ~subresource =
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`DELETE ~request
              ~query:[ (subresource, []) ]
              ~headers:(expected_owner_headers expected_bucket_owner)
              ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return_ok response))

  let create conn ~bucket ?options () =
    let options = Option.value ~default:Create_bucket.default_options options in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        let region = Option.value ~default:(R.region conn) options.region in
        let body =
          if
            Awskit.Region.equal region (Awskit.Region.of_string_exn "us-east-1")
          then ""
          else Bucket_result_xml.create_config region
        in
        let upload = R.Request_body.of_string body in
        let headers =
          if body = "" then [] else [ ("content-type", "application/xml") ]
        in
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_response conn ~method_:`PUT ~request ~query:[] ~headers
              ~payload_hash:(R.Request_body.descriptor upload).payload_hash
              upload ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return_ok { Create_bucket.response }))

  let delete ?expected_bucket_owner conn ~bucket =
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`DELETE ~request ~query:[]
              ~headers:(expected_owner_headers expected_bucket_owner)
              ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () -> return_ok { Delete_bucket.response }))

  let head ?expected_bucket_owner conn ~bucket =
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`HEAD ~request ~query:[]
              ~headers:(expected_owner_headers expected_bucket_owner)
              ~f:(fun response body ->
                let* discarded = discard_response_body body in
                match discarded with
                | Error error -> return_error error
                | Ok () ->
                    let region =
                      Option.bind
                        (Awskit.Response.header response "x-amz-bucket-region")
                        (fun value ->
                          Result.to_option (Awskit.Region.of_string value))
                    in
                    return_ok { Head_bucket.name = bucket; region; response }))

  let exists ?expected_bucket_owner conn ~bucket =
    let* result = head ?expected_bucket_owner conn ~bucket in
    match result with
    | Ok _ -> return_ok true
    | Error error when Error.is_not_found error -> return_ok false
    | Error error -> return_error error

  let list conn =
    match root_request conn with
    | Error error -> return_error error
    | Ok request ->
        with_empty_response conn ~method_:`GET ~request ~query:[] ~headers:[]
          ~f:(fun _response body ->
            let* body = read_response_body body ~max_size:4_194_304L in
            match body with
            | Error error -> return_error error
            | Ok body -> return (Bucket_result_xml.parse_list body))

  let get_location ?expected_bucket_owner conn ~bucket =
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_empty_response conn ~method_:`GET ~request
              ~query:[ ("location", []) ]
              ~headers:(expected_owner_headers expected_bucket_owner)
              ~f:(fun _response body ->
                let* body = read_response_body body ~max_size:1_048_576L in
                match body with
                | Error error -> return_error error
                | Ok body -> return (Bucket_result_xml.parse_location body)))

  module Policy = struct
    let get ?expected_bucket_owner conn ~bucket =
      match validate_bucket bucket with
      | Error error -> return_error error
      | Ok () -> (
          match bucket_root_request conn bucket with
          | Error error -> return_error error
          | Ok request ->
              with_empty_response conn ~method_:`GET ~request
                ~query:[ ("policy", []) ]
                ~headers:(expected_owner_headers expected_bucket_owner)
                ~f:(fun _response body ->
                  let* body = read_response_body body ~max_size:1_048_576L in
                  match body with
                  | Error error -> return_error error
                  | Ok body -> return (Policy.of_json body)))

    let put conn ~bucket ?expected_bucket_owner policy =
      match validate_bucket bucket with
      | Error error -> return_error error
      | Ok () -> (
          let body = Policy.to_json policy in
          let upload = R.Request_body.of_string body in
          let headers =
            [ content_md5_header body; ("content-type", "application/json") ]
            |> add_opt_header "x-amz-expected-bucket-owner"
                 expected_bucket_owner
          in
          match bucket_root_request conn bucket with
          | Error error -> return_error error
          | Ok request ->
              with_response conn ~method_:`PUT ~request
                ~query:[ ("policy", []) ]
                ~headers
                ~payload_hash:(R.Request_body.descriptor upload).payload_hash
                upload
                ~f:(fun response body ->
                  let* discarded = discard_response_body body in
                  match discarded with
                  | Error error -> return_error error
                  | Ok () -> return_ok response))

    let delete ?expected_bucket_owner conn ~bucket =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~subresource:"policy"
  end

  module Versioning = struct
    let get ?expected_bucket_owner conn ~bucket =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"versioning"
        ~max_size:1_048_576L ~parse:Bucket_xml.Versioning.parse

    let put conn ~bucket ?expected_bucket_owner status =
      put_xml ?expected_bucket_owner conn ~bucket ~subresource:"versioning"
        ~body:(Bucket_xml.Versioning.xml (Some status))
  end

  module Tagging = struct
    let parse body response =
      Result.map
        (fun tags -> { Bucket.Tagging.tags; response })
        (parse_tags body)

    let get ?expected_bucket_owner conn ~bucket =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"tagging"
        ~max_size:1_048_576L ~parse

    let put conn ~bucket ?expected_bucket_owner tags =
      match validate_tags tags with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"tagging"
            ~body:(xml_tags tags)

    let delete ?expected_bucket_owner conn ~bucket =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~subresource:"tagging"
  end

  module Encryption = struct
    let get ?expected_bucket_owner conn ~bucket =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"encryption"
        ~max_size:1_048_576L ~parse:Bucket_xml.Encryption.parse

    let put conn ~bucket ?expected_bucket_owner config =
      match Bucket_xml.Encryption.validate_config config with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"encryption"
            ~body:(Bucket_xml.Encryption.xml config)

    let delete ?expected_bucket_owner conn ~bucket =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~subresource:"encryption"
  end

  module Cors = struct
    let get ?expected_bucket_owner conn ~bucket =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"cors"
        ~max_size:1_048_576L ~parse:Bucket_xml.Cors.parse

    let put conn ~bucket ?expected_bucket_owner config =
      match Bucket_xml.Cors.validate_config config with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"cors"
            ~body:(Bucket_xml.Cors.xml config)

    let delete ?expected_bucket_owner conn ~bucket =
      delete_subresource ?expected_bucket_owner conn ~bucket ~subresource:"cors"
  end

  module Public_access_block = struct
    let get ?expected_bucket_owner conn ~bucket =
      get_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"publicAccessBlock" ~max_size:1_048_576L
        ~parse:Bucket_xml.Public_access_block.parse

    let put conn ~bucket ?expected_bucket_owner config =
      put_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"publicAccessBlock"
        ~body:(Bucket_xml.Public_access_block.xml config)

    let delete ?expected_bucket_owner conn ~bucket =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~subresource:"publicAccessBlock"
  end

  module Ownership_controls = struct
    let get ?expected_bucket_owner conn ~bucket =
      get_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"ownershipControls" ~max_size:1_048_576L
        ~parse:Bucket_xml.Ownership_controls.parse

    let put conn ~bucket ?expected_bucket_owner config =
      put_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"ownershipControls"
        ~body:(Bucket_xml.Ownership_controls.xml config)

    let delete ?expected_bucket_owner conn ~bucket =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~subresource:"ownershipControls"
  end
end
