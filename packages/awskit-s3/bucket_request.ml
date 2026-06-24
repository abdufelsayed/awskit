open Headers
open Response
open Tagging_xml
module Error = S3_error

let invalid = S3_error_context.invalid
let decode_with_context = S3_error_context.decode_with_context
let return_s3_error = S3_error_context.return_s3_error
let validate_bucket = S3_validation.validate_bucket
let validate_tags = S3_validation.validate_tags

module Create_bucket = Bucket.Create
module Delete_bucket = Bucket.Delete
module Head_bucket = Bucket.Head
module List_buckets = Bucket.List_buckets
module Get_bucket_location = Bucket.Get_location
module Bucket_policy = Bucket.Policy
module Bucket_versioning = Bucket.Versioning
module Bucket_tagging = Bucket.Tagging
module Bucket_encryption = Bucket.Encryption
module Bucket_cors = Bucket.Cors
module Bucket_public_access_block = Bucket.Public_access_block
module Bucket_ownership_controls = Bucket.Ownership_controls

module Make (C : Request_context.S) = struct
  open C

  let ( let* ) = bind

  type nonrec connection = connection
  type 'a io = 'a R.t

  let return_result return_error return_ok = function
    | Ok value -> return_ok value
    | Error error -> return_error error

  let with_operation_result return_error return_ok response =
    let* result = response in
    return_result return_error return_ok result

  let bucket_root_request conn bucket =
    bucket_request conn ~bucket ~suffix:"/" ~signing_suffix:"/"

  let bucket_string = Bucket_name.to_string
  let content_md5_header body = ("content-md5", content_md5 body)

  let expected_owner_headers expected_bucket_owner =
    []
    |> add_opt_account_id_header "x-amz-expected-bucket-owner"
         expected_bucket_owner

  let owner_from_options default field options =
    let options = Option.value ~default options in
    field options

  let get_xml ?expected_bucket_owner conn ~bucket ~operation ~subresource
      ~max_size ~parse =
    let bucket = bucket_string bucket in
    let return_error = return_s3_error return_error ~operation ~bucket in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`GET ~request
                 ~query:[ (subresource, []) ]
                 ~headers:(expected_owner_headers expected_bucket_owner)
                 ~f:(fun response body ->
                   let* body = read_response_body body ~max_size in
                   match body with
                   | Error error -> return_error error
                   | Ok body ->
                       return_result return_error return_ok
                         (parse body response))))

  let put_xml ?expected_bucket_owner conn ~bucket ~operation ~subresource ~body
      =
    let bucket = bucket_string bucket in
    let return_error = return_s3_error return_error ~operation ~bucket in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        let upload = R.Request_body.of_string body in
        let headers =
          [ content_md5_header body; ("content-type", "application/xml") ]
          |> add_opt_account_id_header "x-amz-expected-bucket-owner"
               expected_bucket_owner
        in
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_response conn ~method_:`PUT ~request
                 ~query:[ (subresource, []) ]
                 ~headers
                 ~payload_hash:(R.Request_body.descriptor upload).payload_hash
                 upload
                 ~f:(fun response body ->
                   let* discarded = discard_response_body body in
                   match discarded with
                   | Error error -> return_error error
                   | Ok () -> return_ok response)))

  let delete_subresource ?expected_bucket_owner conn ~bucket ~operation
      ~subresource =
    let bucket = bucket_string bucket in
    let return_error = return_s3_error return_error ~operation ~bucket in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`DELETE ~request
                 ~query:[ (subresource, []) ]
                 ~headers:(expected_owner_headers expected_bucket_owner)
                 ~f:(fun response body ->
                   let* discarded = discard_response_body body in
                   match discarded with
                   | Error error -> return_error error
                   | Ok () -> return_ok response)))

  let create conn ~bucket ?options () =
    let bucket = bucket_string bucket in
    let options = Option.value ~default:Create_bucket.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"CreateBucket" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        let region = Option.value ~default:(region conn) options.region in
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
            with_operation_result return_error return_ok
              (with_response conn ~method_:`PUT ~request ~query:[] ~headers
                 ~payload_hash:(R.Request_body.descriptor upload).payload_hash
                 upload ~f:(fun response body ->
                   let* discarded = discard_response_body body in
                   match discarded with
                   | Error error -> return_error error
                   | Ok () -> return_ok { Create_bucket.response })))

  let delete conn ~bucket ?options () =
    let expected_bucket_owner =
      owner_from_options Delete_bucket.default_options
        (fun (options : Delete_bucket.options) -> options.expected_bucket_owner)
        options
    in
    let bucket = bucket_string bucket in
    let return_error =
      return_s3_error return_error ~operation:"DeleteBucket" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`DELETE ~request ~query:[]
                 ~headers:(expected_owner_headers expected_bucket_owner)
                 ~f:(fun response body ->
                   let* discarded = discard_response_body body in
                   match discarded with
                   | Error error -> return_error error
                   | Ok () -> return_ok { Delete_bucket.response })))

  let head conn ~bucket ?options () =
    let expected_bucket_owner =
      owner_from_options Head_bucket.default_options
        (fun (options : Head_bucket.options) -> options.expected_bucket_owner)
        options
    in
    let bucket_name = bucket in
    let bucket = bucket_string bucket in
    let return_error =
      return_s3_error return_error ~operation:"HeadBucket" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`HEAD ~request ~query:[]
                 ~headers:(expected_owner_headers expected_bucket_owner)
                 ~f:(fun response body ->
                   let* discarded = discard_response_body body in
                   match discarded with
                   | Error error -> return_error error
                   | Ok () ->
                       let region =
                         match
                           Awskit.Response.header response "x-amz-bucket-region"
                         with
                         | None -> Ok None
                         | Some value -> (
                             match Awskit.Region.of_string value with
                             | Ok region -> Ok (Some region)
                             | Error error ->
                                 Error
                                   (decode_with_context
                                      ~what:
                                        "x-amz-bucket-region response header"
                                      (Awskit.Error.to_string_hum error)))
                       in
                       return_result return_error return_ok
                         (Result.map
                            (fun region ->
                              {
                                Head_bucket.name = bucket_name;
                                region;
                                response;
                              })
                            region))))

  let exists conn ~bucket ?options () =
    let* result = head conn ~bucket ?options () in
    match result with
    | Ok _ -> return_ok true
    | Error error when Error.is_not_found error -> return_ok false
    | Error error -> return_error error

  let list conn =
    let return_error = return_s3_error return_error ~operation:"ListBuckets" in
    match root_request conn with
    | Error error -> return_error error
    | Ok request ->
        with_operation_result return_error return_ok
          (with_empty_response conn ~method_:`GET ~request ~query:[] ~headers:[]
             ~f:(fun response body ->
               let* body = read_response_body body ~max_size:4_194_304L in
               match body with
               | Error error -> return_error error
               | Ok body ->
                   return_result return_error return_ok
                     (Result.map
                        (fun buckets -> { List_buckets.buckets; response })
                        (Bucket_result_xml.parse_list body))))

  let get_location conn ~bucket ?options () =
    let expected_bucket_owner =
      owner_from_options Get_bucket_location.default_options
        (fun (options : Get_bucket_location.options) ->
          options.expected_bucket_owner)
        options
    in
    let bucket = bucket_string bucket in
    let return_error =
      return_s3_error return_error ~operation:"GetBucketLocation" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        match bucket_root_request conn bucket with
        | Error error -> return_error error
        | Ok request ->
            with_operation_result return_error return_ok
              (with_empty_response conn ~method_:`GET ~request
                 ~query:[ ("location", []) ]
                 ~headers:(expected_owner_headers expected_bucket_owner)
                 ~f:(fun response body ->
                   let* body = read_response_body body ~max_size:1_048_576L in
                   match body with
                   | Error error -> return_error error
                   | Ok body ->
                       return_result return_error return_ok
                         (Result.map
                            (fun region ->
                              { Get_bucket_location.region; response })
                            (Bucket_result_xml.parse_location body)))))

  module Policy = struct
    let get conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_policy.default_options
          (fun (options : Bucket_policy.options) ->
            options.expected_bucket_owner)
          options
      in
      let bucket = bucket_string bucket in
      let return_error =
        return_s3_error return_error ~operation:"GetBucketPolicy" ~bucket
      in
      match validate_bucket bucket with
      | Error error -> return_error error
      | Ok () -> (
          match bucket_root_request conn bucket with
          | Error error -> return_error error
          | Ok request ->
              with_operation_result return_error return_ok
                (with_empty_response conn ~method_:`GET ~request
                   ~query:[ ("policy", []) ]
                   ~headers:(expected_owner_headers expected_bucket_owner)
                   ~f:(fun _response body ->
                     let* body = read_response_body body ~max_size:1_048_576L in
                     match body with
                     | Error error -> return_error error
                     | Ok body ->
                         return_result return_error return_ok
                           (Policy.of_json body))))

    let put conn ~bucket ?options ~policy () =
      let expected_bucket_owner =
        owner_from_options Bucket_policy.default_options
          (fun (options : Bucket_policy.options) ->
            options.expected_bucket_owner)
          options
      in
      let bucket = bucket_string bucket in
      let return_error =
        return_s3_error return_error ~operation:"PutBucketPolicy" ~bucket
      in
      match validate_bucket bucket with
      | Error error -> return_error error
      | Ok () -> (
          let body = Policy.to_json policy in
          let upload = R.Request_body.of_string body in
          let headers =
            [ content_md5_header body; ("content-type", "application/json") ]
            |> add_opt_account_id_header "x-amz-expected-bucket-owner"
                 expected_bucket_owner
          in
          match bucket_root_request conn bucket with
          | Error error -> return_error error
          | Ok request ->
              with_operation_result return_error return_ok
                (with_response conn ~method_:`PUT ~request
                   ~query:[ ("policy", []) ]
                   ~headers
                   ~payload_hash:(R.Request_body.descriptor upload).payload_hash
                   upload
                   ~f:(fun response body ->
                     let* discarded = discard_response_body body in
                     match discarded with
                     | Error error -> return_error error
                     | Ok () -> return_ok response)))

    let delete conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_policy.default_options
          (fun (options : Bucket_policy.options) ->
            options.expected_bucket_owner)
          options
      in
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketPolicy" ~subresource:"policy"
  end

  module Versioning = struct
    let validate_status = function
      | Bucket.Versioning.Status.Enabled | Suspended -> Ok ()
      | Unknown value ->
          invalid ~field:"status"
            "unknown bucket versioning status %S cannot be sent" value

    let get conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_versioning.default_options
          (fun (options : Bucket_versioning.options) ->
            options.expected_bucket_owner)
          options
      in
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"versioning"
        ~operation:"GetBucketVersioning" ~max_size:1_048_576L
        ~parse:Bucket_versioning_xml.parse

    let put conn ~bucket ?options ~status () =
      let expected_bucket_owner =
        owner_from_options Bucket_versioning.default_options
          (fun (options : Bucket_versioning.options) ->
            options.expected_bucket_owner)
          options
      in
      let bucket_context = bucket_string bucket in
      let return_error =
        return_s3_error return_error ~operation:"PutBucketVersioning"
          ~bucket:bucket_context
      in
      match validate_status status with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"versioning"
            ~operation:"PutBucketVersioning"
            ~body:(Bucket_versioning_xml.xml (Some status))
  end

  module Tagging = struct
    let parse body response =
      Result.map
        (fun tags -> { Bucket.Tagging.tags; response })
        (parse_tags body)

    let get conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_tagging.default_options
          (fun (options : Bucket_tagging.options) ->
            options.expected_bucket_owner)
          options
      in
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"tagging"
        ~operation:"GetBucketTagging" ~max_size:1_048_576L ~parse

    let put conn ~bucket ?options ~tags () =
      let expected_bucket_owner =
        owner_from_options Bucket_tagging.default_options
          (fun (options : Bucket_tagging.options) ->
            options.expected_bucket_owner)
          options
      in
      let bucket_context = bucket_string bucket in
      let return_error =
        return_s3_error return_error ~operation:"PutBucketTagging"
          ~bucket:bucket_context
      in
      match validate_tags tags with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"tagging"
            ~operation:"PutBucketTagging" ~body:(xml_tags tags)

    let delete conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_tagging.default_options
          (fun (options : Bucket_tagging.options) ->
            options.expected_bucket_owner)
          options
      in
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketTagging" ~subresource:"tagging"
  end

  module Encryption = struct
    let get conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_encryption.default_options
          (fun (options : Bucket_encryption.options) ->
            options.expected_bucket_owner)
          options
      in
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"encryption"
        ~operation:"GetBucketEncryption" ~max_size:1_048_576L
        ~parse:Bucket_encryption_xml.parse

    let put conn ~bucket ?options ~config () =
      let expected_bucket_owner =
        owner_from_options Bucket_encryption.default_options
          (fun (options : Bucket_encryption.options) ->
            options.expected_bucket_owner)
          options
      in
      let bucket_context = bucket_string bucket in
      let return_error =
        return_s3_error return_error ~operation:"PutBucketEncryption"
          ~bucket:bucket_context
      in
      match Bucket_encryption_xml.validate_config config with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"encryption"
            ~operation:"PutBucketEncryption"
            ~body:(Bucket_encryption_xml.xml config)

    let delete conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_encryption.default_options
          (fun (options : Bucket_encryption.options) ->
            options.expected_bucket_owner)
          options
      in
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketEncryption" ~subresource:"encryption"
  end

  module Cors = struct
    let get conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_cors.default_options
          (fun (options : Bucket_cors.options) -> options.expected_bucket_owner)
          options
      in
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"cors"
        ~operation:"GetBucketCors" ~max_size:1_048_576L
        ~parse:Bucket_cors_xml.parse

    let put conn ~bucket ?options ~config () =
      let expected_bucket_owner =
        owner_from_options Bucket_cors.default_options
          (fun (options : Bucket_cors.options) -> options.expected_bucket_owner)
          options
      in
      let bucket_context = bucket_string bucket in
      let return_error =
        return_s3_error return_error ~operation:"PutBucketCors"
          ~bucket:bucket_context
      in
      match Bucket_cors_xml.validate_config config with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"cors"
            ~operation:"PutBucketCors"
            ~body:(Bucket_cors_xml.xml config)

    let delete conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_cors.default_options
          (fun (options : Bucket_cors.options) -> options.expected_bucket_owner)
          options
      in
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketCors" ~subresource:"cors"
  end

  module Public_access_block = struct
    let get conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_public_access_block.default_options
          (fun (options : Bucket_public_access_block.options) ->
            options.expected_bucket_owner)
          options
      in
      get_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"publicAccessBlock" ~max_size:1_048_576L
        ~operation:"GetPublicAccessBlock"
        ~parse:Bucket_access_xml.Public_access_block.parse

    let put conn ~bucket ?options ~config () =
      let expected_bucket_owner =
        owner_from_options Bucket_public_access_block.default_options
          (fun (options : Bucket_public_access_block.options) ->
            options.expected_bucket_owner)
          options
      in
      put_xml ?expected_bucket_owner conn ~bucket
        ~operation:"PutPublicAccessBlock" ~subresource:"publicAccessBlock"
        ~body:(Bucket_access_xml.Public_access_block.xml config)

    let delete conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_public_access_block.default_options
          (fun (options : Bucket_public_access_block.options) ->
            options.expected_bucket_owner)
          options
      in
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeletePublicAccessBlock" ~subresource:"publicAccessBlock"
  end

  module Ownership_controls = struct
    let validate_config (config : Bucket.Ownership_controls.config) =
      match config.object_ownership with
      | Bucket_owner_enforced | Bucket_owner_preferred | Object_writer -> Ok ()
      | Unknown value ->
          invalid ~field:"object_ownership"
            "unknown object ownership %S cannot be sent" value

    let get conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_ownership_controls.default_options
          (fun (options : Bucket_ownership_controls.options) ->
            options.expected_bucket_owner)
          options
      in
      get_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"ownershipControls" ~max_size:1_048_576L
        ~operation:"GetBucketOwnershipControls"
        ~parse:Bucket_access_xml.Ownership_controls.parse

    let put conn ~bucket ?options ~config () =
      let expected_bucket_owner =
        owner_from_options Bucket_ownership_controls.default_options
          (fun (options : Bucket_ownership_controls.options) ->
            options.expected_bucket_owner)
          options
      in
      let bucket_context = bucket_string bucket in
      let return_error =
        return_s3_error return_error ~operation:"PutBucketOwnershipControls"
          ~bucket:bucket_context
      in
      match validate_config config with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket
            ~operation:"PutBucketOwnershipControls"
            ~subresource:"ownershipControls"
            ~body:(Bucket_access_xml.Ownership_controls.xml config)

    let delete conn ~bucket ?options () =
      let expected_bucket_owner =
        owner_from_options Bucket_ownership_controls.default_options
          (fun (options : Bucket_ownership_controls.options) ->
            options.expected_bucket_owner)
          options
      in
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketOwnershipControls"
        ~subresource:"ownershipControls"
  end
end
