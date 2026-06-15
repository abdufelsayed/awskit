open Common
open Headers
open Response
open Tagging_xml
module Create_bucket = Bucket.Create
module Delete_bucket = Bucket.Delete
module Head_bucket = Bucket.Head
module List_buckets = Bucket.List_buckets
module Get_bucket_location = Bucket.Get_location

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

  let content_md5_header body = ("content-md5", content_md5 body)

  let expected_owner_headers expected_bucket_owner =
    [] |> add_opt_header "x-amz-expected-bucket-owner" expected_bucket_owner

  let get_xml ?expected_bucket_owner conn ~bucket ~operation ~subresource
      ~max_size ~parse =
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
    let return_error = return_s3_error return_error ~operation ~bucket in
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
    let options = Option.value ~default:Create_bucket.default_options options in
    let return_error =
      return_s3_error return_error ~operation:"CreateBucket" ~bucket
    in
    match validate_bucket bucket with
    | Error error -> return_error error
    | Ok () -> (
        let region =
          match options.region with
          | None -> Ok (R.region conn)
          | Some region -> Awskit.Region.of_string region
        in
        match region with
        | Error error -> return_error error
        | Ok region -> (
            let body =
              if
                Awskit.Region.equal region
                  (Awskit.Region.of_string_exn "us-east-1")
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
                     ~payload_hash:
                       (R.Request_body.descriptor upload).payload_hash upload
                     ~f:(fun response body ->
                       let* discarded = discard_response_body body in
                       match discarded with
                       | Error error -> return_error error
                       | Ok () -> return_ok { Create_bucket.response }))))

  let delete conn ~bucket ?expected_bucket_owner () =
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

  let head conn ~bucket ?expected_bucket_owner () =
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
                         Option.bind
                           (Awskit.Response.header response
                              "x-amz-bucket-region") (fun value ->
                             Result.to_option (Awskit.Region.of_string value))
                       in
                       return_ok { Head_bucket.name = bucket; region; response }))
        )

  let exists conn ~bucket ?expected_bucket_owner () =
    let* result = head conn ~bucket ?expected_bucket_owner () in
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

  let get_location conn ~bucket ?expected_bucket_owner () =
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
    let get conn ~bucket ?expected_bucket_owner () =
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

    let put conn ~bucket ?expected_bucket_owner policy =
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
            |> add_opt_header "x-amz-expected-bucket-owner"
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

    let delete conn ~bucket ?expected_bucket_owner () =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketPolicy" ~subresource:"policy"
  end

  module Versioning = struct
    let get conn ~bucket ?expected_bucket_owner () =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"versioning"
        ~operation:"GetBucketVersioning" ~max_size:1_048_576L
        ~parse:Bucket_versioning_xml.parse

    let put conn ~bucket ?expected_bucket_owner status =
      put_xml ?expected_bucket_owner conn ~bucket ~subresource:"versioning"
        ~operation:"PutBucketVersioning"
        ~body:(Bucket_versioning_xml.xml (Some status))
  end

  module Tagging = struct
    let parse body response =
      Result.map
        (fun tags -> { Bucket.Tagging.tags; response })
        (parse_tags body)

    let get conn ~bucket ?expected_bucket_owner () =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"tagging"
        ~operation:"GetBucketTagging" ~max_size:1_048_576L ~parse

    let put conn ~bucket ?expected_bucket_owner tags =
      let return_error =
        return_s3_error return_error ~operation:"PutBucketTagging" ~bucket
      in
      match validate_tags tags with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"tagging"
            ~operation:"PutBucketTagging" ~body:(xml_tags tags)

    let delete conn ~bucket ?expected_bucket_owner () =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketTagging" ~subresource:"tagging"
  end

  module Encryption = struct
    let get conn ~bucket ?expected_bucket_owner () =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"encryption"
        ~operation:"GetBucketEncryption" ~max_size:1_048_576L
        ~parse:Bucket_encryption_xml.parse

    let put conn ~bucket ?expected_bucket_owner config =
      let return_error =
        return_s3_error return_error ~operation:"PutBucketEncryption" ~bucket
      in
      match Bucket_encryption_xml.validate_config config with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"encryption"
            ~operation:"PutBucketEncryption"
            ~body:(Bucket_encryption_xml.xml config)

    let delete conn ~bucket ?expected_bucket_owner () =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketEncryption" ~subresource:"encryption"
  end

  module Cors = struct
    let get conn ~bucket ?expected_bucket_owner () =
      get_xml ?expected_bucket_owner conn ~bucket ~subresource:"cors"
        ~operation:"GetBucketCors" ~max_size:1_048_576L
        ~parse:Bucket_cors_xml.parse

    let put conn ~bucket ?expected_bucket_owner config =
      let return_error =
        return_s3_error return_error ~operation:"PutBucketCors" ~bucket
      in
      match Bucket_cors_xml.validate_config config with
      | Error error -> return_error error
      | Ok () ->
          put_xml ?expected_bucket_owner conn ~bucket ~subresource:"cors"
            ~operation:"PutBucketCors"
            ~body:(Bucket_cors_xml.xml config)

    let delete conn ~bucket ?expected_bucket_owner () =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketCors" ~subresource:"cors"
  end

  module Public_access_block = struct
    let get conn ~bucket ?expected_bucket_owner () =
      get_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"publicAccessBlock" ~max_size:1_048_576L
        ~operation:"GetPublicAccessBlock"
        ~parse:Bucket_access_xml.Public_access_block.parse

    let put conn ~bucket ?expected_bucket_owner config =
      put_xml ?expected_bucket_owner conn ~bucket
        ~operation:"PutPublicAccessBlock" ~subresource:"publicAccessBlock"
        ~body:(Bucket_access_xml.Public_access_block.xml config)

    let delete conn ~bucket ?expected_bucket_owner () =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeletePublicAccessBlock" ~subresource:"publicAccessBlock"
  end

  module Ownership_controls = struct
    let get conn ~bucket ?expected_bucket_owner () =
      get_xml ?expected_bucket_owner conn ~bucket
        ~subresource:"ownershipControls" ~max_size:1_048_576L
        ~operation:"GetBucketOwnershipControls"
        ~parse:Bucket_access_xml.Ownership_controls.parse

    let put conn ~bucket ?expected_bucket_owner config =
      put_xml ?expected_bucket_owner conn ~bucket
        ~operation:"PutBucketOwnershipControls" ~subresource:"ownershipControls"
        ~body:(Bucket_access_xml.Ownership_controls.xml config)

    let delete conn ~bucket ?expected_bucket_owner () =
      delete_subresource ?expected_bucket_owner conn ~bucket
        ~operation:"DeleteBucketOwnershipControls"
        ~subresource:"ownershipControls"
  end
end
