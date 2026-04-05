module S3_error = Error
module S3_policy = Policy
open Base

module Make (R : Awskit.Runtime.S) = struct
  let ( let* ) = R.bind

  (* ── helpers ──────────────────────────────────────────────────── *)

  let host conn =
    match R.endpoint_host conn with
    | Some h -> h
    | None -> Fmt.str "s3.%s.amazonaws.com" (R.region conn)

  let sign conn ~meth ~path ~query ~payload ~extra_headers =
    let request_host =
      let h = host conn in
      match R.endpoint_port conn with
      | Some p -> Fmt.str "%s:%d" h p
      | None -> h
    in
    let headers = ("host", request_host) :: extra_headers in
    Awskit.Signing.sign_request ~credentials:(R.credentials conn)
      ~region:(R.region conn) ~service:"s3" ~meth ~path ~query ~headers ~payload
      ~now:(R.clock conn)

  let call_s3 conn ~(meth : Awskit.Request.meth) ~bucket ~path ~query ~payload
      ~extra_headers =
    let s3_path = Fmt.str "/%s%s" bucket path in
    let signed =
      sign conn
        ~meth:(Awskit.Request.meth_to_string meth)
        ~path:s3_path ~query ~payload ~extra_headers
    in
    R.call conn
      {
        Awskit.Request.meth;
        host = host conn;
        port = R.endpoint_port conn;
        path = Internal.resource_path bucket path query;
        headers = signed.headers;
        body = payload;
      }

  let call_s3_root conn ~(meth : Awskit.Request.meth) ~path ~query ~payload
      ~extra_headers =
    let signed =
      sign conn
        ~meth:(Awskit.Request.meth_to_string meth)
        ~path ~query ~payload ~extra_headers
    in
    R.call conn
      {
        Awskit.Request.meth;
        host = host conn;
        port = R.endpoint_port conn;
        path = (if String.is_empty query then path else path ^ "?" ^ query);
        headers = signed.headers;
        body = payload;
      }

  let ok_or_err status body =
    if status >= 200 && status < 300 then Ok ()
    else Error (S3_error.of_status status body)

  (* ── Object operations ────────────────────────────────────────── *)

  module Object = struct
    include Object_

    module Tagging = struct
      include Object_.Tagging

      let get conn ~bucket ~key =
        Internal.Log.debug (fun m -> m "GET-OBJECT-TAGGING %s/%s" bucket key);
        let path = "/" ^ key in
        let query = "tagging=" in
        let* result =
          call_s3 conn ~meth:GET ~bucket ~path ~query ~payload:""
            ~extra_headers:[]
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp ->
              if Awskit.Response.is_success resp then
                match
                  Internal.Xml.decode resp.body ~name:"Tagging"
                    Tagging_xml.tagging_of_xmlm_exn
                with
                | Error _ as error -> error
                | Ok result ->
                    Ok
                      (List.map result.tag_set.tags
                         ~f:(fun (tag : Tagging_xml.tag) ->
                           { Tag.key = tag.key; value = tag.value }))
              else Error (S3_error.of_status resp.status resp.body))

      let put conn ~bucket ~key tags =
        Internal.Log.debug (fun m -> m "PUT-OBJECT-TAGGING %s/%s" bucket key);
        let xml_tags =
          List.map tags ~f:(fun (tag : Tag.t) ->
              { Tagging_xml.key = tag.key; value = tag.value })
        in
        let xml_body =
          Tagging_xml.tagging_to_xmlm { tag_set = { tags = xml_tags } }
          |> Internal.set_xml_name "Tagging"
          |> fun xml -> Ezxmlm.to_string [ xml ]
        in
        let content_md5 =
          Digestif.MD5.(digest_string xml_body |> to_raw_string)
          |> Base64.encode_exn
        in
        let extra_headers =
          [ ("content-md5", content_md5); ("content-type", "application/xml") ]
        in
        let path = "/" ^ key in
        let query = "tagging=" in
        let* result =
          call_s3 conn ~meth:PUT ~bucket ~path ~query ~payload:xml_body
            ~extra_headers
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp ->
              if Awskit.Response.is_success resp then Ok ()
              else Error (S3_error.of_status resp.status resp.body))

      let delete conn ~bucket ~key =
        Internal.Log.debug (fun m -> m "DELETE-OBJECT-TAGGING %s/%s" bucket key);
        let path = "/" ^ key in
        let query = "tagging=" in
        let* result =
          call_s3 conn ~meth:DELETE ~bucket ~path ~query ~payload:""
            ~extra_headers:[]
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp ->
              if Awskit.Response.is_success resp then Ok ()
              else Error (S3_error.of_status resp.status resp.body))
    end

    let put conn ~bucket ~key ?(if_none_match = false) ?if_match ?content_type
        ?(metadata = []) ?storage_class ?tags ?cache_control ?content_encoding
        ?content_disposition body =
      Internal.Log.debug (fun m ->
          m "PUT %s/%s (%d bytes)" bucket key (String.length body));
      let path = "/" ^ key in
      let extra_headers =
        ("content-length", Int.to_string (String.length body))
        :: Internal.Metadata_headers.to_headers metadata
      in
      let extra_headers =
        if if_none_match then ("if-none-match", "*") :: extra_headers
        else extra_headers
      in
      let extra_headers =
        extra_headers
        |> Internal.opt_header "if-match" if_match
        |> Internal.opt_header "content-type" content_type
        |> Internal.opt_header "cache-control" cache_control
        |> Internal.opt_header "content-encoding" content_encoding
        |> Internal.opt_header "content-disposition" content_disposition
      in
      let extra_headers =
        match storage_class with
        | Some sc ->
            ("x-amz-storage-class", Storage_class.to_string sc) :: extra_headers
        | None -> extra_headers
      in
      let extra_headers =
        match tags with
        | Some tags when not (List.is_empty tags) ->
            let tag_str =
              List.map tags ~f:(fun (tag : Tag.t) ->
                  Uri.pct_encode tag.key ^ "=" ^ Uri.pct_encode tag.value)
              |> String.concat ~sep:"&"
            in
            ("x-amz-tagging", tag_str) :: extra_headers
        | _ -> extra_headers
      in
      let* result =
        call_s3 conn ~meth:PUT ~bucket ~path ~query:"" ~payload:body
          ~extra_headers
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp ->
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              Result.map (Awskit.Response.header_exn resp "etag")
                ~f:(fun etag -> { Put_result.etag }))

    let get conn ~bucket ~key ?range ?if_match ?if_none_match ?if_modified_since
        ?if_unmodified_since () =
      Internal.Log.debug (fun m -> m "GET %s/%s" bucket key);
      let path = "/" ^ key in
      let extra_headers =
        []
        |> Internal.opt_header "if-match" if_match
        |> Internal.opt_header "if-none-match" if_none_match
        |> Internal.opt_header "if-modified-since" if_modified_since
        |> Internal.opt_header "if-unmodified-since" if_unmodified_since
      in
      let range_header =
        match range with
        | None -> Ok None
        | Some range -> Result.map (Range.to_header range) ~f:Option.some
      in
      match
        Result.map range_header ~f:(function
          | Some rh -> ("range", rh) :: extra_headers
          | None -> extra_headers)
      with
      | Error _ as error -> R.return error
      | Ok extra_headers ->
          let* result =
            call_s3 conn ~meth:GET ~bucket ~path ~query:"" ~payload:""
              ~extra_headers
          in
          R.return
            (match result with
            | Error e -> Error (S3_error.of_aws_error e)
            | Ok resp ->
                if not (Awskit.Response.is_success resp) then
                  Error (S3_error.of_status resp.status resp.body)
                else get_result_of_response resp)

    let head conn ~bucket ~key ?if_match ?if_none_match ?if_modified_since
        ?if_unmodified_since () =
      Internal.Log.debug (fun m -> m "HEAD %s/%s" bucket key);
      let path = "/" ^ key in
      let extra_headers =
        []
        |> Internal.opt_header "if-match" if_match
        |> Internal.opt_header "if-none-match" if_none_match
        |> Internal.opt_header "if-modified-since" if_modified_since
        |> Internal.opt_header "if-unmodified-since" if_unmodified_since
      in
      let* result =
        call_s3 conn ~meth:HEAD ~bucket ~path ~query:"" ~payload:""
          ~extra_headers
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp ->
            if Awskit.Response.is_success resp then
              Result.map (info_of_response resp) ~f:Option.some
            else if resp.status = 404 then Ok None
            else Error (S3_error.of_status resp.status resp.body))

    let delete conn ~bucket ~key =
      Internal.Log.debug (fun m -> m "DELETE %s/%s" bucket key);
      let path = "/" ^ key in
      let* result =
        call_s3 conn ~meth:DELETE ~bucket ~path ~query:"" ~payload:""
          ~extra_headers:[]
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp ->
            if Awskit.Response.is_success resp then Ok ()
            else Error (S3_error.of_status resp.status resp.body))

    let delete_batch conn ~bucket ~keys =
      Internal.Log.debug (fun m ->
          m "DELETE-BATCH %s (%d keys)" bucket (List.length keys));
      let objects = List.map keys ~f:(fun key -> { Xml.key }) in
      let xml =
        Xml.delete_request_to_xmlm { quiet = false; objects }
        |> Internal.set_xml_name "Delete"
      in
      let xml_body = Ezxmlm.to_string [ xml ] in
      let content_md5 =
        Digestif.MD5.(digest_string xml_body |> to_raw_string)
        |> Base64.encode_exn
      in
      let extra_headers =
        [ ("content-md5", content_md5); ("content-type", "application/xml") ]
      in
      let* result =
        call_s3 conn ~meth:POST ~bucket ~path:"/" ~query:"delete="
          ~payload:xml_body ~extra_headers
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> (
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              match
                Internal.Xml.decode resp.body ~name:"DeleteResult"
                  Xml.delete_result_of_xmlm_exn
              with
              | Error _ as error -> error
              | Ok result ->
                  let deleted =
                    List.map result.deleted
                      ~f:(fun (object_ : Xml.delete_object) ->
                        { Delete_result.Deleted.key = object_.key })
                  in
                  let errors =
                    List.map result.errors ~f:(fun (error : Xml.delete_error) ->
                        {
                          Delete_result.Error.key = error.key;
                          code = error.code;
                          message = error.message;
                        })
                  in
                  Ok { Delete_result.deleted; errors }))

    let copy conn ~src_bucket ~src_key ~dst_bucket ~dst_key ?if_match
        ?if_none_match ?metadata_directive ?(metadata = []) () =
      Internal.Log.debug (fun m ->
          m "COPY %s/%s → %s/%s" src_bucket src_key dst_bucket dst_key);
      let path = "/" ^ dst_key in
      let copy_source = "/" ^ src_bucket ^ "/" ^ src_key in
      let extra_headers =
        ("x-amz-copy-source", copy_source)
        :: Internal.Metadata_headers.to_headers metadata
        |> Internal.opt_header "x-amz-copy-source-if-match" if_match
        |> Internal.opt_header "x-amz-copy-source-if-none-match" if_none_match
      in
      let extra_headers =
        match metadata_directive with
        | Some `Copy -> ("x-amz-metadata-directive", "COPY") :: extra_headers
        | Some `Replace ->
            ("x-amz-metadata-directive", "REPLACE") :: extra_headers
        | None -> extra_headers
      in
      let* result =
        call_s3 conn ~meth:PUT ~bucket:dst_bucket ~path ~query:"" ~payload:""
          ~extra_headers
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> (
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              match
                Internal.Xml.decode resp.body ~name:"CopyObjectResult"
                  Xml.copy_result_of_xmlm_exn
              with
              | Error _ as error -> error
              | Ok result ->
                  Ok
                    {
                      Copy_result.etag = result.etag;
                      last_modified = result.last_modified;
                    }))

    let list conn ~bucket ~prefix ?delimiter ?(max_keys = 1000) ?start_after
        ?continuation_token () =
      Internal.Log.debug (fun m -> m "LIST %s prefix=%s" bucket prefix);
      if max_keys <= 0 then
        R.return (Error (`Invalid_request "max_keys must be positive"))
      else
        let qp =
          [
            ("list-type", "2");
            ("prefix", prefix);
            ("max-keys", Int.to_string max_keys);
          ]
        in
        let qp =
          ( ( qp |> fun query_params ->
              match delimiter with
              | Some delimiter -> ("delimiter", delimiter) :: query_params
              | None -> query_params )
          |> fun query_params ->
            match start_after with
            | Some start_after -> ("start-after", start_after) :: query_params
            | None -> query_params )
          |> fun query_params ->
          match continuation_token with
          | Some continuation_token ->
              ("continuation-token", continuation_token) :: query_params
          | None -> query_params
        in
        let query =
          Uri.encoded_of_query
            (List.map qp ~f:(fun (key, value) -> (key, [ value ])))
        in
        let* result =
          call_s3 conn ~meth:GET ~bucket ~path:"/" ~query ~payload:""
            ~extra_headers:[]
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> (
              if not (Awskit.Response.is_success resp) then
                Error (S3_error.of_status resp.status resp.body)
              else
                match
                  Internal.Xml.decode resp.body ~name:"ListBucketResult"
                    Xml.list_result_of_xmlm_exn
                with
                | Error _ as error -> error
                | Ok result ->
                    let keys =
                      List.map result.contents
                        ~f:(fun (content : Xml.list_content) -> content.key)
                    in
                    Ok
                      {
                        List_result.keys;
                        is_truncated = result.is_truncated;
                        next_continuation_token = result.next_continuation_token;
                      }))
  end

  (* ── Bucket operations ──────────────────────────────────────── *)

  module Bucket = struct
    type bucket_policy = S3_policy.t

    include Bucket

    let do_get conn bucket query =
      call_s3 conn ~meth:GET ~bucket ~path:"/" ~query ~payload:""
        ~extra_headers:[]

    let do_put conn bucket query body extra_headers =
      call_s3 conn ~meth:PUT ~bucket ~path:"/" ~query ~payload:body
        ~extra_headers

    let do_delete conn bucket query =
      call_s3 conn ~meth:DELETE ~bucket ~path:"/" ~query ~payload:""
        ~extra_headers:[]

    let create conn ~bucket ?region () =
      Internal.Log.debug (fun m -> m "CREATE-BUCKET %s" bucket);
      let body =
        match region with
        | Some region when not (String.equal region "us-east-1") ->
            let xml =
              Xml.create_bucket_config_to_xmlm { location_constraint = region }
              |> Internal.set_xml_name "CreateBucketConfiguration"
            in
            Ezxmlm.to_string [ xml ]
        | _ -> ""
      in
      let extra_headers =
        if String.is_empty body then []
        else [ ("content-type", "application/xml") ]
      in
      let* result = do_put conn bucket "" body extra_headers in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> ok_or_err resp.status resp.body)

    let delete conn ~bucket =
      Internal.Log.debug (fun m -> m "DELETE-BUCKET %s" bucket);
      let* result = do_delete conn bucket "" in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> ok_or_err resp.status resp.body)

    let head conn ~bucket =
      Internal.Log.debug (fun m -> m "HEAD-BUCKET %s" bucket);
      let* result =
        call_s3 conn ~meth:HEAD ~bucket ~path:"/" ~query:"" ~payload:""
          ~extra_headers:[]
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp ->
            if Awskit.Response.is_success resp then Ok true
            else if resp.status = 404 then Ok false
            else Error (S3_error.of_status resp.status resp.body))

    let list conn =
      Internal.Log.debug (fun m -> m "LIST-BUCKETS");
      let* result =
        call_s3_root conn ~meth:GET ~path:"/" ~query:"" ~payload:""
          ~extra_headers:[]
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> (
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              match
                Internal.Xml.decode resp.body ~name:"ListBucketsResult"
                  Xml.list_result_of_xmlm_exn
              with
              | Error _ as error -> error
              | Ok result ->
                  Ok
                    (List.map result.buckets.bucket
                       ~f:(fun (b : Xml.bucket_info) ->
                         { Info.name = b.name; creation_date = b.creation_date }))
            ))

    let get_location conn ~bucket =
      let* result = do_get conn bucket "location=" in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> (
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              match
                Internal.Xml.decode resp.body ~name:"GetBucketLocationResult"
                  Xml.location_of_xmlm_exn
              with
              | Error _ as error -> error
              | Ok result ->
                  Ok (Option.value result.location ~default:"us-east-1")))

    module Policy = struct
      include Bucket.Policy_

      let get conn ~bucket =
        let* result = do_get conn bucket "policy=" in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp ->
              if Awskit.Response.is_success resp then
                S3_policy.of_json resp.body
              else Error (S3_error.of_status resp.status resp.body))

      let put conn ~bucket policy =
        let body = S3_policy.to_json policy in
        let* result =
          do_put conn bucket "policy=" body
            [ ("content-type", "application/json") ]
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> ok_or_err resp.status resp.body)

      let delete conn ~bucket =
        let* result = do_delete conn bucket "policy=" in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> ok_or_err resp.status resp.body)
    end

    module Versioning = struct
      include Bucket.Versioning

      let get conn ~bucket =
        let* result = do_get conn bucket "versioning=" in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> (
              if not (Awskit.Response.is_success resp) then
                Error (S3_error.of_status resp.status resp.body)
              else
                match
                  Internal.Xml.decode resp.body ~name:"VersioningConfiguration"
                    Xml.versioning_config_of_xmlm_exn
                with
                | Error _ as error -> error
                | Ok result -> (
                    match result.status with
                    | None -> Ok None
                    | Some status -> (
                        match Status.of_string status with
                        | Some status -> Ok (Some status)
                        | None ->
                            Error
                              (`Invalid_response
                                 (Fmt.str "invalid versioning status %S" status))
                        ))))

      let put conn ~bucket status =
        let xml =
          Xml.versioning_config_to_xmlm
            { status = Some (Status.to_string status) }
          |> Internal.set_xml_name "VersioningConfiguration"
        in
        let body = Ezxmlm.to_string [ xml ] in
        let* result =
          do_put conn bucket "versioning=" body
            [ ("content-type", "application/xml") ]
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> ok_or_err resp.status resp.body)
    end

    module Tagging = struct
      include Bucket.Tagging

      let get conn ~bucket =
        let* result = do_get conn bucket "tagging=" in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> (
              if not (Awskit.Response.is_success resp) then
                Error (S3_error.of_status resp.status resp.body)
              else
                match
                  Internal.Xml.decode resp.body ~name:"Tagging"
                    Tagging_xml.tagging_of_xmlm_exn
                with
                | Error _ as error -> error
                | Ok result ->
                    Ok
                      (List.map result.tag_set.tags
                         ~f:(fun (tag : Tagging_xml.tag) ->
                           { Tag.key = tag.key; value = tag.value }))))

      let put conn ~bucket tags =
        let xml_tags =
          List.map tags ~f:(fun (tag : Tag.t) ->
              { Tagging_xml.key = tag.key; value = tag.value })
        in
        let body =
          Tagging_xml.xml_of_tagging { tag_set = { tags = xml_tags } }
          |> fun xml -> Ezxmlm.to_string [ xml ]
        in
        let content_md5 =
          Digestif.MD5.(digest_string body |> to_raw_string)
          |> Base64.encode_exn
        in
        let* result =
          do_put conn bucket "tagging=" body
            [
              ("content-md5", content_md5); ("content-type", "application/xml");
            ]
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> ok_or_err resp.status resp.body)

      let delete conn ~bucket =
        let* result = do_delete conn bucket "tagging=" in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> ok_or_err resp.status resp.body)
    end

    module Encryption = struct
      include Bucket.Encryption

      let get conn ~bucket =
        let* result = do_get conn bucket "encryption=" in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> (
              if not (Awskit.Response.is_success resp) then
                Error (S3_error.of_status resp.status resp.body)
              else
                match
                  Internal.Xml.decode resp.body
                    ~name:"ServerSideEncryptionConfiguration"
                    Xml.encryption_config_of_xmlm_exn
                with
                | Error _ as error -> error
                | Ok config ->
                    let rules =
                      List.map config.rules
                        ~f:(fun (rule : Xml.encryption_rule) ->
                          match
                            Algorithm.of_string rule.apply.sse_algorithm
                          with
                          | Some sse_algorithm ->
                              Ok
                                {
                                  Rule.sse_algorithm;
                                  kms_master_key_id =
                                    rule.apply.kms_master_key_id;
                                }
                          | None ->
                              Error
                                (`Invalid_response
                                   (Fmt.str "invalid encryption algorithm %S"
                                      rule.apply.sse_algorithm)))
                    in
                    Result.map (all_results rules) ~f:(fun rules ->
                        { Config.rules })))

      let put conn ~bucket (config : Config.t) =
        let xml_rules =
          List.map config.rules ~f:(fun (rule : Rule.t) ->
              {
                Xml.apply =
                  {
                    Xml.sse_algorithm = Algorithm.to_string rule.sse_algorithm;
                    kms_master_key_id = rule.kms_master_key_id;
                  };
              })
        in
        let xml =
          Xml.encryption_config_to_xmlm { rules = xml_rules }
          |> Internal.set_xml_name "ServerSideEncryptionConfiguration"
        in
        let body = Ezxmlm.to_string [ xml ] in
        let content_md5 =
          Digestif.MD5.(digest_string body |> to_raw_string)
          |> Base64.encode_exn
        in
        let* result =
          do_put conn bucket "encryption=" body
            [
              ("content-md5", content_md5); ("content-type", "application/xml");
            ]
        in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> ok_or_err resp.status resp.body)

      let delete conn ~bucket =
        let* result = do_delete conn bucket "encryption=" in
        R.return
          (match result with
          | Error e -> Error (S3_error.of_aws_error e)
          | Ok resp -> ok_or_err resp.status resp.body)
    end
  end

  (* ── Multipart operations ───────────────────────────────────── *)

  module Multipart = struct
    include Multipart

    let create conn ~bucket ~key ?content_type ?metadata ?storage_class () =
      Internal.Log.debug (fun m -> m "CREATE-MULTIPART %s/%s" bucket key);
      let path = "/" ^ key in
      let query = "uploads=" in
      let extra_headers =
        ( (match metadata with
            | Some metadata -> Internal.Metadata_headers.to_headers metadata
            | None -> [])
        |> fun headers ->
          match content_type with
          | Some ct -> ("content-type", ct) :: headers
          | None -> headers )
        |> fun headers ->
        match storage_class with
        | Some sc ->
            ("x-amz-storage-class", Storage_class.to_string sc) :: headers
        | None -> headers
      in
      let* result =
        call_s3 conn ~meth:POST ~bucket ~path ~query ~payload:"" ~extra_headers
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> (
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              match
                Internal.Xml.decode resp.body
                  ~name:"InitiateMultipartUploadResult"
                  Xml.initiate_result_of_xmlm_exn
              with
              | Error _ as error -> error
              | Ok result -> Ok { Upload.upload_id = result.upload_id; key }))

    let upload_part conn ~bucket ~key ~upload_id ~part_number body =
      Internal.Log.debug (fun m ->
          m "UPLOAD-PART %s/%s #%d (%d bytes)" bucket key part_number
            (String.length body));
      let path = "/" ^ key in
      let query =
        Uri.encoded_of_query
          [
            ("partNumber", [ Int.to_string part_number ]);
            ("uploadId", [ upload_id ]);
          ]
      in
      let extra_headers =
        [ ("content-length", Int.to_string (String.length body)) ]
      in
      let* result =
        call_s3 conn ~meth:PUT ~bucket ~path ~query ~payload:body ~extra_headers
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp ->
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              Result.map (Awskit.Response.header_exn resp "etag")
                ~f:(fun etag -> { Part.part_number; etag }))

    let complete conn ~bucket ~key ~upload_id parts =
      Internal.Log.debug (fun m ->
          m "COMPLETE-MULTIPART %s/%s (%d parts)" bucket key (List.length parts));
      let xml_parts =
        List.map parts ~f:(fun (part : Part.t) ->
            { Xml.part_number = part.part_number; etag = part.etag })
      in
      let xml =
        Xml.complete_request_to_xmlm { parts = xml_parts }
        |> Internal.set_xml_name "CompleteMultipartUpload"
      in
      let xml_body = Ezxmlm.to_string [ xml ] in
      let path = "/" ^ key in
      let query = Uri.encoded_of_query [ ("uploadId", [ upload_id ]) ] in
      let* result =
        call_s3 conn ~meth:POST ~bucket ~path ~query ~payload:xml_body
          ~extra_headers:[ ("content-type", "application/xml") ]
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> (
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              match
                Internal.Xml.decode resp.body
                  ~name:"CompleteMultipartUploadResult"
                  Xml.complete_result_of_xmlm_exn
              with
              | Error _ as error -> error
              | Ok result -> Ok { Object_.Put_result.etag = result.etag }))

    let abort conn ~bucket ~key ~upload_id =
      Internal.Log.debug (fun m -> m "ABORT-MULTIPART %s/%s" bucket key);
      let path = "/" ^ key in
      let query = Uri.encoded_of_query [ ("uploadId", [ upload_id ]) ] in
      let* result =
        call_s3 conn ~meth:DELETE ~bucket ~path ~query ~payload:""
          ~extra_headers:[]
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp ->
            if Awskit.Response.is_success resp then Ok ()
            else Error (S3_error.of_status resp.status resp.body))

    let list_parts conn ~bucket ~key ~upload_id =
      Internal.Log.debug (fun m -> m "LIST-PARTS %s/%s" bucket key);
      let path = "/" ^ key in
      let query = Uri.encoded_of_query [ ("uploadId", [ upload_id ]) ] in
      let* result =
        call_s3 conn ~meth:GET ~bucket ~path ~query ~payload:""
          ~extra_headers:[]
      in
      R.return
        (match result with
        | Error e -> Error (S3_error.of_aws_error e)
        | Ok resp -> (
            if not (Awskit.Response.is_success resp) then
              Error (S3_error.of_status resp.status resp.body)
            else
              match
                Internal.Xml.decode resp.body ~name:"ListPartsResult"
                  Xml.list_parts_result_of_xmlm_exn
              with
              | Error _ as error -> error
              | Ok result ->
                  Ok
                    (List.map result.parts
                       ~f:(fun (part : Xml.list_part_info) ->
                         {
                           Part_info.part_number = part.part_number;
                           etag = part.etag;
                           size = part.size;
                           last_modified = part.last_modified;
                         }))))
  end

  (* ── Presigned URLs ─────────────────────────────────────────── *)

  module Presigned = struct
    let get_object conn ~bucket ~key ?expires_in () =
      Presigned.get_object ~region:(R.region conn)
        ~credentials:(R.credentials conn) ~now:(R.clock conn) ~host:(host conn)
        ?port:(R.endpoint_port conn) ~bucket ~key ?expires_in ()

    let put_object conn ~bucket ~key ?expires_in ?content_type () =
      Presigned.put_object ~region:(R.region conn)
        ~credentials:(R.credentials conn) ~now:(R.clock conn) ~host:(host conn)
        ?port:(R.endpoint_port conn) ~bucket ~key ?expires_in ?content_type ()
  end
end
