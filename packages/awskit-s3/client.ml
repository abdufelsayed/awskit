module S3_error = Error
module S3_policy = Policy
open Base

module Make (R : Awskit.Runtime.S) = struct
  let ( let* ) = R.bind

  (* ── helpers ──────────────────────────────────────────────────── *)

  let endpoint conn =
    match R.endpoint conn with
    | Some endpoint -> endpoint
    | None ->
        Awskit.Endpoint.https
          ~host:(Fmt.str "s3.%s.amazonaws.com" (R.region conn))
          ()

  let host conn = Awskit.Endpoint.host (endpoint conn)
  let port conn = Awskit.Endpoint.port (endpoint conn)
  let request_host conn = Awskit.Endpoint.authority (endpoint conn)

  let sign conn ~meth ~path ~query ~payload ~extra_headers =
    let headers = ("host", request_host conn) :: extra_headers in
    Awskit.Signing.sign_request ~credentials:(R.credentials conn)
      ~region:(R.region conn) ~service:"s3" ~meth ~path ~query ~headers ~payload
      ~now:(R.clock conn)

  let sign_params conn ~meth ~path ~query_params ~payload ~extra_headers =
    let headers = ("host", request_host conn) :: extra_headers in
    Awskit.Signing.sign_request_params ~credentials:(R.credentials conn)
      ~region:(R.region conn) ~service:"s3" ~meth ~path ~query_params ~headers
      ~payload ~now:(R.clock conn)

  let call_s3 conn ~(meth : Awskit.Request.meth) ~bucket ~path ~sign_path ~query
      ~payload ~extra_headers =
    let s3_path = Fmt.str "/%s%s" bucket sign_path in
    let signed =
      sign conn
        ~meth:(Awskit.Request.meth_to_string meth)
        ~path:s3_path ~query ~payload ~extra_headers
    in
    R.call conn
      {
        Awskit.Request.meth;
        host = host conn;
        port = port conn;
        path = Internal.resource_path bucket path query;
        headers = signed.headers;
        body = payload;
      }

  let call_s3_qp conn ~(meth : Awskit.Request.meth) ~bucket ~path ~sign_path
      ~query_params ~payload ~extra_headers =
    let query = Internal.query_of_params query_params in
    let s3_path = Fmt.str "/%s%s" bucket sign_path in
    let signed =
      sign_params conn
        ~meth:(Awskit.Request.meth_to_string meth)
        ~path:s3_path ~query_params ~payload ~extra_headers
    in
    R.call conn
      {
        Awskit.Request.meth;
        host = host conn;
        port = port conn;
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
        port = port conn;
        path = (if String.is_empty query then path else path ^ "?" ^ query);
        headers = signed.headers;
        body = payload;
      }

  let ok_or_err status body =
    if status >= 200 && status < 300 then Ok ()
    else Error (S3_error.of_status status body)

  let validate_bucket_key bucket key =
    let open Internal.Validation in
    let open Internal.Let_syntax in
    let* () = validate_bucket bucket in
    let* () = validate_key key in
    Ok ()

  let validate_bucket_only bucket = Internal.Validation.validate_bucket bucket
  let etag_condition_header = Object_.Etag_condition.to_header

  let opt_etag_condition_header name condition headers =
    match condition with
    | None -> headers
    | Some condition -> (name, etag_condition_header condition) :: headers

  let validate_etag_condition name condition =
    Internal.Validation.validate_range_header_value name
      (Option.map condition ~f:etag_condition_header)

  let read_precondition_headers
      ({
         Object_.Preconditions.Read.if_match;
         if_none_match;
         if_modified_since;
         if_unmodified_since;
       } :
        Object_.Preconditions.Read.t) =
    []
    |> opt_etag_condition_header "if-match" if_match
    |> opt_etag_condition_header "if-none-match" if_none_match
    |> Internal.opt_header "if-modified-since" if_modified_since
    |> Internal.opt_header "if-unmodified-since" if_unmodified_since

  let validate_read_preconditions
      ({
         Object_.Preconditions.Read.if_match;
         if_none_match;
         if_modified_since;
         if_unmodified_since;
       } :
        Object_.Preconditions.Read.t) =
    let open Internal.Let_syntax in
    let* () = validate_etag_condition "if-match" if_match in
    let* () = validate_etag_condition "if-none-match" if_none_match in
    let* () =
      Internal.Validation.validate_range_header_value "if-modified-since"
        if_modified_since
    in
    Internal.Validation.validate_range_header_value "if-unmodified-since"
      if_unmodified_since

  let write_precondition_headers
      ({ Object_.Preconditions.Write.if_match; if_none_match } :
        Object_.Preconditions.Write.t) =
    []
    |> opt_etag_condition_header "if-match" if_match
    |> opt_etag_condition_header "if-none-match" if_none_match

  let validate_write_preconditions
      ({ Object_.Preconditions.Write.if_match; if_none_match } :
        Object_.Preconditions.Write.t) =
    let open Internal.Let_syntax in
    let* () = validate_etag_condition "if-match" if_match in
    validate_etag_condition "if-none-match" if_none_match

  let delete_precondition_headers
      ({
         Object_.Preconditions.Delete.if_match;
         if_match_last_modified_time;
         if_match_size;
       } :
        Object_.Preconditions.Delete.t) =
    []
    |> opt_etag_condition_header "if-match" if_match
    |> Internal.opt_header "x-amz-if-match-last-modified-time"
         if_match_last_modified_time
    |> Internal.opt_header "x-amz-if-match-size"
         (Option.map if_match_size ~f:Int.to_string)

  let validate_delete_preconditions
      ({
         Object_.Preconditions.Delete.if_match;
         if_match_last_modified_time;
         if_match_size;
       } :
        Object_.Preconditions.Delete.t) =
    let open Internal.Let_syntax in
    let* () = validate_etag_condition "if-match" if_match in
    let* () =
      Internal.Validation.validate_range_header_value
        "x-amz-if-match-last-modified-time" if_match_last_modified_time
    in
    match if_match_size with
    | Some size when size < 0 ->
        Error (`Invalid_request "x-amz-if-match-size must be non-negative")
    | _ -> Ok ()

  let copy_source_precondition_headers
      ({
         Object_.Preconditions.Copy_source.if_match;
         if_none_match;
         if_modified_since;
         if_unmodified_since;
       } :
        Object_.Preconditions.Copy_source.t) =
    []
    |> opt_etag_condition_header "x-amz-copy-source-if-match" if_match
    |> opt_etag_condition_header "x-amz-copy-source-if-none-match" if_none_match
    |> Internal.opt_header "x-amz-copy-source-if-modified-since"
         if_modified_since
    |> Internal.opt_header "x-amz-copy-source-if-unmodified-since"
         if_unmodified_since

  let validate_copy_source_preconditions
      ({
         Object_.Preconditions.Copy_source.if_match;
         if_none_match;
         if_modified_since;
         if_unmodified_since;
       } :
        Object_.Preconditions.Copy_source.t) =
    let open Internal.Let_syntax in
    let* () = validate_etag_condition "x-amz-copy-source-if-match" if_match in
    let* () =
      validate_etag_condition "x-amz-copy-source-if-none-match" if_none_match
    in
    let* () =
      Internal.Validation.validate_range_header_value
        "x-amz-copy-source-if-modified-since" if_modified_since
    in
    Internal.Validation.validate_range_header_value
      "x-amz-copy-source-if-unmodified-since" if_unmodified_since

  let validate_common_object_headers ?content_type ?cache_control
      ?content_encoding ?content_disposition () =
    let open Internal.Validation in
    let open Internal.Let_syntax in
    let* () = validate_range_header_value "content-type" content_type in
    let* () = validate_range_header_value "cache-control" cache_control in
    let* () = validate_range_header_value "content-encoding" content_encoding in
    let* () =
      validate_range_header_value "content-disposition" content_disposition
    in
    Ok ()

  (* ── Object operations ────────────────────────────────────────── *)

  module Object = struct
    include Object_

    module Tagging = struct
      include Object_.Tagging

      let get conn ~bucket ~key =
        Internal.Log.debug (fun m -> m "GET-OBJECT-TAGGING %s/%s" bucket key);
        match validate_bucket_key bucket key with
        | Error _ as error -> R.return error
        | Ok () ->
            let raw_path = Internal.raw_object_key_path key in
            let path = Internal.encode_object_key_path key in
            let query = Internal.query_of_params [ ("tagging", []) ] in
            let* result =
              call_s3 conn ~meth:GET ~bucket ~path ~sign_path:raw_path ~query
                ~payload:"" ~extra_headers:[]
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
        match
          Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
              Internal.Validation.validate_tags tags)
        with
        | Error _ as error -> R.return error
        | Ok () ->
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
              [
                ("content-md5", content_md5); ("content-type", "application/xml");
              ]
            in
            let raw_path = Internal.raw_object_key_path key in
            let path = Internal.encode_object_key_path key in
            let query = Internal.query_of_params [ ("tagging", []) ] in
            let* result =
              call_s3 conn ~meth:PUT ~bucket ~path ~sign_path:raw_path ~query
                ~payload:xml_body ~extra_headers
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp ->
                  if Awskit.Response.is_success resp then Ok ()
                  else Error (S3_error.of_status resp.status resp.body))

      let delete conn ~bucket ~key =
        Internal.Log.debug (fun m -> m "DELETE-OBJECT-TAGGING %s/%s" bucket key);
        match validate_bucket_key bucket key with
        | Error _ as error -> R.return error
        | Ok () ->
            let raw_path = Internal.raw_object_key_path key in
            let path = Internal.encode_object_key_path key in
            let query = Internal.query_of_params [ ("tagging", []) ] in
            let* result =
              call_s3 conn ~meth:DELETE ~bucket ~path ~sign_path:raw_path ~query
                ~payload:"" ~extra_headers:[]
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp ->
                  if Awskit.Response.is_success resp then Ok ()
                  else Error (S3_error.of_status resp.status resp.body))
    end

    let put conn ~bucket ~key ?(preconditions = Preconditions.Write.none)
        ?content_type ?(metadata = []) ?storage_class ?tags ?cache_control
        ?content_encoding ?content_disposition body =
      Internal.Log.debug (fun m ->
          m "PUT %s/%s (%d bytes)" bucket key (String.length body));
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            let open Internal.Validation in
            let open Internal.Let_syntax in
            let* () = validate_metadata metadata in
            let* () = validate_write_preconditions preconditions in
            let* () =
              validate_common_object_headers ?content_type ?cache_control
                ?content_encoding ?content_disposition ()
            in
            match tags with Some tags -> validate_tags tags | None -> Ok ())
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let extra_headers =
            ("content-length", Int.to_string (String.length body))
            :: Internal.Metadata_headers.to_headers metadata
          in
          let extra_headers =
            extra_headers
            |> List.append (write_precondition_headers preconditions)
            |> Internal.opt_header "content-type" content_type
            |> Internal.opt_header "cache-control" cache_control
            |> Internal.opt_header "content-encoding" content_encoding
            |> Internal.opt_header "content-disposition" content_disposition
          in
          let extra_headers =
            match storage_class with
            | Some sc ->
                ("x-amz-storage-class", Storage_class.to_string sc)
                :: extra_headers
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
            call_s3 conn ~meth:PUT ~bucket ~path ~sign_path:raw_path ~query:""
              ~payload:body ~extra_headers
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

    let get conn ~bucket ~key ?range ?(preconditions = Preconditions.Read.none)
        () =
      Internal.Log.debug (fun m -> m "GET %s/%s" bucket key);
      let range_header =
        match range with
        | None -> Ok None
        | Some range -> Result.map (Range.to_header range) ~f:Option.some
      in
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            let open Internal.Validation in
            let open Internal.Let_syntax in
            let* () = validate_read_preconditions preconditions in
            Result.map range_header ~f:(fun _ -> ()))
      with
      | Error _ as error -> R.return error
      | Ok () -> (
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let extra_headers = read_precondition_headers preconditions in
          match
            Result.map range_header ~f:(function
              | Some rh -> ("range", rh) :: extra_headers
              | None -> extra_headers)
          with
          | Error _ as error -> R.return error
          | Ok extra_headers ->
              let* result =
                call_s3 conn ~meth:GET ~bucket ~path ~sign_path:raw_path
                  ~query:"" ~payload:"" ~extra_headers
              in
              R.return
                (match result with
                | Error e -> Error (S3_error.of_aws_error e)
                | Ok resp ->
                    if not (Awskit.Response.is_success resp) then
                      Error (S3_error.of_status resp.status resp.body)
                    else get_result_of_response resp))

    let head conn ~bucket ~key ?(preconditions = Preconditions.Read.none) () =
      Internal.Log.debug (fun m -> m "HEAD %s/%s" bucket key);
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            validate_read_preconditions preconditions)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let extra_headers = read_precondition_headers preconditions in
          let* result =
            call_s3 conn ~meth:HEAD ~bucket ~path ~sign_path:raw_path ~query:""
              ~payload:"" ~extra_headers
          in
          R.return
            (match result with
            | Error e -> Error (S3_error.of_aws_error e)
            | Ok resp ->
                if Awskit.Response.is_success resp then
                  Result.map (info_of_response resp) ~f:Option.some
                else if resp.status = 404 then Ok None
                else Error (S3_error.of_status resp.status resp.body))

    let delete conn ~bucket ~key ?(preconditions = Preconditions.Delete.none) ()
        =
      Internal.Log.debug (fun m -> m "DELETE %s/%s" bucket key);
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            validate_delete_preconditions preconditions)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let extra_headers = delete_precondition_headers preconditions in
          let* result =
            call_s3 conn ~meth:DELETE ~bucket ~path ~sign_path:raw_path
              ~query:"" ~payload:"" ~extra_headers
          in
          R.return
            (match result with
            | Error e -> Error (S3_error.of_aws_error e)
            | Ok resp ->
                if Awskit.Response.is_success resp then Ok ()
                else Error (S3_error.of_status resp.status resp.body))

    module Delete_object = struct
      type t = {
        key : string;
        version_id : string option;
        etag : string option;
        last_modified_time : string option;
        size : int option;
      }
      [@@deriving show, eq]

      let v ?version_id ?etag ?last_modified_time ?size key =
        { key; version_id; etag; last_modified_time; size }
    end

    let validate_delete_object (object_ : Delete_object.t) =
      let open Internal.Let_syntax in
      let* () = Internal.Validation.validate_key object_.key in
      let* () =
        Internal.Validation.validate_range_header_value "VersionId"
          object_.version_id
      in
      let* () =
        Internal.Validation.validate_range_header_value "ETag" object_.etag
      in
      let* () =
        Internal.Validation.validate_range_header_value "LastModifiedTime"
          object_.last_modified_time
      in
      match object_.size with
      | Some size when size < 0 ->
          Error (`Invalid_request "Size must be non-negative")
      | _ -> Ok ()

    let delete_batch conn ~bucket ~objects =
      Internal.Log.debug (fun m ->
          m "DELETE-BATCH %s (%d keys)" bucket (List.length objects));
      match
        Result.bind (validate_bucket_only bucket) ~f:(fun () ->
            let open Internal.Let_syntax in
            let* () =
              Internal.Validation.validate_delete_batch_keys
                (List.map objects ~f:(fun object_ -> object_.Delete_object.key))
            in
            List.fold objects ~init:(Ok ()) ~f:(fun acc object_ ->
                match acc with
                | Error _ as error -> error
                | Ok () -> validate_delete_object object_))
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let objects =
            List.map objects ~f:(fun object_ ->
                {
                  Xml.key = object_.key;
                  version_id = object_.version_id;
                  etag = object_.etag;
                  last_modified_time = object_.last_modified_time;
                  size = object_.size;
                })
          in
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
            [
              ("content-md5", content_md5); ("content-type", "application/xml");
            ]
          in
          let* result =
            call_s3_qp conn ~meth:POST ~bucket ~path:"/" ~sign_path:"/"
              ~query_params:[ ("delete", []) ]
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
                        List.map result.errors
                          ~f:(fun (error : Xml.delete_error) ->
                            {
                              Delete_result.Error.key = error.key;
                              code = error.code;
                              message = error.message;
                            })
                      in
                      Ok { Delete_result.deleted; errors }))

    let copy conn ~src_bucket ~src_key ~dst_bucket ~dst_key
        ?(source_preconditions = Preconditions.Copy_source.none)
        ?metadata_directive ?(metadata = []) () =
      Internal.Log.debug (fun m ->
          m "COPY %s/%s → %s/%s" src_bucket src_key dst_bucket dst_key);
      match
        Result.bind (validate_bucket_key src_bucket src_key) ~f:(fun () ->
            let open Internal.Validation in
            let open Internal.Let_syntax in
            let* () = validate_bucket_key dst_bucket dst_key in
            let* () = validate_metadata metadata in
            validate_copy_source_preconditions source_preconditions)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path dst_key in
          let path = Internal.encode_object_key_path dst_key in
          let copy_source =
            Internal.encoded_copy_source ~bucket:src_bucket ~key:src_key
          in
          let extra_headers =
            ("x-amz-copy-source", copy_source)
            :: Internal.Metadata_headers.to_headers metadata
            |> List.append
                 (copy_source_precondition_headers source_preconditions)
          in
          let extra_headers =
            match metadata_directive with
            | Some `Copy ->
                ("x-amz-metadata-directive", "COPY") :: extra_headers
            | Some `Replace ->
                ("x-amz-metadata-directive", "REPLACE") :: extra_headers
            | None -> extra_headers
          in
          let* result =
            call_s3 conn ~meth:PUT ~bucket:dst_bucket ~path ~sign_path:raw_path
              ~query:"" ~payload:"" ~extra_headers
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
      match
        Result.bind (validate_bucket_only bucket) ~f:(fun () ->
            let open Internal.Validation in
            let open Internal.Let_syntax in
            let* () = validate_max_keys max_keys in
            let* () = ensure_no_ctl ~field:"prefix" prefix in
            let* () = validate_range_header_value "delimiter" delimiter in
            let* () = validate_range_header_value "start_after" start_after in
            validate_range_header_value "continuation_token" continuation_token)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let params =
            [
              ("list-type", [ "2" ]);
              ("prefix", [ prefix ]);
              ("max-keys", [ Int.to_string max_keys ]);
            ]
          in
          let params =
            match delimiter with
            | Some delimiter -> ("delimiter", [ delimiter ]) :: params
            | None -> params
          in
          let params =
            match start_after with
            | Some start_after -> ("start-after", [ start_after ]) :: params
            | None -> params
          in
          let params =
            match continuation_token with
            | Some continuation_token ->
                ("continuation-token", [ continuation_token ]) :: params
            | None -> params
          in
          let* result =
            call_s3_qp conn ~meth:GET ~bucket ~path:"/" ~sign_path:"/"
              ~query_params:params ~payload:"" ~extra_headers:[]
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
                          next_continuation_token =
                            result.next_continuation_token;
                        }))
  end

  (* ── Bucket operations ──────────────────────────────────────── *)

  module Bucket = struct
    type bucket_policy = S3_policy.t

    include Bucket

    let do_get conn bucket query =
      call_s3 conn ~meth:GET ~bucket ~path:"/" ~sign_path:"/" ~query ~payload:""
        ~extra_headers:[]

    let do_put conn bucket query body extra_headers =
      call_s3 conn ~meth:PUT ~bucket ~path:"/" ~sign_path:"/" ~query
        ~payload:body ~extra_headers

    let do_delete conn bucket query =
      call_s3 conn ~meth:DELETE ~bucket ~path:"/" ~sign_path:"/" ~query
        ~payload:"" ~extra_headers:[]

    let create conn ~bucket ?region () =
      Internal.Log.debug (fun m -> m "CREATE-BUCKET %s" bucket);
      match validate_bucket_only bucket with
      | Error _ as error -> R.return error
      | Ok () ->
          let region = Option.value region ~default:(R.region conn) in
          let body =
            if String.equal region "us-east-1" then ""
            else
              let xml =
                Xml.create_bucket_config_to_xmlm
                  { location_constraint = region }
                |> Internal.set_xml_name "CreateBucketConfiguration"
              in
              Ezxmlm.to_string [ xml ]
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
      match validate_bucket_only bucket with
      | Error _ as error -> R.return error
      | Ok () ->
          let* result = do_delete conn bucket "" in
          R.return
            (match result with
            | Error e -> Error (S3_error.of_aws_error e)
            | Ok resp -> ok_or_err resp.status resp.body)

    let head conn ~bucket =
      Internal.Log.debug (fun m -> m "HEAD-BUCKET %s" bucket);
      match validate_bucket_only bucket with
      | Error _ as error -> R.return error
      | Ok () ->
          let* result =
            call_s3 conn ~meth:HEAD ~bucket ~path:"/" ~sign_path:"/" ~query:""
              ~payload:"" ~extra_headers:[]
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
      match validate_bucket_only bucket with
      | Error _ as error -> R.return error
      | Ok () ->
          let* result =
            do_get conn bucket (Internal.query_of_params [ ("location", []) ])
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
                      ~name:"GetBucketLocationResult" Xml.location_of_xmlm_exn
                  with
                  | Error _ as error -> error
                  | Ok result ->
                      Ok (Option.value result.location ~default:"us-east-1")))

    module Policy = struct
      include Bucket.Policy_

      let get conn ~bucket =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let* result =
              do_get conn bucket (Internal.query_of_params [ ("policy", []) ])
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp ->
                  if Awskit.Response.is_success resp then
                    S3_policy.of_json resp.body
                  else Error (S3_error.of_status resp.status resp.body))

      let put conn ~bucket policy =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let body = S3_policy.to_json policy in
            let* result =
              do_put conn bucket
                (Internal.query_of_params [ ("policy", []) ])
                body
                [ ("content-type", "application/json") ]
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp -> ok_or_err resp.status resp.body)

      let delete conn ~bucket =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let* result =
              do_delete conn bucket
                (Internal.query_of_params [ ("policy", []) ])
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp -> ok_or_err resp.status resp.body)
    end

    module Versioning = struct
      include Bucket.Versioning

      let get conn ~bucket =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let* result =
              do_get conn bucket
                (Internal.query_of_params [ ("versioning", []) ])
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
                        ~name:"VersioningConfiguration"
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
                                     (Fmt.str "invalid versioning status %S"
                                        status))))))

      let put conn ~bucket status =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let xml =
              Xml.versioning_config_to_xmlm
                { status = Some (Status.to_string status) }
              |> Internal.set_xml_name "VersioningConfiguration"
            in
            let body = Ezxmlm.to_string [ xml ] in
            let* result =
              do_put conn bucket
                (Internal.query_of_params [ ("versioning", []) ])
                body
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
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let* result =
              do_get conn bucket (Internal.query_of_params [ ("tagging", []) ])
            in
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
        match
          Result.bind (validate_bucket_only bucket) ~f:(fun () ->
              Internal.Validation.validate_tags tags)
        with
        | Error _ as error -> R.return error
        | Ok () ->
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
              do_put conn bucket
                (Internal.query_of_params [ ("tagging", []) ])
                body
                [
                  ("content-md5", content_md5);
                  ("content-type", "application/xml");
                ]
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp -> ok_or_err resp.status resp.body)

      let delete conn ~bucket =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let* result =
              do_delete conn bucket
                (Internal.query_of_params [ ("tagging", []) ])
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp -> ok_or_err resp.status resp.body)
    end

    module Encryption = struct
      include Bucket.Encryption

      let get conn ~bucket =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let* result =
              do_get conn bucket
                (Internal.query_of_params [ ("encryption", []) ])
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
                                       (Fmt.str
                                          "invalid encryption algorithm %S"
                                          rule.apply.sse_algorithm)))
                        in
                        Result.map (all_results rules) ~f:(fun rules ->
                            { Config.rules })))

      let put conn ~bucket (config : Config.t) =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let xml_rules =
              List.map config.rules ~f:(fun (rule : Rule.t) ->
                  {
                    Xml.apply =
                      {
                        Xml.sse_algorithm =
                          Algorithm.to_string rule.sse_algorithm;
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
              do_put conn bucket
                (Internal.query_of_params [ ("encryption", []) ])
                body
                [
                  ("content-md5", content_md5);
                  ("content-type", "application/xml");
                ]
            in
            R.return
              (match result with
              | Error e -> Error (S3_error.of_aws_error e)
              | Ok resp -> ok_or_err resp.status resp.body)

      let delete conn ~bucket =
        match validate_bucket_only bucket with
        | Error _ as error -> R.return error
        | Ok () ->
            let* result =
              do_delete conn bucket
                (Internal.query_of_params [ ("encryption", []) ])
            in
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
      let metadata = Option.value metadata ~default:[] in
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            let open Internal.Validation in
            let open Internal.Let_syntax in
            let* () = validate_metadata metadata in
            validate_range_header_value "content-type" content_type)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let query_params = [ ("uploads", []) ] in
          let extra_headers =
            ( Internal.Metadata_headers.to_headers metadata |> fun headers ->
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
            call_s3_qp conn ~meth:POST ~bucket ~path ~sign_path:raw_path
              ~query_params ~payload:"" ~extra_headers
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
                  | Ok result -> Ok { Upload.upload_id = result.upload_id; key }
                ))

    let upload_part conn ~bucket ~key ~upload_id ~part_number body =
      Internal.Log.debug (fun m ->
          m "UPLOAD-PART %s/%s #%d (%d bytes)" bucket key part_number
            (String.length body));
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            let open Internal.Validation in
            let open Internal.Let_syntax in
            let* () = validate_upload_id upload_id in
            validate_part_number part_number)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let query_params =
            [
              ("partNumber", [ Int.to_string part_number ]);
              ("uploadId", [ upload_id ]);
            ]
          in
          let extra_headers =
            [ ("content-length", Int.to_string (String.length body)) ]
          in
          let* result =
            call_s3_qp conn ~meth:PUT ~bucket ~path ~sign_path:raw_path
              ~query_params ~payload:body ~extra_headers
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
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            let open Internal.Validation in
            let open Internal.Let_syntax in
            let* () = validate_upload_id upload_id in
            validate_completed_parts parts)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let xml_parts =
            List.map parts ~f:(fun (part : Part.t) ->
                { Xml.part_number = part.part_number; etag = part.etag })
          in
          let xml =
            Xml.complete_request_to_xmlm { parts = xml_parts }
            |> Internal.set_xml_name "CompleteMultipartUpload"
          in
          let xml_body = Ezxmlm.to_string [ xml ] in
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let query_params = [ ("uploadId", [ upload_id ]) ] in
          let* result =
            call_s3_qp conn ~meth:POST ~bucket ~path ~sign_path:raw_path
              ~query_params ~payload:xml_body
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
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            Internal.Validation.validate_upload_id upload_id)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let query_params = [ ("uploadId", [ upload_id ]) ] in
          let* result =
            call_s3_qp conn ~meth:DELETE ~bucket ~path ~sign_path:raw_path
              ~query_params ~payload:"" ~extra_headers:[]
          in
          R.return
            (match result with
            | Error e -> Error (S3_error.of_aws_error e)
            | Ok resp ->
                if Awskit.Response.is_success resp then Ok ()
                else Error (S3_error.of_status resp.status resp.body))

    let list_parts conn ~bucket ~key ~upload_id =
      Internal.Log.debug (fun m -> m "LIST-PARTS %s/%s" bucket key);
      match
        Result.bind (validate_bucket_key bucket key) ~f:(fun () ->
            Internal.Validation.validate_upload_id upload_id)
      with
      | Error _ as error -> R.return error
      | Ok () ->
          let raw_path = Internal.raw_object_key_path key in
          let path = Internal.encode_object_key_path key in
          let query_params = [ ("uploadId", [ upload_id ]) ] in
          let* result =
            call_s3_qp conn ~meth:GET ~bucket ~path ~sign_path:raw_path
              ~query_params ~payload:"" ~extra_headers:[]
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
        ~credentials:(R.credentials conn) ~now:(R.clock conn)
        ~endpoint:(endpoint conn) ~bucket ~key ?expires_in ()

    let put_object conn ~bucket ~key ?expires_in ?content_type () =
      Presigned.put_object ~region:(R.region conn)
        ~credentials:(R.credentials conn) ~now:(R.clock conn)
        ~endpoint:(endpoint conn) ~bucket ~key ?expires_in ?content_type ()
  end
end
