open Awskit_s3

let count_from_env ~var ~default =
  match Sys.getenv_opt var with
  | None | Some "" -> default
  | Some value -> (
      match int_of_string_opt value with
      | Some count when count > 0 -> count
      | _ -> default)

let broad_count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:500
let boundary_count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:300
let default_count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:200
let tagging_field_count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:250
let oversized_tag_count = count_from_env ~var:"AWSKIT_QCHECK_COUNT" ~default:100

let split_ampersand value =
  if String.equal value "" then [] else String.split_on_char '&' value

let expanded_count params =
  List.fold_left
    (fun count (_key, values) -> count + max 1 (List.length values))
    0 params

let test_region = Awskit.Region.of_string_exn "us-east-1"

let print_query params =
  params
  |> List.map (fun (key, values) ->
      Fmt.str "%s=[%s]" key (String.concat "," values))
  |> String.concat ";"

let print_bucket_key (bucket, key) = Fmt.str "%s/%s" bucket key

let object_path key =
  "/" ^ Protocol_wire_model.uri_encode ~encode_slash:false key

let path_style_object_path ~bucket ~key = "/" ^ bucket ^ object_path key

let query_values name uri =
  Uri.query uri |> List.assoc_opt name |> Option.value ~default:[]

let is_sigv4_query_param name =
  match String.lowercase_ascii name with
  | "x-amz-algorithm" | "x-amz-credential" | "x-amz-date" | "x-amz-expires"
  | "x-amz-signedheaders" | "x-amz-signature" | "x-amz-security-token" ->
      true
  | _ -> false

let has_no_sigv4_query_params uri =
  Uri.query uri
  |> List.for_all (fun (name, _) -> not (is_sigv4_query_param name))

let range_of_generated = function
  | `Bytes (start, finish) -> Range.bytes_exn ~start ~finish
  | `From start -> Range.from_exn start
  | `Suffix length -> Range.suffix_exn length

let generated_range_header range = Range.to_header (range_of_generated range)

let prop_canonical_query_params_sorted =
  QCheck.Test.make ~count:broad_count
    ~name:"canonical query params sort encoded pairs"
    (QCheck.make ~print:print_query Protocol_generators.query_params)
    (fun params ->
      let canonical = Awskit.Signing.canonical_query_params params in
      let pairs = split_ampersand canonical in
      List.length pairs = expanded_count params
      && String.equal canonical (Protocol_wire_model.canonical_query params))

let prop_canonical_query_duplicate_keys =
  QCheck.Test.make ~count:boundary_count
    ~name:"canonical query params preserve duplicate keys"
    (QCheck.make ~print:print_query Protocol_generators.duplicate_query_params)
    (fun params ->
      let canonical = Awskit.Signing.canonical_query_params params in
      let pairs = split_ampersand canonical in
      List.length pairs = expanded_count params
      && String.equal canonical (Protocol_wire_model.canonical_query params))

let content_range_equal (left : Range.Content_range.t)
    (right : Range.Content_range.t) =
  Int64.equal left.start right.start
  && Int64.equal left.finish right.finish
  && Option.equal Int64.equal left.complete_length right.complete_length

let prop_content_range_valid_round_trips =
  QCheck.Test.make ~count:broad_count
    ~name:"Content-Range valid values round-trip"
    (QCheck.make ~print:Protocol_wire_model.content_range_header
       Protocol_generators.valid_content_range) (fun generated ->
      let header = Protocol_wire_model.content_range_header generated in
      match Range.Content_range.of_header header with
      | Error _ -> false
      | Ok value -> (
          match
            Range.Content_range.to_header value |> Range.Content_range.of_header
          with
          | Error _ -> false
          | Ok reparsed ->
              Protocol_wire_model.content_range_matches generated value
              && content_range_equal value reparsed))

let prop_content_range_invalid_headers_decode_error =
  QCheck.Test.make ~count:boundary_count
    ~name:"Content-Range invalid boundary families are decode errors"
    (QCheck.make ~print:Fun.id Protocol_generators.invalid_content_range)
    (fun header ->
      match Range.Content_range.of_header header with
      | Error error -> Protocol_support.is_decode_error error
      | Ok _ -> false)

let prop_endpoint_rejects_url_parts =
  QCheck.Test.make ~count:default_count
    ~name:"endpoint parser rejects URL parts and malformed authorities"
    (QCheck.make ~print:String.escaped
       Protocol_generators.malformed_endpoint_authority) (fun value ->
      Result.is_error (Awskit.Endpoint.of_string value))

let prop_endpoint_auto_virtual_hosted_object_paths =
  QCheck.Test.make ~count:default_count
    ~name:"default endpoint auto style uses virtual-hosted object paths"
    (QCheck.make ~print:print_bucket_key
       QCheck.Gen.(
         pair Protocol_generators.valid_bucket_name
           Protocol_generators.protocol_object_key))
    (fun (bucket, key) ->
      let typed_bucket = Bucket_name.of_string_exn bucket in
      let typed_key = Object_key.of_string_exn key in
      match
        Endpoint_resolver.resolve_object_request Endpoint_config.default
          ~region:test_region ~bucket:typed_bucket ~key:typed_key
      with
      | Error _ -> false
      | Ok resolved ->
          String.equal
            (bucket ^ ".s3.us-east-1.amazonaws.com")
            (Awskit.Endpoint.authority resolved.endpoint)
          && String.equal (object_path key) resolved.path
          && String.equal ("/" ^ key) resolved.signing_path
          && resolved.style = `Virtual_hosted
          && Awskit.Region.equal test_region resolved.signing_region)

let prop_endpoint_auto_dotted_bucket_uses_path_style =
  QCheck.Test.make ~count:default_count
    ~name:"default HTTPS endpoint uses path-style for dotted buckets"
    (QCheck.make ~print:print_bucket_key
       QCheck.Gen.(
         pair Protocol_generators.valid_dotted_bucket_name
           Protocol_generators.protocol_object_key))
    (fun (bucket, key) ->
      let typed_bucket = Bucket_name.of_string_exn bucket in
      let typed_key = Object_key.of_string_exn key in
      match
        Endpoint_resolver.resolve_object_request Endpoint_config.default
          ~region:test_region ~bucket:typed_bucket ~key:typed_key
      with
      | Error _ -> false
      | Ok resolved ->
          String.equal "s3.us-east-1.amazonaws.com"
            (Awskit.Endpoint.authority resolved.endpoint)
          && String.equal (path_style_object_path ~bucket ~key) resolved.path
          && String.equal ("/" ^ bucket ^ "/" ^ key) resolved.signing_path
          && resolved.style = `Path
          && Awskit.Region.equal test_region resolved.signing_region)

let prop_endpoint_accelerate_rejects_dotted_buckets =
  QCheck.Test.make ~count:boundary_count
    ~name:"accelerate endpoint rejects dotted buckets"
    (QCheck.make ~print:print_bucket_key
       QCheck.Gen.(
         pair Protocol_generators.valid_dotted_bucket_name
           Protocol_generators.protocol_object_key))
    (fun (bucket, key) ->
      let endpoint_config =
        Endpoint_config.aws ~endpoint_variant:`Accelerate ()
      in
      let typed_bucket = Bucket_name.of_string_exn bucket in
      let typed_key = Object_key.of_string_exn key in
      match
        Endpoint_resolver.resolve_object_request endpoint_config
          ~region:test_region ~bucket:typed_bucket ~key:typed_key
      with
      | Error error -> Awskit.Error.validation_field error = Some "bucket"
      | Ok _ -> false)

let prop_header_values_reject_newline =
  QCheck.Test.make ~count:default_count
    ~name:"request header values reject newline"
    (QCheck.make
       ~print:(fun value -> String.escaped value)
       Protocol_generators.newline_header_value)
    (fun value ->
      Result.is_error (Awskit.Request.validate_headers [ ("x-test", value) ]))

let prop_metadata_rejects_case_insensitive_duplicate_keys =
  let key_gen =
    Protocol_generators.gen_string ~min:1 ~max:12
      ~chars:"abcdefghijklmnopqrstuvwxyz"
  in
  let value_gen =
    Protocol_generators.gen_string ~min:0 ~max:12
      ~chars:"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  in
  QCheck.Test.make ~count:default_count
    ~name:"metadata rejects case-insensitive duplicate keys"
    (QCheck.make
       ~print:(fun (key, first, second) -> Fmt.str "%s=%s/%s" key first second)
       QCheck.Gen.(triple key_gen value_gen value_gen))
    (fun (key, first, second) ->
      Result.is_error
        (Metadata.of_list
           [ (key, first); (String.uppercase_ascii key, second) ]))

let prop_presigned_upload_part_safe_uri_keeps_operation_query =
  QCheck.Test.make ~count:default_count
    ~name:"presigned upload-part safe URI keeps only operation query"
    (QCheck.make
       ~print:(fun (bucket, key, upload_id, part_number) ->
         Fmt.str "%s/%s upload=%s part=%d" bucket key upload_id part_number)
       QCheck.Gen.(
         quad Protocol_generators.valid_bucket_name
           Protocol_generators.protocol_object_key Protocol_generators.upload_id
           (int_range 1 10_000)))
    (fun (bucket, key, upload_id, part_number) ->
      let upload_id = Multipart.Upload_id.of_string_exn upload_id in
      let upload =
        Multipart.Upload.resume
          ~bucket:(Bucket_name.of_string_exn bucket)
          ~key:(Object_key.of_string_exn key)
          ~upload_id
      in
      let part_number = Multipart.Part_number.of_int_exn part_number in
      match
        Presigned.upload_part ~region:"us-east-1"
          ~credentials:Protocol_support.credentials
          ~now:Protocol_support.test_time ~upload ~part_number ()
      with
      | Error _ -> false
      | Ok presigned ->
          let safe_uri = Presigned.safe_uri presigned in
          Presigned.method_ presigned = `PUT
          && has_no_sigv4_query_params safe_uri
          && query_values "partNumber" safe_uri
             = [ Multipart.Part_number.to_int part_number |> string_of_int ]
          && query_values "uploadId" safe_uri
             = [ Multipart.Upload_id.to_string upload_id ]
          && List.mem_assoc "host" (Presigned.signed_headers presigned)
          && not (List.mem_assoc "host" (Presigned.request_headers presigned)))

let prop_presigned_rejects_invalid_extra_signed_headers =
  QCheck.Test.make ~count:boundary_count
    ~name:"presigned requests reject invalid extra signed headers"
    (QCheck.make ~print:String.escaped Protocol_generators.newline_header_value)
    (fun value ->
      let options : Presigned.Get_object.options =
        {
          Presigned.Get_object.default_options with
          extra_signed_headers = [ ("x-extra", value) ];
        }
      in
      Result.is_error
        (Presigned.get_object ~region:"us-east-1"
           ~credentials:Protocol_support.credentials
           ~now:Protocol_support.test_time
           ~bucket:(Protocol_support.bucket_name "bucket")
           ~key:(Protocol_support.object_key "file.txt")
           ~options ()))

let prop_presigned_rejects_invalid_expiration_bounds =
  QCheck.Test.make ~count:boundary_count
    ~name:"presigned requests reject invalid expiration bounds"
    (QCheck.make ~print:string_of_int
       Protocol_generators.invalid_presign_expires_seconds) (fun seconds ->
      let options : Presigned.Get_object.options =
        {
          Presigned.Get_object.default_options with
          expires_in = Some (Ptime.Span.of_int_s seconds);
        }
      in
      match
        Presigned.get_object ~region:"us-east-1"
          ~credentials:Protocol_support.credentials
          ~now:Protocol_support.test_time
          ~bucket:(Protocol_support.bucket_name "bucket")
          ~key:(Protocol_support.object_key "file.txt")
          ~options ()
      with
      | Error error -> Awskit.Error.validation_field error = Some "expires_in"
      | Ok _ -> false)

let tagging_result_from_xml body =
  let conn =
    Protocol_recording_runtime.connect
      [ Protocol_recording_runtime.response 200 body ]
  in
  Protocol_recording_runtime.S3.Bucket.Tagging.get conn
    ~bucket:(Protocol_support.bucket_name "bucket")
    ()

let tag_pairs_to_string tags =
  tags
  |> List.map (fun (key, value) -> Fmt.str "%s=%S" key value)
  |> String.concat ";"

let invalid_tag_field_to_string = function
  | `Key key -> Fmt.str "key=%S" key
  | `Value value -> Fmt.str "value=%S" value

let prop_tagging_xml_rejects_invalid_tag_fields =
  QCheck.Test.make ~count:tagging_field_count
    ~name:"tagging XML rejects invalid tag fields as decode errors"
    (QCheck.make ~print:invalid_tag_field_to_string
       Protocol_generators.invalid_tag_field) (fun invalid ->
      let tags =
        match invalid with
        | `Key key -> [ (key, "value") ]
        | `Value value -> [ ("key", value) ]
      in
      match tagging_result_from_xml (Protocol_wire_model.tagging_xml tags) with
      | Error error -> Protocol_support.is_decode_error error
      | Ok _ -> false)

let prop_tagging_xml_rejects_duplicate_tag_keys =
  QCheck.Test.make ~count:default_count
    ~name:"tagging XML rejects duplicate tag keys as decode errors"
    (QCheck.make
       ~print:(fun (key, first, second) -> Fmt.str "%s=%S/%S" key first second)
       QCheck.Gen.(
         triple Protocol_generators.valid_tag_key
           Protocol_generators.valid_tag_value
           Protocol_generators.valid_tag_value))
    (fun (key, first, second) ->
      match
        tagging_result_from_xml
          (Protocol_wire_model.tagging_xml [ (key, first); (key, second) ])
      with
      | Error error -> Protocol_support.is_decode_error error
      | Ok _ -> false)

let prop_tagging_xml_rejects_oversized_tag_sets =
  QCheck.Test.make ~count:oversized_tag_count
    ~name:"tagging XML rejects tag sets above the S3 limit as decode errors"
    (QCheck.make ~print:tag_pairs_to_string
       Protocol_generators.oversized_tag_set) (fun tags ->
      match tagging_result_from_xml (Protocol_wire_model.tagging_xml tags) with
      | Error error -> Protocol_support.is_decode_error error
      | Ok _ -> false)

let planned_part_count ~content_length ~part_size =
  if content_length = 0 then 0 else ((content_length - 1) / part_size) + 1

let prop_download_ranges_cover_content_length =
  QCheck.Test.make ~count:boundary_count
    ~name:"download ranges exactly cover content length with exact bounds"
    (QCheck.make
       ~print:(fun (content_length, part_size) ->
         Fmt.str "content_length=%d part_size=%d" content_length part_size)
       Protocol_generators.transfer_download_size)
    (fun (content_length, part_size) ->
      match
        Transfer.Plan.download_ranges
          ~content_length:(Int64.of_int content_length)
          ~part_size
      with
      | Error _ -> false
      | Ok ranges ->
          let expected_count = planned_part_count ~content_length ~part_size in
          let total =
            List.fold_left
              (fun acc (range : Transfer.Plan.download_range) ->
                acc + range.length)
              0 ranges
          in
          let rec valid_ranges expected_index expected_offset = function
            | [] -> true
            | (range : Transfer.Plan.download_range) :: rest -> (
                let remaining = content_length - expected_offset in
                let expected_length = min part_size remaining in
                let expected_finish =
                  Int64.of_int (expected_offset + expected_length - 1)
                in
                range.index = expected_index
                && range.length = expected_length
                && range.length > 0
                && range.length <= part_size
                && Int64.equal range.offset (Int64.of_int expected_offset)
                &&
                match Range.view range.range with
                | Range.Bytes (start, finish) ->
                    Int64.equal start range.offset
                    && Int64.equal finish expected_finish
                    && valid_ranges (expected_index + 1)
                         (expected_offset + range.length)
                         rest
                | Range.From _ | Range.Suffix _ -> false)
          in
          List.length ranges = expected_count
          && total = content_length
          && valid_ranges 1 0 ranges)

let prop_upload_parts_cover_content_length =
  QCheck.Test.make ~count:default_count
    ~name:
      "upload parts exactly cover non-empty content length with exact bounds"
    (QCheck.make
       ~print:(fun (content_length, part_size) ->
         Fmt.str "content_length=%d part_size=%d" content_length part_size)
       Protocol_generators.transfer_upload_size)
    (fun (content_length, part_size) ->
      match
        Transfer.Plan.upload_parts
          ~content_length:(Int64.of_int content_length)
          ~part_size
      with
      | Error _ -> false
      | Ok parts ->
          let expected_count = planned_part_count ~content_length ~part_size in
          let total =
            List.fold_left
              (fun acc (part : Transfer.Plan.upload_part) -> acc + part.length)
              0 parts
          in
          let rec valid_parts expected_index expected_offset = function
            | [] -> true
            | (part : Transfer.Plan.upload_part) :: rest ->
                let remaining = content_length - expected_offset in
                let expected_length = min part_size remaining in
                Multipart.Part_number.to_int part.part_number = expected_index
                && part.length = expected_length
                && part.length > 0
                && part.length <= part_size
                && Int64.equal part.offset (Int64.of_int expected_offset)
                && valid_parts (expected_index + 1)
                     (expected_offset + part.length)
                     rest
          in
          List.length parts = expected_count
          && total = content_length
          && valid_parts 1 0 parts)

let prop_get_request_emits_range_header =
  QCheck.Test.make ~count:default_count
    ~name:"GetObject range option emits exact Range header"
    (QCheck.make ~print:generated_range_header Protocol_generators.valid_range)
    (fun generated ->
      let range = range_of_generated generated in
      let options = Object.Get.options_exn ~range () in
      let conn =
        Protocol_recording_runtime.connect
          [
            Protocol_recording_runtime.response
              ~headers:[ ("content-length", "0") ]
              206 "";
          ]
      in
      match
        Protocol_recording_runtime.S3.Object.get conn
          ~bucket:(Protocol_support.bucket_name "bucket")
          ~key:(Protocol_support.object_key "file.txt")
          ~options
          ~consume:
            (Protocol_recording_runtime.S3.Reader.to_string ~max_bytes:1L)
          ()
      with
      | Error _ -> false
      | Ok _ ->
          let call = Protocol_recording_runtime.last_call conn in
          Protocol_support.header "range" call.request.headers
          = Some (Range.to_header range))

let multipart_part_exn number size =
  Multipart.Part.create_exn
    ~part_number:(Multipart.Part_number.of_int_exn number)
    ~etag:(Object.Etag.of_string_exn (Fmt.str "\"part-%d\"" number))
    ~size ()

let complete_upload_result parts =
  let upload =
    Multipart.Upload.resume
      ~bucket:(Protocol_support.bucket_name "bucket")
      ~key:(Protocol_support.object_key "large.bin")
      ~upload_id:(Multipart.Upload_id.of_string_exn "upload-1")
  in
  let conn =
    Protocol_recording_runtime.connect
      [
        Protocol_recording_runtime.response 200
          {|<CompleteMultipartUploadResult><ETag>"final"</ETag></CompleteMultipartUploadResult>|};
      ]
  in
  ( conn,
    Protocol_recording_runtime.S3.Multipart.complete_upload conn ~upload ~parts
      () )

let prop_complete_upload_rejects_unsorted_parts_before_request =
  QCheck.Test.make ~count:boundary_count
    ~name:"complete multipart rejects unsorted parts before request"
    (QCheck.make ~print:string_of_int QCheck.Gen.(int_range 2 10_000))
    (fun number ->
      let parts =
        [
          multipart_part_exn number 5_242_880L;
          multipart_part_exn (number - 1) 1L;
        ]
      in
      let conn, result = complete_upload_result parts in
      match result with
      | Error error ->
          Awskit.Error.validation_field error = Some "part_number"
          && conn.Protocol_recording_runtime.Runtime.calls = []
      | Ok _ -> false)

let prop_complete_upload_rejects_small_nonfinal_parts =
  QCheck.Test.make ~count:boundary_count
    ~name:"complete multipart rejects undersized non-final parts"
    (QCheck.make ~print:Int64.to_string
       QCheck.Gen.(map Int64.of_int (int_range 0 5_242_879)))
    (fun first_size ->
      let parts =
        [ multipart_part_exn 1 first_size; multipart_part_exn 2 1L ]
      in
      let conn, result = complete_upload_result parts in
      match result with
      | Error error ->
          Awskit.Error.validation_field error = Some "parts"
          && conn.Protocol_recording_runtime.Runtime.calls = []
      | Ok _ -> false)

let protocol_family_properties =
  [
    (Protocol_generators.Query, prop_canonical_query_params_sorted);
    (Protocol_generators.Duplicate_query, prop_canonical_query_duplicate_keys);
    ( Protocol_generators.Invalid_content_range,
      prop_content_range_invalid_headers_decode_error );
    (Protocol_generators.Endpoint_malformed, prop_endpoint_rejects_url_parts);
    (Protocol_generators.Header_newline, prop_header_values_reject_newline);
    ( Protocol_generators.Invalid_tag_field,
      prop_tagging_xml_rejects_invalid_tag_fields );
    ( Protocol_generators.Oversized_tag_set,
      prop_tagging_xml_rejects_oversized_tag_sets );
  ]

let remaining_protocol_properties =
  [
    prop_content_range_valid_round_trips;
    prop_endpoint_auto_virtual_hosted_object_paths;
    prop_endpoint_auto_dotted_bucket_uses_path_style;
    prop_endpoint_accelerate_rejects_dotted_buckets;
    prop_metadata_rejects_case_insensitive_duplicate_keys;
    prop_presigned_upload_part_safe_uri_keeps_operation_query;
    prop_presigned_rejects_invalid_extra_signed_headers;
    prop_presigned_rejects_invalid_expiration_bounds;
    prop_tagging_xml_rejects_duplicate_tag_keys;
    prop_download_ranges_cover_content_length;
    prop_upload_parts_cover_content_length;
    prop_get_request_emits_range_header;
    prop_complete_upload_rejects_unsorted_parts_before_request;
    prop_complete_upload_rejects_small_nonfinal_parts;
  ]

let required_protocol_family_bins =
  [
    "protocol.family.query";
    "protocol.family.duplicate-query";
    "protocol.family.endpoint-malformed";
    "protocol.family.header-newline";
    "protocol.family.invalid-content-range";
    "protocol.family.invalid-tag-field";
    "protocol.family.oversized-tag-set";
  ]

let test_protocol_family_coverage () =
  let observed =
    protocol_family_properties
    |> List.map (fun (family, _) -> Protocol_generators.family_bin family)
  in
  let has_observed bin = List.exists (String.equal bin) observed in
  match
    List.filter
      (fun bin -> not (has_observed bin))
      required_protocol_family_bins
  with
  | [] -> ()
  | missing ->
      Alcotest.failf
        "S3 protocol generator families missing semantic coverage bins:\n\
         %s\n\n\
         observed:\n\
         %s"
        (String.concat "\n" missing)
        (String.concat "\n" observed)

let suite =
  [
    ( "workload:awskit-s3:protocol-wire",
      Alcotest.test_case "generator family semantic coverage" `Quick
        test_protocol_family_coverage
      :: List.map Protocol_support.to_alcotest
           (List.map snd protocol_family_properties
           @ remaining_protocol_properties) );
  ]

let () = Alcotest.run "awskit-s3-protocol-wire" suite
