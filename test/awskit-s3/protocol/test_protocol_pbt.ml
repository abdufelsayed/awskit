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
let generator_sample_count = 1_600
let generator_sample_minimum = 40

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

let print_empty_absent_query_case (prefix, key, suffix) =
  Fmt.str "prefix={%s};key=%S;suffix={%s}" (print_query prefix) key
    (print_query suffix)

let print_headers headers =
  headers
  |> List.map (fun (name, value) -> Fmt.str "%S=%S" name value)
  |> String.concat ";"

let print_invalid_header_boundary boundary =
  boundary
  |> Protocol_generators.invalid_header_boundary_headers
  |> print_headers

let print_bucket_key (bucket, key) = Fmt.str "%s/%s" bucket key

let object_path key =
  "/" ^ Protocol_wire_model.uri_encode ~encode_slash:false key

let path_style_object_path ~bucket ~key = "/" ^ bucket ^ object_path key

let presigned_get ?(endpoint_config = Endpoint_config.default) ~bucket ~key () =
  let signer =
    Presigned.Signer.create ~region:test_region
      ~credentials:Protocol_support.credentials ~endpoint_config ()
  in
  Presigned.Signer.get_object signer ~now:Protocol_support.test_time
    ~bucket:(Bucket_name.of_string_exn bucket)
    ~key:(Object_key.of_string_exn key)
    ()

let presigned_safe_uri ?endpoint_config ~bucket ~key () =
  presigned_get ?endpoint_config ~bucket ~key ()
  |> Result.map Presigned.safe_uri

let safe_uri_is ~host ~path uri =
  let expected = Fmt.str "https://%s%s" host path in
  Uri.equal (Uri.of_string expected) uri

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

let prop_canonical_query_duplicate_empty_values =
  QCheck.Test.make ~count:boundary_count
    ~name:"canonical query params preserve duplicate keys with absent values"
    (QCheck.make ~print:print_query
       Protocol_generators.duplicate_empty_query_params) (fun params ->
      let canonical = Awskit.Signing.canonical_query_params params in
      let pairs = split_ampersand canonical in
      let empty_pairs =
        Protocol_wire_model.expand_query params
        |> List.filter (fun (_key, value) -> String.equal value "")
        |> List.length
      in
      List.length pairs = expanded_count params
      && empty_pairs >= 2
      && String.equal canonical (Protocol_wire_model.canonical_query params))

let prop_canonical_query_empty_and_absent_values_match =
  QCheck.Test.make ~count:boundary_count
    ~name:"canonical query params equate empty values and absent value lists"
    (QCheck.make ~print:print_empty_absent_query_case
       Protocol_generators.empty_absent_query_case)
    (fun (prefix, key, suffix) ->
      let absent_query = prefix @ [ (key, []) ] @ suffix in
      let empty_query = prefix @ [ (key, [ "" ]) ] @ suffix in
      let absent_canonical =
        Awskit.Signing.canonical_query_params absent_query
      in
      let empty_canonical = Awskit.Signing.canonical_query_params empty_query in
      String.equal absent_canonical
        (Protocol_wire_model.canonical_query absent_query)
      && String.equal empty_canonical
           (Protocol_wire_model.canonical_query empty_query)
      && String.equal absent_canonical empty_canonical)

let prop_canonical_query_percent_triplets_are_literal_bytes =
  QCheck.Test.make ~count:boundary_count
    ~name:"canonical query params encode percent triplets as literal bytes"
    (QCheck.make ~print:print_query
       Protocol_generators.percent_encoded_query_params) (fun params ->
      let components_match =
        Protocol_wire_model.expand_query params
        |> List.for_all (fun (key, value) ->
            String.equal
              (Awskit.Signing.uri_encode key)
              (Protocol_wire_model.uri_encode key)
            && String.equal
                 (Awskit.Signing.uri_encode value)
                 (Protocol_wire_model.uri_encode value))
      in
      components_match
      && String.equal
           (Awskit.Signing.canonical_query_params params)
           (Protocol_wire_model.canonical_query params))

let prop_canonical_query_sorts_encoded_pairs_not_raw_pairs =
  QCheck.Test.make ~count:boundary_count
    ~name:"canonical query params sort by encoded key and value"
    (QCheck.make ~print:print_query
       Protocol_generators.encoded_sort_query_params) (fun params ->
      Protocol_wire_model.query_sort_changes_after_encoding params
      && String.equal
           (Awskit.Signing.canonical_query_params params)
           (Protocol_wire_model.canonical_query params))

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

let is_safe_canonical_endpoint endpoint =
  Awskit.Endpoint.scheme endpoint = `Https
  && String.equal (Awskit.Endpoint.host endpoint) "s3.us-east-1.amazonaws.com"
  && Option.is_none (Awskit.Endpoint.port endpoint)
  && String.equal
       (Awskit.Endpoint.to_url_prefix endpoint)
       Protocol_mutation.endpoint_seed

let prop_mutated_endpoint_values_are_rejected_or_canonical =
  QCheck.Test.make ~count:boundary_count
    ~name:"mutated endpoint values are rejected or canonical"
    (QCheck.make ~print:String.escaped Protocol_mutation.mutated_endpoint)
    (fun value ->
      match Awskit.Endpoint.of_string value with
      | Error _ -> true
      | Ok endpoint -> is_safe_canonical_endpoint endpoint)

let prop_protocol_object_keys_are_valid =
  QCheck.Test.make ~count:default_count
    ~name:"protocol object-key generator emits valid object keys"
    (QCheck.make ~print:String.escaped Protocol_generators.protocol_object_key)
    (fun key -> Result.is_ok (Object_key.of_string key))

let prop_endpoint_auto_virtual_hosted_object_paths =
  QCheck.Test.make ~count:default_count
    ~name:
      "default endpoint auto style uses virtual-hosted object paths when \
       supported"
    (QCheck.make ~print:print_bucket_key
       QCheck.Gen.(
         pair Protocol_generators.valid_bucket_name
           Protocol_generators.protocol_object_key))
    (fun (bucket, key) ->
      match presigned_safe_uri ~bucket ~key () with
      | Error _ -> false
      | Ok uri ->
          if String.equal key "soap" then
            safe_uri_is ~host:"s3.us-east-1.amazonaws.com"
              ~path:(path_style_object_path ~bucket ~key)
              uri
          else
            safe_uri_is
              ~host:(bucket ^ ".s3.us-east-1.amazonaws.com")
              ~path:(object_path key) uri)

let endpoint_or_fail label config ~region =
  Endpoint_config.endpoint config ~region |> Protocol_support.ok_or_fail label

let test_endpoint_config_rejects_path_style_acceleration () =
  List.iter
    (fun endpoint_variant ->
      match
        Endpoint_config.aws ~addressing_style:`Path ~endpoint_variant ()
      with
      | Error error ->
          Alcotest.(check (option string))
            "validation field" (Some "addressing_style")
            (Awskit.Error.validation_field error)
      | Ok _ -> Alcotest.fail "path-style acceleration must be rejected")
    [ `Accelerate; `Accelerate_dualstack ];
  ignore
    (Endpoint_config.aws ~addressing_style:`Path ~endpoint_variant:`Regional ()
    |> Protocol_support.ok_or_fail "regional path style");
  ignore
    (Endpoint_config.aws ~addressing_style:`Virtual_hosted
       ~endpoint_variant:`Accelerate ()
    |> Protocol_support.ok_or_fail "virtual-hosted acceleration")

let test_endpoint_china_partition_hosts () =
  let cases =
    [
      ( "regional cn-north-1",
        Endpoint_config.aws_exn ~endpoint_variant:`Regional (),
        "cn-north-1",
        "s3.cn-north-1.amazonaws.com.cn" );
      ( "dualstack cn-northwest-1",
        Endpoint_config.aws_exn ~endpoint_variant:`Dualstack (),
        "cn-northwest-1",
        "s3.dualstack.cn-northwest-1.amazonaws.com.cn" );
      ( "accelerate cn-north-1",
        Endpoint_config.aws_exn ~endpoint_variant:`Accelerate (),
        "cn-north-1",
        "s3-accelerate.amazonaws.com.cn" );
      ( "fips dualstack cn-northwest-1",
        Endpoint_config.aws_exn ~endpoint_variant:`Fips_dualstack (),
        "cn-northwest-1",
        "s3-fips.dualstack.cn-northwest-1.amazonaws.com.cn" );
    ]
  in
  List.iter
    (fun (label, config, region, expected_host) ->
      let endpoint =
        endpoint_or_fail label config
          ~region:(Awskit.Region.of_string_exn region)
      in
      Alcotest.(check string)
        label expected_host
        (Awskit.Endpoint.host endpoint))
    cases

let test_endpoint_soap_key_addressing () =
  let auto =
    presigned_safe_uri ~bucket:"bucket" ~key:"soap" ()
    |> Protocol_support.ok_or_fail "auto soap object"
  in
  Alcotest.(check string)
    "auto soap host" "s3.us-east-1.amazonaws.com"
    (Uri.host auto |> Option.value ~default:"");
  Alcotest.(check string) "auto soap path" "/bucket/soap" (Uri.path auto);
  let normal =
    presigned_safe_uri ~bucket:"bucket" ~key:"soapbox" ()
    |> Protocol_support.ok_or_fail "auto soapbox object"
  in
  Alcotest.(check string)
    "auto soapbox host" "bucket.s3.us-east-1.amazonaws.com"
    (Uri.host normal |> Option.value ~default:"");
  let virtual_hosted =
    Endpoint_config.aws_exn ~addressing_style:`Virtual_hosted ()
  in
  match
    presigned_get ~endpoint_config:virtual_hosted ~bucket:"bucket" ~key:"soap"
      ()
  with
  | Error error ->
      Alcotest.(check (option string))
        "virtual-hosted soap validation field" (Some "key")
        (Awskit.Error.validation_field error)
  | Ok request ->
      Alcotest.failf "expected virtual-hosted soap to fail, got safe URI %s"
        (Presigned.safe_uri request |> Uri.to_string)

let prop_endpoint_auto_dotted_bucket_uses_path_style =
  QCheck.Test.make ~count:default_count
    ~name:"default HTTPS endpoint uses path-style for dotted buckets"
    (QCheck.make ~print:print_bucket_key
       QCheck.Gen.(
         pair Protocol_generators.valid_dotted_bucket_name
           Protocol_generators.protocol_object_key))
    (fun (bucket, key) ->
      match presigned_safe_uri ~bucket ~key () with
      | Error _ -> false
      | Ok uri ->
          safe_uri_is ~host:"s3.us-east-1.amazonaws.com"
            ~path:(path_style_object_path ~bucket ~key)
            uri)

let prop_endpoint_paths_preserve_percent_encoded_object_keys =
  QCheck.Test.make ~count:boundary_count
    ~name:"endpoint paths preserve percent-encoded object-key spellings"
    (QCheck.make ~print:Fun.id Protocol_generators.percent_encoded_object_key)
    (fun key ->
      match Object_key.of_string key with
      | Error _ -> true
      | Ok _ -> (
          let path_bucket = "bucket.example" in
          match
            ( presigned_safe_uri ~bucket:"bucket" ~key (),
              presigned_safe_uri ~bucket:path_bucket ~key () )
          with
          | Ok virtual_hosted, Ok path_style ->
              safe_uri_is ~host:"bucket.s3.us-east-1.amazonaws.com"
                ~path:(object_path key) virtual_hosted
              && safe_uri_is ~host:"s3.us-east-1.amazonaws.com"
                   ~path:(path_style_object_path ~bucket:path_bucket ~key)
                   path_style
          | Error _, _ | _, Error _ -> false))

let prop_endpoint_accelerate_rejects_dotted_buckets =
  QCheck.Test.make ~count:boundary_count
    ~name:"accelerate endpoint rejects dotted buckets"
    (QCheck.make ~print:print_bucket_key
       QCheck.Gen.(
         pair Protocol_generators.valid_dotted_bucket_name
           Protocol_generators.protocol_object_key))
    (fun (bucket, key) ->
      let endpoint_config =
        Endpoint_config.aws_exn ~endpoint_variant:`Accelerate ()
      in
      match presigned_get ~endpoint_config ~bucket ~key () with
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

let prop_canonical_headers_normalize_repeated_case_variants =
  QCheck.Test.make ~count:boundary_count
    ~name:"canonical headers normalize repeated case variants and whitespace"
    (QCheck.make ~print:print_headers Protocol_generators.canonical_header_list)
    (fun headers ->
      let expected_headers = Protocol_wire_model.canonical_headers headers in
      let actual_headers = Awskit.Signing.canonical_headers headers in
      actual_headers = expected_headers
      && String.equal
           (Awskit.Signing.canonical_headers_block actual_headers)
           (Protocol_wire_model.canonical_headers_block headers)
      && String.equal
           (Awskit.Signing.canonical_header_names actual_headers)
           (Protocol_wire_model.signed_header_names headers))

let prop_request_headers_reject_newline_and_name_controls =
  QCheck.Test.make ~count:boundary_count
    ~name:"request headers reject newline and name control boundaries"
    (QCheck.make ~print:print_invalid_header_boundary
       Protocol_generators.invalid_header_boundary) (fun boundary ->
      match
        Protocol_generators.invalid_header_boundary_headers boundary
        |> Awskit.Request.validate_headers
      with
      | Error error -> Awskit.Error.validation_field error = Some "header"
      | Ok () -> false)

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
      let signer =
        Presigned.Signer.create
          ~region:(Awskit.Region.of_string_exn "us-east-1")
          ~credentials:Protocol_support.credentials ()
      in
      match
        Presigned.Signer.upload_part signer ~now:Protocol_support.test_time
          ~upload ~part_number ()
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
          && List.mem "host" (Presigned.signed_header_names presigned)
          && not (List.mem "host" (Presigned.request_header_names presigned)))

let prop_presigned_rejects_invalid_additional_headers =
  QCheck.Test.make ~count:boundary_count
    ~name:"presigned requests reject invalid extra signed headers"
    (QCheck.make ~print:String.escaped Protocol_generators.newline_header_value)
    (fun value ->
      Result.is_error
        (Presigned.Additional_headers.of_list [ ("x-extra", value) ]))

let prop_presigned_rejects_invalid_expiration_bounds =
  QCheck.Test.make ~count:boundary_count
    ~name:"presigned requests reject invalid expiration bounds"
    (QCheck.make ~print:string_of_int
       Protocol_generators.invalid_presign_expires_seconds) (fun seconds ->
      match Presigned.Lifetime.of_span (Ptime.Span.of_int_s seconds) with
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

let is_decode_or_validation_error error =
  match Awskit.Error.kind error with
  | Decode _ -> true
  | Validation _ -> true
  | _ -> Awskit.Error.is_validation error

let prop_mutated_tagging_xml_decodes_or_fails_safely =
  QCheck.Test.make ~count:boundary_count
    ~name:"mutated tagging XML decodes or fails safely"
    (QCheck.make ~print:String.escaped Protocol_mutation.mutated_tagging_xml)
    (fun body ->
      match tagging_result_from_xml body with
      | Ok result -> List.length (Tag.Set.to_list result.tags) <= 10
      | Error error -> is_decode_or_validation_error error)

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
      let options = Object.Get.options ~range () in
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

let multipart_checksum_part_exn number =
  let checksum =
    Object.Checksum.value_exn ~algorithm:Object.Checksum.Algorithm.Sha256
      ~value:(Base64.encode_exn (String.make 32 (Char.chr (number mod 256))))
  in
  Multipart.Part.create_exn
    ~part_number:(Multipart.Part_number.of_int_exn number)
    ~etag:(Object.Etag.of_string_exn (Fmt.str "\"part-%d\"" number))
    ~checksum ()

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
      match Multipart.Complete.Parts.of_list parts with
      | Error error -> Awskit.Error.validation_field error = Some "part_number"
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
      match Multipart.Complete.Parts.of_list parts with
      | Error error -> Awskit.Error.validation_field error = Some "parts"
      | Ok _ -> false)

let prop_complete_upload_rejects_checksum_parts_not_starting_at_one =
  QCheck.Test.make ~count:boundary_count
    ~name:
      "complete multipart rejects checksumed parts not starting at one before \
       request"
    (QCheck.make ~print:string_of_int QCheck.Gen.(int_range 2 10_000))
    (fun number ->
      match
        Multipart.Complete.Parts.of_list [ multipart_checksum_part_exn number ]
      with
      | Error error -> Awskit.Error.validation_field error = Some "part_number"
      | Ok _ -> false)

let prop_complete_upload_rejects_checksum_part_gaps =
  QCheck.Test.make ~count:boundary_count
    ~name:"complete multipart rejects checksumed part gaps before request"
    (QCheck.make ~print:string_of_int QCheck.Gen.(int_range 3 10_000))
    (fun number ->
      match
        Multipart.Complete.Parts.of_list
          [ multipart_checksum_part_exn 1; multipart_checksum_part_exn number ]
      with
      | Error error -> Awskit.Error.validation_field error = Some "part_number"
      | Ok _ -> false)

let expect_validation_without_request label conn result =
  (match result with
  | Error error ->
      Alcotest.(check bool)
        (label ^ " is validation") true
        (Awskit.Error.is_validation error)
  | Ok _ -> Alcotest.failf "%s unexpectedly succeeded" label);
  Alcotest.(check int)
    (label ^ " request count") 0
    (List.length (Protocol_recording_runtime.calls conn))

let test_negative_max_bytes_rejected_before_request () =
  let module S3 = Protocol_recording_runtime.S3 in
  let bucket = Protocol_support.bucket_name "bucket" in
  let key = Protocol_support.object_key "file.txt" in
  let check label call =
    let conn = Protocol_recording_runtime.connect [] in
    expect_validation_without_request label conn (call conn)
  in
  check "get_string" (fun conn ->
      S3.Object.get_string conn ~bucket ~key ~max_bytes:(-1L) ());
  check "get_bytes" (fun conn ->
      S3.Object.get_bytes conn ~bucket ~key ~max_bytes:(-1L) ());
  check "find_string" (fun conn ->
      S3.Object.find_string conn ~bucket ~key ~max_bytes:(-1L) ());
  check "find_bytes" (fun conn ->
      S3.Object.find_bytes conn ~bucket ~key ~max_bytes:(-1L) ())

let test_invalid_max_pages_rejected_before_request () =
  let module S3 = Protocol_recording_runtime.S3 in
  let bucket = Protocol_support.bucket_name "bucket" in
  let key = Protocol_support.object_key "file.txt" in
  let upload =
    Multipart.Upload.resume ~bucket ~key
      ~upload_id:(Multipart.Upload_id.of_string_exn "upload-1")
  in
  let check label call =
    let conn = Protocol_recording_runtime.connect [] in
    expect_validation_without_request label conn (call conn)
  in
  check "list fold_pages" (fun conn ->
      S3.Object.List.fold_pages conn ~bucket ~max_pages:0 ~init:()
        ~f:(fun () _page -> Ok ())
        ());
  check "list pages" (fun conn ->
      S3.Object.List.pages conn ~bucket ~max_pages:0 ());
  check "list objects" (fun conn ->
      S3.Object.List.objects conn ~bucket ~max_pages:(-1) ());
  check "list keys" (fun conn ->
      S3.Object.List.keys conn ~bucket ~max_pages:0 ());
  check "versions fold_pages" (fun conn ->
      S3.Object.Versions.fold_pages conn ~bucket ~max_pages:0 ~init:()
        ~f:(fun () _page -> Ok ())
        ());
  check "versions pages" (fun conn ->
      S3.Object.Versions.pages conn ~bucket ~max_pages:0 ());
  check "versions object_versions" (fun conn ->
      S3.Object.Versions.object_versions conn ~bucket ~max_pages:(-1) ());
  check "versions delete_markers" (fun conn ->
      S3.Object.Versions.delete_markers conn ~bucket ~max_pages:0 ());
  check "list-parts fold_pages" (fun conn ->
      S3.Multipart.List_parts.fold_pages conn ~upload ~max_pages:0 ~init:()
        ~f:(fun () _page -> Ok ())
        ());
  check "list-parts pages" (fun conn ->
      S3.Multipart.List_parts.pages conn ~upload ~max_pages:0 ());
  check "list-parts parts" (fun conn ->
      S3.Multipart.List_parts.parts conn ~upload ~max_pages:(-1) ())

type protocol_generator_sample =
  | Sample_query of (string * string list) list
  | Sample_duplicate_query of (string * string list) list
  | Sample_duplicate_empty_query of (string * string list) list
  | Sample_empty_absent_query of
      (string * string list) list * string * (string * string list) list
  | Sample_percent_encoded_query of (string * string list) list
  | Sample_encoded_sort_query of (string * string list) list
  | Sample_endpoint_malformed of string
  | Sample_endpoint_mutation of string
  | Sample_header_canonical of (string * string) list
  | Sample_header_invalid_boundary of
      Protocol_generators.invalid_header_boundary
  | Sample_header_newline of string
  | Sample_invalid_content_range of string
  | Sample_invalid_tag_field of [ `Key of string | `Value of string ]
  | Sample_object_key of string
  | Sample_oversized_tag_set of (string * string) list
  | Sample_percent_encoded_object_key of string
  | Sample_tagging_xml_mutation of string

let protocol_generator_sample_gen =
  let open QCheck.Gen in
  oneof
    [
      map (fun value -> Sample_query value) Protocol_generators.query_params;
      map
        (fun value -> Sample_duplicate_query value)
        Protocol_generators.duplicate_query_params;
      map
        (fun value -> Sample_duplicate_empty_query value)
        Protocol_generators.duplicate_empty_query_params;
      map
        (fun (prefix, key, suffix) ->
          Sample_empty_absent_query (prefix, key, suffix))
        Protocol_generators.empty_absent_query_case;
      map
        (fun value -> Sample_percent_encoded_query value)
        Protocol_generators.percent_encoded_query_params;
      map
        (fun value -> Sample_encoded_sort_query value)
        Protocol_generators.encoded_sort_query_params;
      map
        (fun value -> Sample_endpoint_malformed value)
        Protocol_generators.malformed_endpoint_authority;
      map
        (fun value -> Sample_endpoint_mutation value)
        Protocol_mutation.mutated_endpoint;
      map
        (fun value -> Sample_header_canonical value)
        Protocol_generators.canonical_header_list;
      map
        (fun value -> Sample_header_invalid_boundary value)
        Protocol_generators.invalid_header_boundary;
      map
        (fun value -> Sample_header_newline value)
        Protocol_generators.newline_header_value;
      map
        (fun value -> Sample_invalid_content_range value)
        Protocol_generators.invalid_content_range;
      map
        (fun value -> Sample_invalid_tag_field value)
        Protocol_generators.invalid_tag_field;
      map
        (fun value -> Sample_object_key value)
        Protocol_generators.protocol_object_key;
      map
        (fun value -> Sample_oversized_tag_set value)
        Protocol_generators.oversized_tag_set;
      map
        (fun value -> Sample_percent_encoded_object_key value)
        Protocol_generators.percent_encoded_object_key;
      map
        (fun value -> Sample_tagging_xml_mutation value)
        Protocol_mutation.mutated_tagging_xml;
    ]

let protocol_generator_sample_family = function
  | Sample_query _ -> Protocol_generators.Query
  | Sample_duplicate_query _ -> Protocol_generators.Duplicate_query
  | Sample_duplicate_empty_query _ -> Protocol_generators.Duplicate_empty_query
  | Sample_empty_absent_query _ -> Protocol_generators.Empty_absent_query
  | Sample_percent_encoded_query _ -> Protocol_generators.Percent_encoded_query
  | Sample_encoded_sort_query _ -> Protocol_generators.Encoded_sort_query
  | Sample_endpoint_malformed _ -> Protocol_generators.Endpoint_malformed
  | Sample_endpoint_mutation _ -> Protocol_generators.Endpoint_mutation
  | Sample_header_canonical _ -> Protocol_generators.Header_canonical
  | Sample_header_invalid_boundary _ ->
      Protocol_generators.Header_invalid_boundary
  | Sample_header_newline _ -> Protocol_generators.Header_newline
  | Sample_invalid_content_range _ -> Protocol_generators.Invalid_content_range
  | Sample_invalid_tag_field _ -> Protocol_generators.Invalid_tag_field
  | Sample_object_key _ -> Protocol_generators.Object_key
  | Sample_oversized_tag_set _ -> Protocol_generators.Oversized_tag_set
  | Sample_percent_encoded_object_key _ ->
      Protocol_generators.Percent_encoded_object_key
  | Sample_tagging_xml_mutation _ -> Protocol_generators.Tagging_xml_mutation

let protocol_generator_sample_bin sample =
  Protocol_generators.family_bin (protocol_generator_sample_family sample)

let protocol_generator_sample_bins sample =
  [ protocol_generator_sample_bin sample ]

let qcheck_seed_from_env () =
  match Sys.getenv_opt "QCHECK_SEED" with
  | Some value -> int_of_string_opt value
  | None -> None

let fresh_sample_seed () =
  Random.self_init ();
  Random.int 1_000_000_000

let protocol_generator_sample_seed () =
  match qcheck_seed_from_env () with
  | Some seed -> seed
  | None -> fresh_sample_seed ()

let protocol_generator_samples ~seed =
  let rand = Random.State.make [| seed |] in
  QCheck.Gen.generate ~rand ~n:generator_sample_count
    protocol_generator_sample_gen

let stable_protocol_generator_sample_bins =
  [
    "protocol.family.query";
    "protocol.family.duplicate-query";
    "protocol.family.duplicate-empty-query";
    "protocol.family.empty-absent-query";
    "protocol.family.percent-encoded-query";
    "protocol.family.encoded-sort-query";
    "protocol.family.endpoint-malformed";
    "protocol.family.endpoint-mutation";
    "protocol.family.header-canonical";
    "protocol.family.header-invalid-boundary";
    "protocol.family.header-newline";
    "protocol.family.invalid-content-range";
    "protocol.family.invalid-tag-field";
    "protocol.family.object-key";
    "protocol.family.oversized-tag-set";
    "protocol.family.percent-encoded-object-key";
    "protocol.family.tagging-xml-mutation";
  ]

let stable_protocol_generator_sample_thresholds =
  List.map
    (fun bin ->
      Workload_coverage.threshold ~bin ~minimum:generator_sample_minimum)
    stable_protocol_generator_sample_bins

let protocol_generator_sample_coverage samples =
  let coverage =
    Workload_coverage.of_lists samples ~bins:protocol_generator_sample_bins
  in
  Workload_coverage.add_many
    (Workload_coverage.adjacent_pairs ~bin:protocol_generator_sample_bin samples)
    coverage

let test_protocol_generator_sample_observability () =
  (* This is aggregate sample observability for stable families only. It is not
     reduced replay readiness; most protocol families still have broad printers
     rather than family-specific shrinkers. *)
  let seed = protocol_generator_sample_seed () in
  let samples = protocol_generator_samples ~seed in
  let coverage = protocol_generator_sample_coverage samples in
  let missing =
    Workload_coverage.missing stable_protocol_generator_sample_bins coverage
  in
  let weak =
    Workload_coverage.threshold_failures coverage
      ~thresholds:stable_protocol_generator_sample_thresholds
  in
  match (missing, weak) with
  | [], [] -> ()
  | _ ->
      Alcotest.failf
        "S3 protocol generator sample observability failed across %d samples:\n\
         sample seed: %d\n\
         reproduce: QCHECK_SEED=%d opam exec -- dune build @s3-protocol-laws\n\n\
         missing stable families:\n\
         %s\n\n\
         weak stable families:\n\
         %s\n\n\
         observed sample bins:\n\
         %s"
        (List.length samples) seed seed
        (if missing = [] then "(none)" else String.concat "\n" missing)
        (match Workload_coverage.threshold_failure_lines weak with
        | [] -> "(none)"
        | lines -> String.concat "\n" lines)
        (Workload_coverage.pp coverage)

let protocol_family_properties =
  [
    (Protocol_generators.Query, prop_canonical_query_params_sorted);
    (Protocol_generators.Duplicate_query, prop_canonical_query_duplicate_keys);
    ( Protocol_generators.Duplicate_empty_query,
      prop_canonical_query_duplicate_empty_values );
    ( Protocol_generators.Empty_absent_query,
      prop_canonical_query_empty_and_absent_values_match );
    ( Protocol_generators.Percent_encoded_query,
      prop_canonical_query_percent_triplets_are_literal_bytes );
    ( Protocol_generators.Encoded_sort_query,
      prop_canonical_query_sorts_encoded_pairs_not_raw_pairs );
    ( Protocol_generators.Invalid_content_range,
      prop_content_range_invalid_headers_decode_error );
    (Protocol_generators.Endpoint_malformed, prop_endpoint_rejects_url_parts);
    ( Protocol_generators.Endpoint_mutation,
      prop_mutated_endpoint_values_are_rejected_or_canonical );
    ( Protocol_generators.Header_canonical,
      prop_canonical_headers_normalize_repeated_case_variants );
    ( Protocol_generators.Header_invalid_boundary,
      prop_request_headers_reject_newline_and_name_controls );
    (Protocol_generators.Header_newline, prop_header_values_reject_newline);
    ( Protocol_generators.Invalid_tag_field,
      prop_tagging_xml_rejects_invalid_tag_fields );
    (Protocol_generators.Object_key, prop_protocol_object_keys_are_valid);
    ( Protocol_generators.Oversized_tag_set,
      prop_tagging_xml_rejects_oversized_tag_sets );
    ( Protocol_generators.Percent_encoded_object_key,
      prop_endpoint_paths_preserve_percent_encoded_object_keys );
    ( Protocol_generators.Tagging_xml_mutation,
      prop_mutated_tagging_xml_decodes_or_fails_safely );
  ]

let remaining_protocol_properties =
  [
    prop_content_range_valid_round_trips;
    prop_endpoint_auto_virtual_hosted_object_paths;
    prop_endpoint_auto_dotted_bucket_uses_path_style;
    prop_endpoint_accelerate_rejects_dotted_buckets;
    prop_metadata_rejects_case_insensitive_duplicate_keys;
    prop_presigned_upload_part_safe_uri_keeps_operation_query;
    prop_presigned_rejects_invalid_additional_headers;
    prop_presigned_rejects_invalid_expiration_bounds;
    prop_tagging_xml_rejects_duplicate_tag_keys;
    prop_download_ranges_cover_content_length;
    prop_upload_parts_cover_content_length;
    prop_get_request_emits_range_header;
    prop_complete_upload_rejects_unsorted_parts_before_request;
    prop_complete_upload_rejects_small_nonfinal_parts;
    prop_complete_upload_rejects_checksum_parts_not_starting_at_one;
    prop_complete_upload_rejects_checksum_part_gaps;
  ]

let required_protocol_property_registration_bins =
  [
    "protocol.family.query";
    "protocol.family.duplicate-query";
    "protocol.family.duplicate-empty-query";
    "protocol.family.empty-absent-query";
    "protocol.family.percent-encoded-query";
    "protocol.family.encoded-sort-query";
    "protocol.family.endpoint-malformed";
    "protocol.family.endpoint-mutation";
    "protocol.family.header-canonical";
    "protocol.family.header-invalid-boundary";
    "protocol.family.header-newline";
    "protocol.family.invalid-content-range";
    "protocol.family.invalid-tag-field";
    "protocol.family.oversized-tag-set";
    "protocol.family.percent-encoded-object-key";
    "protocol.family.tagging-xml-mutation";
  ]

let test_protocol_property_family_registration () =
  let observed =
    protocol_family_properties
    |> List.map (fun (family, _) -> Protocol_generators.family_bin family)
  in
  let has_observed bin = List.exists (String.equal bin) observed in
  match
    List.filter
      (fun bin -> not (has_observed bin))
      required_protocol_property_registration_bins
  with
  | [] -> ()
  | missing ->
      Alcotest.failf
        "S3 protocol property families missing registered coverage bins:\n\
         %s\n\n\
         observed:\n\
         %s"
        (String.concat "\n" missing)
        (String.concat "\n" observed)

let suite =
  [
    ( "workload:awskit-s3:protocol-wire",
      Alcotest.test_case "generator sample observability (not replay coverage)"
        `Quick test_protocol_generator_sample_observability
      :: Alcotest.test_case "endpoint config rejects path-style acceleration"
           `Quick test_endpoint_config_rejects_path_style_acceleration
      :: Alcotest.test_case "AWS China endpoint partition hosts" `Quick
           test_endpoint_china_partition_hosts
      :: Alcotest.test_case "soap object key addressing" `Quick
           test_endpoint_soap_key_addressing
      :: Alcotest.test_case "property family registration coverage" `Quick
           test_protocol_property_family_registration
      :: Alcotest.test_case "negative max_bytes rejected before request" `Quick
           test_negative_max_bytes_rejected_before_request
      :: Alcotest.test_case "invalid max_pages rejected before request" `Quick
           test_invalid_max_pages_rejected_before_request
      :: List.map Protocol_support.to_alcotest
           (List.map snd protocol_family_properties
           @ remaining_protocol_properties) );
  ]

let () = Alcotest.run "awskit-s3-protocol-wire" suite
