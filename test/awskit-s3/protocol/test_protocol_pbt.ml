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

let print_query params =
  params
  |> List.map (fun (key, values) ->
      Fmt.str "%s=[%s]" key (String.concat "," values))
  |> String.concat ";"

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

let suite =
  [
    ( "workload:awskit-s3:protocol-wire",
      List.map Protocol_support.to_alcotest
        [
          prop_canonical_query_params_sorted;
          prop_canonical_query_duplicate_keys;
          prop_content_range_valid_round_trips;
          prop_content_range_invalid_headers_decode_error;
          prop_endpoint_rejects_url_parts;
          prop_header_values_reject_newline;
          prop_metadata_rejects_case_insensitive_duplicate_keys;
          prop_tagging_xml_rejects_invalid_tag_fields;
          prop_tagging_xml_rejects_duplicate_tag_keys;
          prop_tagging_xml_rejects_oversized_tag_sets;
          prop_download_ranges_cover_content_length;
          prop_upload_parts_cover_content_length;
        ] );
  ]

let () = Alcotest.run "awskit-s3-protocol-wire" suite
