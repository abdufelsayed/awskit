module Xml = S3_xml

let ( let* ) = S3_result.( let* )

module List_objects_v2 = Object.List

let parse_result ~path name parse value =
  match parse value with
  | Ok value -> Ok value
  | Error error ->
      Xml.decode_field_error ~path "<%s> has invalid value %S: %s" name value
        (Awskit.Error.to_string_hum error)

let optional_text_result ~path name parse nodes =
  match Xml.child_text name nodes with
  | None -> Ok None
  | Some value -> Result.map Option.some (parse_result ~path name parse value)

let optional_non_empty_text_result ~path name parse nodes =
  match Xml.child_text name nodes with
  | None | Some "" -> Ok None
  | Some value -> Result.map Option.some (parse_result ~path name parse value)

let optional_storage_class ~path nodes =
  match Xml.child_text "StorageClass" nodes with
  | None -> Ok None
  | Some "" ->
      Xml.decode_field_error ~path "<StorageClass> has invalid value %S" ""
  | Some value ->
      Result.map Option.some
        (parse_result ~path "StorageClass" Storage_class.of_string value)

let parse_checksum_summary nodes =
  {
    Object.Checksum.algorithms =
      Xml.child_texts "ChecksumAlgorithm" nodes
      |> List.map String.trim
      |> List.map Object.Checksum.Algorithm.of_string;
    checksum_type =
      Option.map Object.Checksum.Type.of_string
        (Xml.child_text "ChecksumType" nodes);
  }

let parse_page ~response body =
  let* nodes = Xml.decode_root body ~name:"ListBucketResult" in
  let* objects =
    Xml.children_result "Contents" nodes ~f:(fun index nodes ->
        let path = Fmt.str "ListBucketResult.Contents[%d]" index in
        let* key_text = Xml.required_child_text ~path "Key" nodes in
        let* key = parse_result ~path "Key" Object_key.of_string key_text in
        let* size =
          Xml.optional_child_parse ~path "Size"
            S3_parse.non_negative_int64_of_string_opt nodes
        in
        let* etag =
          Xml.optional_child_result ~path "ETag" Object.Etag.of_string nodes
        in
        let* last_modified =
          Xml.optional_child_parse ~path "LastModified" S3_time.of_string nodes
        in
        let* storage_class = optional_storage_class ~path nodes in
        Ok
          {
            List_objects_v2.key;
            size;
            etag;
            last_modified;
            storage_class;
            checksum = parse_checksum_summary nodes;
          })
  in
  let* key_count =
    Xml.optional_child_parse ~path:"ListBucketResult" "KeyCount"
      S3_parse.non_negative_int_of_string_opt nodes
  in
  let* is_truncated =
    Xml.optional_child_parse ~path:"ListBucketResult" "IsTruncated"
      Response.parse_bool nodes
  in
  let* bucket =
    optional_text_result ~path:"ListBucketResult" "Name" Bucket_name.of_string
      nodes
  in
  let* prefix =
    optional_non_empty_text_result ~path:"ListBucketResult" "Prefix"
      Object_key.Prefix.of_string nodes
  in
  let* delimiter =
    optional_text_result ~path:"ListBucketResult" "Delimiter"
      List_objects_v2.Delimiter.of_string nodes
  in
  let* common_prefixes =
    Xml.children_result "CommonPrefixes" nodes ~f:(fun index nodes ->
        let path = Fmt.str "ListBucketResult.CommonPrefixes[%d]" index in
        let* prefix = Xml.required_child_text ~path "Prefix" nodes in
        parse_result ~path "Prefix" Object_key.Prefix.of_string prefix)
  in
  let* continuation_token =
    optional_text_result ~path:"ListBucketResult" "ContinuationToken"
      List_objects_v2.Continuation_token.of_string nodes
  in
  let* next_continuation_token =
    optional_text_result ~path:"ListBucketResult" "NextContinuationToken"
      List_objects_v2.Continuation_token.of_string nodes
  in
  Ok
    {
      List_objects_v2.bucket;
      prefix;
      delimiter;
      objects;
      common_prefixes;
      key_count;
      is_truncated = Option.value ~default:false is_truncated;
      continuation_token;
      next_continuation_token;
      response;
    }
