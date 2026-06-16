open Common
module List_objects_v2 = Object.List

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
        let* key = Xml.required_child_text ~path "Key" nodes in
        let* size =
          Xml.optional_child_parse ~path "Size" int64_of_string_opt nodes
        in
        let* etag =
          Xml.optional_child_result ~path "ETag" Object.Etag.of_string nodes
        in
        let* last_modified =
          Xml.optional_child_parse ~path "LastModified" ptime_of_string nodes
        in
        let* storage_class =
          Xml.optional_child_parse ~path "StorageClass" Storage_class.of_string
            nodes
        in
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
      int_of_string_opt nodes
  in
  let* is_truncated =
    Xml.optional_child_parse ~path:"ListBucketResult" "IsTruncated"
      Response.parse_bool nodes
  in
  Ok
    {
      List_objects_v2.bucket = Xml.child_text "Name" nodes;
      prefix = Xml.child_text "Prefix" nodes;
      delimiter = Xml.child_text "Delimiter" nodes;
      objects;
      common_prefixes =
        Xml.children "CommonPrefixes" nodes
        |> List.filter_map (Xml.child_text "Prefix");
      key_count;
      is_truncated = Option.value ~default:false is_truncated;
      continuation_token = Xml.child_text "ContinuationToken" nodes;
      next_continuation_token = Xml.child_text "NextContinuationToken" nodes;
      response;
    }
