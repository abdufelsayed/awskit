open Common
module List_object_versions = Object.Versions

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

let optional_marker_text name nodes =
  match Xml.child_text name nodes with
  | None | Some "" -> None
  | Some value -> Some value

let optional_version_marker ~path name nodes =
  match optional_marker_text name nodes with
  | None -> Ok None
  | Some value ->
      Result.map Option.some
        (parse_result ~path name Object.Version_id.of_string value)

let optional_key_marker ~path name nodes =
  match optional_marker_text name nodes with
  | None -> Ok None
  | Some value ->
      Result.map Option.some
        (parse_result ~path name Object_key.of_string value)

let optional_storage_class ~path nodes =
  match Xml.child_text "StorageClass" nodes with
  | None -> Ok None
  | Some "" ->
      Xml.decode_field_error ~path "<StorageClass> has invalid value %S" ""
  | Some value -> Ok (Some (Storage_class.of_string value))

let parse_owner nodes =
  match Xml.child "Owner" nodes with
  | None -> None
  | Some nodes ->
      Object.Owner.create
        ?id:(Xml.child_text "ID" nodes)
        ?display_name:(Xml.child_text "DisplayName" nodes)
        ()

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
  let* nodes = Xml.decode_root body ~name:"ListVersionsResult" in
  let* versions =
    Xml.children_result "Version" nodes ~f:(fun index nodes ->
        let path = Fmt.str "ListVersionsResult.Version[%d]" index in
        let* key_text = Xml.required_child_text ~path "Key" nodes in
        let* key = parse_result ~path "Key" Object_key.of_string key_text in
        let* version_id =
          optional_text_result ~path "VersionId" Object.Version_id.of_string
            nodes
        in
        let* is_latest =
          Xml.optional_child_parse ~path "IsLatest" Response.parse_bool nodes
        in
        let* last_modified =
          Xml.optional_child_parse ~path "LastModified" ptime_of_string nodes
        in
        let* etag =
          Xml.optional_child_result ~path "ETag" Object.Etag.of_string nodes
        in
        let* size =
          Xml.optional_child_parse ~path "Size" non_negative_int64_of_string_opt
            nodes
        in
        let* storage_class = optional_storage_class ~path nodes in
        Ok
          {
            List_object_versions.key;
            version_id;
            is_latest;
            last_modified;
            etag;
            size;
            storage_class;
            owner = parse_owner nodes;
            checksum = parse_checksum_summary nodes;
          })
  in
  let* delete_markers =
    Xml.children_result "DeleteMarker" nodes ~f:(fun index nodes ->
        let path = Fmt.str "ListVersionsResult.DeleteMarker[%d]" index in
        let* key_text = Xml.required_child_text ~path "Key" nodes in
        let* key = parse_result ~path "Key" Object_key.of_string key_text in
        let* version_id =
          optional_text_result ~path "VersionId" Object.Version_id.of_string
            nodes
        in
        let* is_latest =
          Xml.optional_child_parse ~path "IsLatest" Response.parse_bool nodes
        in
        let* last_modified =
          Xml.optional_child_parse ~path "LastModified" ptime_of_string nodes
        in
        Ok
          {
            List_object_versions.key;
            version_id;
            is_latest;
            last_modified;
            owner = parse_owner nodes;
          })
  in
  let* is_truncated =
    Xml.optional_child_parse ~path:"ListVersionsResult" "IsTruncated"
      Response.parse_bool nodes
  in
  let* version_id_marker =
    optional_version_marker ~path:"ListVersionsResult" "VersionIdMarker" nodes
  in
  let* next_version_id_marker =
    optional_version_marker ~path:"ListVersionsResult" "NextVersionIdMarker"
      nodes
  in
  let* bucket =
    optional_text_result ~path:"ListVersionsResult" "Name" Bucket_name.of_string
      nodes
  in
  let* prefix =
    optional_non_empty_text_result ~path:"ListVersionsResult" "Prefix"
      Object_key.Prefix.of_string nodes
  in
  let* delimiter =
    optional_text_result ~path:"ListVersionsResult" "Delimiter"
      List_object_versions.Delimiter.of_string nodes
  in
  let* common_prefixes =
    Xml.children_result "CommonPrefixes" nodes ~f:(fun index nodes ->
        let path = Fmt.str "ListVersionsResult.CommonPrefixes[%d]" index in
        let* prefix = Xml.required_child_text ~path "Prefix" nodes in
        parse_result ~path "Prefix" Object_key.Prefix.of_string prefix)
  in
  let* key_marker =
    optional_key_marker ~path:"ListVersionsResult" "KeyMarker" nodes
  in
  let* next_key_marker =
    optional_key_marker ~path:"ListVersionsResult" "NextKeyMarker" nodes
  in
  Ok
    {
      List_object_versions.bucket;
      prefix;
      delimiter;
      versions;
      delete_markers;
      common_prefixes;
      is_truncated = Option.value ~default:false is_truncated;
      key_marker;
      version_id_marker;
      next_key_marker;
      next_version_id_marker;
      response;
    }
