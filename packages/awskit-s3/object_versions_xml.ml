open Common
module List_object_versions = Object.Versions

let parse_version_id value =
  Result.to_option (Object.Version_id.of_string value)

let optional_marker_text name nodes =
  match Xml.child_text name nodes with
  | None | Some "" -> None
  | Some value -> Some value

let optional_version_marker ~path name nodes =
  match optional_marker_text name nodes with
  | None -> Ok None
  | Some value -> (
      match parse_version_id value with
      | Some marker -> Ok (Some marker)
      | None ->
          Xml.decode_field_error ~path "<%s> has invalid value %S" name value)

let parse_owner nodes =
  match Xml.child "Owner" nodes with
  | None -> None
  | Some nodes -> Xml.child_text "ID" nodes

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
        let* key = Xml.required_child_text ~path "Key" nodes in
        let* version_id =
          Xml.optional_child_parse ~path "VersionId" parse_version_id nodes
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
        let* storage_class =
          Xml.optional_child_parse ~path "StorageClass" Storage_class.of_string
            nodes
        in
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
        let* key = Xml.required_child_text ~path "Key" nodes in
        let* version_id =
          Xml.optional_child_parse ~path "VersionId" parse_version_id nodes
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
  Ok
    {
      List_object_versions.bucket = Xml.child_text "Name" nodes;
      prefix = Xml.child_text "Prefix" nodes;
      delimiter = Xml.child_text "Delimiter" nodes;
      versions;
      delete_markers;
      common_prefixes =
        Xml.children "CommonPrefixes" nodes
        |> List.filter_map (Xml.child_text "Prefix");
      is_truncated = Option.value ~default:false is_truncated;
      key_marker = optional_marker_text "KeyMarker" nodes;
      version_id_marker;
      next_key_marker = optional_marker_text "NextKeyMarker" nodes;
      next_version_id_marker;
      response;
    }
