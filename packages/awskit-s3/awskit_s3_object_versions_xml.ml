open Awskit_s3_common

open struct
  module Object = Awskit_s3_object
end

let parse_version_id value =
  Result.to_option (Object.Version_id.of_string value)

let parse_owner nodes =
  match Xml.child "Owner" nodes with
  | None -> None
  | Some nodes -> Xml.child_text "ID" nodes

let parse_page ~request body =
  let* nodes = Xml.decode_root body ~name:"ListVersionsResult" in
  let versions =
    Xml.children "Version" nodes
    |> List.filter_map (fun nodes ->
        match Xml.child_text "Key" nodes with
        | None -> None
        | Some key ->
            Some
              {
                Object.Versions.key;
                version_id =
                  Option.bind
                    (Xml.child_text "VersionId" nodes)
                    parse_version_id;
                is_latest =
                  Option.bind
                    (Xml.child_text "IsLatest" nodes)
                    Awskit_s3_response.parse_bool;
                last_modified =
                  Option.bind
                    (Xml.child_text "LastModified" nodes)
                    ptime_of_string;
                etag =
                  Option.bind (Xml.child_text "ETag" nodes) (fun v ->
                      Result.to_option (Object.Etag.of_string v));
                size =
                  Option.bind (Xml.child_text "Size" nodes) int64_of_string_opt;
                storage_class =
                  Option.bind
                    (Xml.child_text "StorageClass" nodes)
                    Storage_class.of_string;
                owner = parse_owner nodes;
              })
  in
  let delete_markers =
    Xml.children "DeleteMarker" nodes
    |> List.filter_map (fun nodes ->
        match Xml.child_text "Key" nodes with
        | None -> None
        | Some key ->
            Some
              {
                Object.Versions.key;
                version_id =
                  Option.bind
                    (Xml.child_text "VersionId" nodes)
                    parse_version_id;
                is_latest =
                  Option.bind
                    (Xml.child_text "IsLatest" nodes)
                    Awskit_s3_response.parse_bool;
                last_modified =
                  Option.bind
                    (Xml.child_text "LastModified" nodes)
                    ptime_of_string;
                owner = parse_owner nodes;
              })
  in
  Ok
    {
      Object.Versions.bucket = Xml.child_text "Name" nodes;
      prefix = Xml.child_text "Prefix" nodes;
      delimiter = Xml.child_text "Delimiter" nodes;
      versions;
      delete_markers;
      common_prefixes =
        Xml.children "CommonPrefixes" nodes
        |> List.filter_map (Xml.child_text "Prefix");
      is_truncated =
        Option.value ~default:false
          (Option.bind
             (Xml.child_text "IsTruncated" nodes)
             Awskit_s3_response.parse_bool);
      key_marker = Xml.child_text "KeyMarker" nodes;
      version_id_marker =
        Option.bind (Xml.child_text "VersionIdMarker" nodes) parse_version_id;
      next_key_marker = Xml.child_text "NextKeyMarker" nodes;
      next_version_id_marker =
        Option.bind
          (Xml.child_text "NextVersionIdMarker" nodes)
          parse_version_id;
      request;
    }
