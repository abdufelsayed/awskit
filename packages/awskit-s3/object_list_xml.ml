open Common
open Operation_data

open struct
  module Object = Object
end

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
  let objects =
    Xml.children "Contents" nodes
    |> List.filter_map (fun nodes ->
        match Xml.child_text "Key" nodes with
        | None -> None
        | Some key ->
            Some
              {
                List_objects_v2.key;
                size =
                  Option.bind (Xml.child_text "Size" nodes) int64_of_string_opt;
                etag =
                  Option.bind (Xml.child_text "ETag" nodes) (fun v ->
                      Result.to_option (Object.Etag.of_string v));
                last_modified =
                  Option.bind
                    (Xml.child_text "LastModified" nodes)
                    ptime_of_string;
                storage_class =
                  Option.bind
                    (Xml.child_text "StorageClass" nodes)
                    Storage_class.of_string;
                checksum = parse_checksum_summary nodes;
              })
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
      key_count =
        Option.bind (Xml.child_text "KeyCount" nodes) int_of_string_opt;
      is_truncated =
        Option.value ~default:false
          (Option.bind (Xml.child_text "IsTruncated" nodes) Response.parse_bool);
      continuation_token = Xml.child_text "ContinuationToken" nodes;
      next_continuation_token = Xml.child_text "NextContinuationToken" nodes;
      response;
    }
