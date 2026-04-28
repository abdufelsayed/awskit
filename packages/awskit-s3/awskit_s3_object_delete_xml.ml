open Awskit_s3_common

open struct
  module Object = Awskit_s3_object
end

let validate_objects objects =
  let rec loop = function
    | [] -> Ok ()
    | (object_ : Object.Delete_many.object_) :: rest ->
        let* () = validate_key object_.key in
        loop rest
  in
  loop objects

let body objects =
  let object_xml (object_ : Object.Delete_many.object_) =
    let optional name value =
      match value with None -> [] | Some value -> [ Xml.text name value ]
    in
    Xml.el "Object"
      ([ Xml.text "Key" object_.Object.Delete_many.key ]
      @ optional "VersionId"
          (Option.map Object.Version_id.to_string object_.version_id)
      @ optional "ETag" (Option.map Object.Etag.to_string object_.etag)
      @ optional "LastModifiedTime"
          (Option.map ptime_to_header object_.last_modified_time)
      @ optional "Size" (Option.map Int64.to_string object_.size))
  in
  Xml.el "Delete" (Xml.text "Quiet" "false" :: List.map object_xml objects)
  |> Xml.to_string

let parse_result ~request body =
  let* nodes = Xml.decode_root body ~name:"DeleteResult" in
  let deleted =
    Xml.children "Deleted" nodes
    |> List.filter_map (fun nodes ->
        match Xml.child_text "Key" nodes with
        | None -> None
        | Some key ->
            Some
              {
                Object.Delete_many.key;
                version_id =
                  Option.bind (Xml.child_text "VersionId" nodes) (fun v ->
                      Result.to_option (Object.Version_id.of_string v));
                delete_marker =
                  Option.bind
                    (Xml.child_text "DeleteMarker" nodes)
                    Awskit_s3_response.parse_bool;
              })
  in
  let errors =
    Xml.children "Error" nodes
    |> List.filter_map (fun nodes ->
        match (Xml.child_text "Key" nodes, Xml.child_text "Code" nodes) with
        | Some key, Some code ->
            Some
              {
                Object.Delete_many.key;
                code;
                message = Xml.child_text "Message" nodes;
              }
        | _ -> None)
  in
  Ok { Object.Delete_many.deleted; errors; request }
