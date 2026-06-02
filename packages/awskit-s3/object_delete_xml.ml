open Common
open Operation_data

open struct
  module Object = Object
end

let validate_objects objects =
  let rec loop = function
    | [] -> Ok ()
    | (object_ : Delete_objects.object_) :: rest ->
        let* () = validate_key object_.key in
        loop rest
  in
  loop objects

let body objects =
  let object_xml (object_ : Delete_objects.object_) =
    let optional name value =
      match value with None -> [] | Some value -> [ Xml.text name value ]
    in
    Xml.el "Object"
      ([ Xml.text "Key" object_.Delete_objects.key ]
      @ optional "VersionId"
          (Option.map Object.Version_id.to_string object_.version_id)
      @ optional "ETag" (Option.map Object.Etag.to_string object_.etag))
  in
  Xml.el "Delete" (Xml.text "Quiet" "false" :: List.map object_xml objects)
  |> Xml.to_string

let parse_result ~response body =
  let* nodes = Xml.decode_root body ~name:"DeleteResult" in
  let deleted =
    Xml.children "Deleted" nodes
    |> List.filter_map (fun nodes ->
        match Xml.child_text "Key" nodes with
        | None -> None
        | Some key ->
            Some
              {
                Delete_objects.key;
                version_id =
                  Option.bind (Xml.child_text "VersionId" nodes) (fun v ->
                      Result.to_option (Object.Version_id.of_string v));
                delete_marker =
                  Option.bind
                    (Xml.child_text "DeleteMarker" nodes)
                    Response.parse_bool;
              })
  in
  let errors =
    Xml.children "Error" nodes
    |> List.filter_map (fun nodes ->
        match (Xml.child_text "Key" nodes, Xml.child_text "Code" nodes) with
        | Some key, Some code ->
            Some
              {
                Delete_objects.key;
                code;
                message = Xml.child_text "Message" nodes;
              }
        | _ -> None)
  in
  Ok { Delete_objects.deleted; errors; response }
