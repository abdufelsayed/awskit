open Common
module Delete_objects = Object.Delete_many

let validate_objects objects =
  let count = List.length objects in
  if count = 0 then
    invalid ~field:"objects"
      "delete objects request must contain at least one object"
  else if count > Delete_objects.max_objects then
    invalid ~field:"objects"
      "delete objects request must contain at most %d objects"
      Delete_objects.max_objects
  else Ok ()

let body objects =
  let object_xml (object_ : Delete_objects.object_) =
    let optional name value =
      match value with None -> [] | Some value -> [ Xml.text name value ]
    in
    Xml.el "Object"
      ([ Xml.text "Key" (Object_key.to_string object_.Delete_objects.key) ]
      @ optional "VersionId"
          (Option.map Object.Version_id.to_string object_.version_id)
      @ optional "ETag" (Option.map Object.Etag.to_string object_.etag))
  in
  Xml.el "Delete" (Xml.text "Quiet" "false" :: List.map object_xml objects)
  |> Xml.to_string

let parse_key ~path name nodes =
  let* key = Xml.required_child_text ~path name nodes in
  match Object_key.of_string key with
  | Ok key -> Ok key
  | Error error ->
      Xml.decode_field_error ~path "<%s> has invalid value %S: %s" name key
        (Awskit.Error.to_string_hum error)

let parse_version_id ~path name nodes =
  Xml.optional_child_result ~path name Object.Version_id.of_string nodes

let parse_result ~response body =
  let* nodes = Xml.decode_root body ~name:"DeleteResult" in
  let* deleted =
    Xml.children_result "Deleted" nodes ~f:(fun index nodes ->
        let path = Fmt.str "DeleteResult.Deleted[%d]" index in
        let* key = parse_key ~path "Key" nodes in
        let* version_id = parse_version_id ~path "VersionId" nodes in
        let* delete_marker =
          Xml.optional_child_parse ~path "DeleteMarker" Response.parse_bool
            nodes
        in
        Ok { Delete_objects.key; version_id; delete_marker })
  in
  let* errors =
    Xml.children_result "Error" nodes ~f:(fun index nodes ->
        let path = Fmt.str "DeleteResult.Error[%d]" index in
        let* key = parse_key ~path "Key" nodes in
        let* code = Xml.required_child_text ~path "Code" nodes in
        Ok
          { Delete_objects.key; code; message = Xml.child_text "Message" nodes })
  in
  Ok { Delete_objects.deleted; errors; response }
