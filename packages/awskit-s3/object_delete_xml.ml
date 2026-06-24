module Xml = S3_xml

let ( let* ) = S3_result.( let* )

module Delete_objects = Object.Delete_many

let validate_objects objects =
  let count = List.length objects in
  if count = 0 then
    S3_error_context.invalid ~field:"objects"
      "delete objects request must contain at least one object"
  else if count > Delete_objects.max_objects then
    S3_error_context.invalid ~field:"objects"
      "delete objects request must contain at most %d objects"
      Delete_objects.max_objects
  else Ok ()

let condition_etag_xml_value etag =
  let value = Object.Etag.to_string etag in
  let len = String.length value in
  if len >= 2 && value.[0] = '"' && value.[len - 1] = '"' then
    String.sub value 1 (len - 2)
  else value

let body objects =
  let object_xml (object_ : Delete_objects.object_) =
    let optional name value =
      match value with None -> [] | Some value -> [ Xml.text name value ]
    in
    Xml.el "Object"
      ([ Xml.text "Key" (Object_key.to_string object_.Delete_objects.key) ]
      @ optional "VersionId"
          (Option.map Object.Version_id.to_string object_.version_id)
      @ optional "ETag" (Option.map condition_etag_xml_value object_.etag))
  in
  Xml.el "Delete" (List.map object_xml objects) |> Xml.to_string

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
  let* nodes =
    match Xml.root body with
    | Error _ as error -> error
    | Ok ("Error", _) -> Error (Response.embedded_service_error response body)
    | Ok ("DeleteResult", nodes) -> Ok nodes
    | Ok (actual, _) ->
        Error
          (Xml.decode_with_context ~what:"DeleteResult XML"
             (Fmt.str "expected DeleteResult XML, got %s" actual))
  in
  let* deleted =
    Xml.children_result "Deleted" nodes ~f:(fun index nodes ->
        let path = Fmt.str "DeleteResult.Deleted[%d]" index in
        let* key = parse_key ~path "Key" nodes in
        let* version_id = parse_version_id ~path "VersionId" nodes in
        let* delete_marker =
          Xml.optional_child_parse ~path "DeleteMarker" Response.parse_bool
            nodes
        in
        let* delete_marker_version_id =
          parse_version_id ~path "DeleteMarkerVersionId" nodes
        in
        Ok
          {
            Delete_objects.key;
            version_id;
            delete_marker;
            delete_marker_version_id;
          })
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
