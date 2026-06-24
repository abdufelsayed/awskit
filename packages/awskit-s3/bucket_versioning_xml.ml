open Common

let xml status =
  Xml.el "VersioningConfiguration"
    (match status with
    | None -> []
    | Some status ->
        [ Xml.text "Status" (Bucket.Versioning.Status.to_string status) ])
  |> Bucket_xml_support.xml_body

let parse body response =
  let* nodes = Xml.decode_root body ~name:"VersioningConfiguration" in
  match Xml.child_text "Status" nodes with
  | None -> Ok { Bucket.Versioning.status = None; response }
  | Some "" ->
      Xml.decode_field_error ~path:"VersioningConfiguration"
        "<Status> has invalid value %S" ""
  | Some value ->
      Ok
        {
          Bucket.Versioning.status =
            Some (Bucket.Versioning.Status.of_string value);
          response;
        }
