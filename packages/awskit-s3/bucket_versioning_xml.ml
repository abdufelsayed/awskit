open Core

let xml status =
  Xml.el "VersioningConfiguration"
    (match status with
    | None -> []
    | Some status ->
        [ Xml.text "Status" (Bucket.Versioning.Status.to_string status) ])
  |> Bucket_config_xml_support.xml_body

let parse body response =
  let* nodes = Xml.decode_root body ~name:"VersioningConfiguration" in
  let status =
    Option.bind
      (Xml.child_text "Status" nodes)
      Bucket.Versioning.Status.of_string
  in
  Ok { Bucket.Versioning.status; response }
