open Awskit_s3_common

let xml_tags tags =
  Xml.el "Tagging"
    [
      Xml.el "TagSet"
        (List.map
           (fun (tag : Tag.t) ->
             Xml.el "Tag" [ Xml.text "Key" tag.key; Xml.text "Value" tag.value ])
           tags);
    ]
  |> Xml.to_string

let parse_tags body =
  let* nodes = Xml.decode_root body ~name:"Tagging" in
  let tag_set = Option.value ~default:[] (Xml.child "TagSet" nodes) in
  let tags =
    Xml.children "Tag" tag_set
    |> List.filter_map (fun nodes ->
        match (Xml.child_text "Key" nodes, Xml.child_text "Value" nodes) with
        | Some key, Some value -> Some { Tag.key; value }
        | _ -> None)
  in
  Ok tags
