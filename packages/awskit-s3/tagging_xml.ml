open Common

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
  Xml.children_result "Tag" tag_set ~f:(fun index nodes ->
      let path = Fmt.str "Tagging.TagSet.Tag[%d]" index in
      let* key = Xml.required_child_text ~path "Key" nodes in
      let* value = Xml.required_child_text ~path "Value" nodes in
      Ok { Tag.key; value })
