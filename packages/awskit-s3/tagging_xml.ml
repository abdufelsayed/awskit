open Common

let xml_tags tags =
  Xml.el "Tagging"
    [
      Xml.el "TagSet"
        (List.map
           (fun tag ->
             Xml.el "Tag"
               [
                 Xml.text "Key" (Tag.key tag); Xml.text "Value" (Tag.value tag);
               ])
           (Tag.Set.to_list tags));
    ]
  |> Xml.to_string

let parse_tags body =
  let* nodes = Xml.decode_root body ~name:"Tagging" in
  let tag_set = Option.value ~default:[] (Xml.child "TagSet" nodes) in
  let* tags =
    Xml.children_result "Tag" tag_set ~f:(fun index nodes ->
        let path = Fmt.str "Tagging.TagSet.Tag[%d]" index in
        let* key = Xml.required_child_text ~path "Key" nodes in
        let* value = Xml.required_child_text ~path "Value" nodes in
        match Tag.create ~key ~value with
        | Ok _ as result -> result
        | Error error ->
            Xml.decode_field_error ~path "%s" (Awskit.Error.to_string_hum error))
  in
  match Tag.Set.of_list tags with
  | Ok _ as result -> result
  | Error error ->
      Xml.decode_field_error ~path:"Tagging.TagSet" "%s"
        (Awskit.Error.to_string_hum error)
