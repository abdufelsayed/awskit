open Base
(** Shared XML wire types for tagging (used by both Object.Tagging and
    Bucket.Tagging). *)

type tag = { key : string; [@key "Key"] value : string [@key "Value"] }
[@@deriving
  of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
  to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

type tag_set = { tags : tag list [@key "Tag"] }
[@@deriving
  of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
  to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

type tagging = { tag_set : tag_set [@key "TagSet"] }
[@@deriving
  of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
  to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

let xml_of_tagging t = tagging_to_xmlm t |> Internal.set_xml_name "Tagging"
