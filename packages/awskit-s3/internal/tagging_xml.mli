val xml_tags : Tag.t list -> string
val parse_tags : string -> (Tag.t list, Awskit.Error.t) result
