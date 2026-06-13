(** S3 tagging XML encoder/decoder. *)

val xml_tags : Tag.t list -> string
(** Encode tags as an S3 tagging XML document. *)

val parse_tags : string -> (Tag.t list, Awskit.Error.t) result
(** Decode an S3 tagging XML document. *)
