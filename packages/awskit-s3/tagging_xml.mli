(** S3 tagging XML encoder/decoder. *)

val xml_tags : Tag.Set.t -> string
(** Encode tags as an S3 tagging XML document. *)

val parse_tags : string -> (Tag.Set.t, Awskit.Error.t) result
(** Decode an S3 tagging XML document. *)
