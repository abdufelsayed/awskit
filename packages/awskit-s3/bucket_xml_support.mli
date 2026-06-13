(** Shared helpers for bucket XML encoders/decoders. *)

val xml_body : Ezxmlm.node -> string
(** Serialize an XML node as a request body. *)

val bool_text : bool -> string
(** Render an S3 XML boolean. *)

val validate_opt_header :
  string -> string option -> (unit, Awskit.Error.t) result
(** Validate an optional XML/header string field. *)

val validate_string_list :
  field:string -> string list -> (unit, Awskit.Error.t) result
(** Validate all string values in a list field. *)

val validate_all :
  (unit, Awskit.Error.t) result list -> (unit, Awskit.Error.t) result
(** Return the first validation error from a list of checks. *)
