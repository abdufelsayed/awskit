val xml_body : Ezxmlm.node -> string
val bool_text : bool -> string

val validate_opt_header :
  string -> string option -> (unit, Awskit.Error.t) result

val validate_string_list :
  field:string -> string list -> (unit, Awskit.Error.t) result

val validate_all :
  (unit, Awskit.Error.t) result list -> (unit, Awskit.Error.t) result
