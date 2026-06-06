val add_opt_header :
  string -> string option -> (string * string) list -> (string * string) list

val write_precondition_headers :
  Object.Preconditions.Write.t -> (string * string) list

val read_precondition_headers :
  Object.Preconditions.Read.t -> (string * string) list

val delete_precondition_headers :
  Object.Preconditions.Delete.t -> (string * string) list

val copy_source_precondition_headers :
  Object.Preconditions.Copy_source.t -> (string * string) list

val validate_common_headers :
  ?content_type:string ->
  ?cache_control:string ->
  ?content_encoding:string ->
  ?content_disposition:string ->
  unit ->
  (unit, Awskit.Error.t) result

val tags_header : Tag.t list -> string option
val checksum_header_name : Object.Checksum.Algorithm.t -> string option

val validate_checksum_algorithm :
  Object.Checksum.Algorithm.t -> (unit, Awskit.Error.t) result

val validate_checksum_type :
  Object.Checksum.Type.t -> (unit, Awskit.Error.t) result

val validate_checksum_value :
  Object.Checksum.value -> (unit, Awskit.Error.t) result

val checksum_value_headers :
  Object.Checksum.value option -> (string * string) list

val checksum_algorithm_header :
  Object.Checksum.Algorithm.t option -> (string * string) list

val checksum_type_header :
  Object.Checksum.Type.t option -> (string * string) list

val checksum_mode_header :
  Object.Checksum.Mode.t option -> (string * string) list

val multipart_object_size_header : int64 option -> (string * string) list

val encryption_request_headers :
  Object.Encryption.request option -> (string * string) list
