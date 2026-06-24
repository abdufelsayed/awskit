(** Internal simulator aliases for shared S3 header construction helpers. *)

val add_opt_account_id_header :
  string ->
  Awskit_s3.Account_id.t option ->
  (string * string) list ->
  (string * string) list

val add_opt_content_type_header :
  string ->
  Awskit_s3.Content_type.t option ->
  (string * string) list ->
  (string * string) list

val write_precondition_headers :
  Awskit_s3.Object.Preconditions.Write.t -> (string * string) list

val read_precondition_headers :
  Awskit_s3.Object.Preconditions.Read.t -> (string * string) list

val delete_precondition_headers :
  Awskit_s3.Object.Preconditions.Delete.t -> (string * string) list

val copy_source_precondition_headers :
  Awskit_s3.Object.Preconditions.Copy_source.t -> (string * string) list

val validate_common_headers :
  ?content_type:string ->
  ?cache_control:string ->
  ?content_encoding:string ->
  ?content_disposition:string ->
  unit ->
  (unit, Awskit.Error.t) result

val tags_header : Awskit_s3.Tag.Set.t -> string option

val checksum_header_name :
  Awskit_s3.Object.Checksum.Algorithm.t -> string option

val validate_checksum_algorithm :
  Awskit_s3.Object.Checksum.Algorithm.t -> (unit, Awskit.Error.t) result

val validate_checksum_type :
  Awskit_s3.Object.Checksum.Type.t -> (unit, Awskit.Error.t) result

val validate_checksum_value :
  Awskit_s3.Object.Checksum.value -> (unit, Awskit.Error.t) result

val validate_storage_class :
  Awskit_s3.Storage_class.t -> (unit, Awskit.Error.t) result

val checksum_value_headers :
  Awskit_s3.Object.Checksum.value option -> (string * string) list

val checksum_algorithm_header :
  Awskit_s3.Object.Checksum.Algorithm.t option -> (string * string) list

val checksum_type_header :
  Awskit_s3.Object.Checksum.Type.t option -> (string * string) list

val checksum_mode_header :
  Awskit_s3.Object.Checksum.Mode.t option -> (string * string) list

val multipart_object_size_header : int64 option -> (string * string) list

val encryption_request_headers :
  Awskit_s3.Object.Encryption.request option -> (string * string) list
