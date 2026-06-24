(** Low-level S3 header construction and validation helpers.

    Most applications should use {!module:Awskit_s3.Object},
    {!module:Awskit_s3.Bucket}, {!module:Awskit_s3.Multipart}, or
    {!module:Awskit_s3.Presigned}. This internal helper is used by awskit's
    built-in request builders to apply the same header encoding rules
    consistently. *)

val add_opt_header :
  string -> string option -> (string * string) list -> (string * string) list
(** Add a header when the optional value is [Some]. *)

val add_opt_account_id_header :
  string ->
  Account_id.t option ->
  (string * string) list ->
  (string * string) list
(** Add a header from an optional account id. *)

val add_opt_content_type_header :
  string ->
  Content_type.t option ->
  (string * string) list ->
  (string * string) list
(** Add a header from an optional content type. *)

val write_precondition_headers :
  Object.Preconditions.Write.t -> (string * string) list
(** Render object write preconditions as request headers. *)

val read_precondition_headers :
  Object.Preconditions.Read.t -> (string * string) list
(** Render object read/head preconditions as request headers. *)

val delete_precondition_headers :
  Object.Preconditions.Delete.t -> (string * string) list
(** Render object delete preconditions as request headers. *)

val copy_source_precondition_headers :
  Object.Preconditions.Copy_source.t -> (string * string) list
(** Render copy-source preconditions as request headers. *)

val validate_common_headers :
  ?content_type:string ->
  ?cache_control:string ->
  ?content_encoding:string ->
  ?content_disposition:string ->
  unit ->
  (unit, Awskit.Error.t) result
(** Validate common HTTP header values before they are sent to S3. *)

val tags_header : Tag.Set.t -> string option
(** Encode tags for the [x-amz-tagging] header. *)

val checksum_header_name : Object.Checksum.Algorithm.t -> string option
(** Return the S3 checksum header name for algorithms that are valid in
    checksum-value headers. *)

val validate_checksum_algorithm :
  Object.Checksum.Algorithm.t -> (unit, Awskit.Error.t) result
(** Validate that a checksum algorithm can be requested from S3. *)

val validate_checksum_type :
  Object.Checksum.Type.t -> (unit, Awskit.Error.t) result
(** Validate that a checksum type can be sent to S3. *)

val validate_checksum_value :
  Object.Checksum.value -> (unit, Awskit.Error.t) result
(** Validate that a checksum value has a supported algorithm/header shape. *)

val validate_storage_class : Storage_class.t -> (unit, Awskit.Error.t) result
(** Validate that a storage class can be sent to S3. *)

val checksum_value_headers :
  Object.Checksum.value option -> (string * string) list
(** Render an optional checksum value as request headers. *)

val checksum_algorithm_header :
  Object.Checksum.Algorithm.t option -> (string * string) list
(** Render an optional checksum algorithm request header. *)

val checksum_type_header :
  Object.Checksum.Type.t option -> (string * string) list
(** Render an optional checksum type request header. *)

val checksum_mode_header :
  Object.Checksum.Mode.t option -> (string * string) list
(** Render an optional checksum mode request header. *)

val multipart_object_size_header : int64 option -> (string * string) list
(** Render the optional multipart object size completion header. *)

val encryption_request_headers :
  Object.Encryption.request option -> (string * string) list
(** Render optional server-side encryption request headers. *)
