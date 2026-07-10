(** AWS Signature Version 4 request signing. Pure, no IO. *)

type result
(** Opaque signed request artifact. *)

val signed_header_names : result -> string list
(** Return the canonical signed-header names. *)

val reveal_headers : result -> (string * string) list
(** Deliberately reveal the headers required for the HTTP request.

    The list includes [authorization], signing timestamps and payload hash, and
    may include a session token. Treat values as sensitive and avoid logs. *)

val ptime_to_date_time : Ptime.t -> string * string
(** [(datestamp, amz_date)] in AWS SigV4 format. *)

val uri_encode : ?encode_slash:bool -> string -> string
(** URI-encode per AWS rules. [encode_slash:false] for paths. *)

val canonical_query_params : (string * string list) list -> string
(** Sort parameters by name/value and URI-encode keys and values. Each key may
    appear multiple times with different values. *)

val canonical_headers : (string * string) list -> (string * string) list
(** Return SigV4 canonical headers: lower-case names, normalized whitespace,
    sorted names, and comma-joined duplicate names. This helper does not
    validate header safety or enforce the single-[Host] signing requirement. *)

val canonical_header_names : (string * string) list -> string
(** Render the semicolon-separated signed header name list from canonical
    headers. *)

val canonical_headers_block : (string * string) list -> string
(** Render canonical headers as the newline-terminated SigV4 header block. *)

val sign_request_params :
  credentials:Credentials.t ->
  region:Region.t ->
  service:string ->
  method_:Request.Method.t ->
  path:string ->
  query_params:(string * string list) list ->
  headers:(string * string) list ->
  payload_hash:Body.Payload_hash.t ->
  now:Ptime.t ->
  (result, Error.t) Stdlib.result
(** Sign a request from structured query parameters.

    The returned header list includes the caller's headers plus SigV4 headers.
    Header validation rejects duplicate [Host] headers and unsafe header names
    or values. Use this current structured API instead of building a raw query
    string for signing. *)

val sign_request_params_exn :
  credentials:Credentials.t ->
  region:Region.t ->
  service:string ->
  method_:Request.Method.t ->
  path:string ->
  query_params:(string * string list) list ->
  headers:(string * string) list ->
  payload_hash:Body.Payload_hash.t ->
  now:Ptime.t ->
  result
(** Like {!val:sign_request_params}, but raises [Error.Awskit_error] carrying
    the structured error on signing or validation failure. *)
