(** AWS Signature Version 4 request signing. Pure, no IO. *)

type signed_headers = {
  headers : (string * string) list;
  signed_headers_str : string;
}
(** [headers] includes [authorization], [x-amz-date], [x-amz-content-sha256],
    and [x-amz-security-token] when credentials include a session token. *)

val ptime_to_date_time : Ptime.t -> string * string
(** [(datestamp, amz_date)] in AWS SigV4 format. *)

val uri_encode : ?encode_slash:bool -> string -> string
(** URI-encode per AWS rules. [encode_slash:false] for paths. *)

val canonical_query_params : (string * string list) list -> string
(** Sort parameters by name/value and URI-encode keys and values. Each key may
    appear multiple times with different values. *)

val canonical_query : string -> string
(** Parse, sort, and URI-encode a raw query string. Prefer
    {!val:canonical_query_params} when already holding structured parameters. *)

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
  (signed_headers, Error.t) result

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
  signed_headers

val sign_request :
  credentials:Credentials.t ->
  region:Region.t ->
  service:string ->
  method_:Request.Method.t ->
  path:string ->
  query:string ->
  headers:(string * string) list ->
  payload_hash:Body.Payload_hash.t ->
  now:Ptime.t ->
  (signed_headers, Error.t) result

val sign_request_exn :
  credentials:Credentials.t ->
  region:Region.t ->
  service:string ->
  method_:Request.Method.t ->
  path:string ->
  query:string ->
  headers:(string * string) list ->
  payload_hash:Body.Payload_hash.t ->
  now:Ptime.t ->
  signed_headers
