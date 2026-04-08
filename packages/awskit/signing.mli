(** AWS SigV4 request signing. Pure — no IO.

    Service packages handle signing automatically. Available for custom AWS
    services or lower-level use.

    {[
    let signed =
      Awskit.Signing.sign_request ~credentials ~region:"us-east-1" ~service:"s3"
        ~meth:"GET" ~path:"/my-bucket/hello.txt" ~query:""
        ~headers:[ ("host", "s3.us-east-1.amazonaws.com") ]
        ~payload:"" ~now:(Ptime_clock.now ())
    ]} *)

type signed_headers = {
  headers : (string * string) list;
  signed_headers_str : string;
}
(** [headers] includes [authorization], [x-amz-date], [x-amz-content-sha256].
    [signed_headers_str] is the semicolon-separated list of signed header names.
*)

val ptime_to_date_time : Ptime.t -> string * string
(** [(datestamp, amz_date)] — ["YYYYMMDD"] and ["YYYYMMDDTHHMMSSZ"]. *)

val uri_encode : ?encode_slash:bool -> string -> string
(** URI-encode per AWS rules. [encode_slash:false] for paths. *)

val canonical_query_params : (string * string list) list -> string
(** Sort parameters by name/value and URI-encode keys and values. Each key may
    appear multiple times with different values. *)

val canonical_query : string -> string
(** Parse, sort, and URI-encode a raw query string. Prefer
    {!val:canonical_query_params} when you already have structured query
    parameters. *)

val sign_request_params :
  credentials:Credentials.t ->
  region:string ->
  service:string ->
  meth:string ->
  path:string ->
  query_params:(string * string list) list ->
  headers:(string * string) list ->
  payload:string ->
  now:Ptime.t ->
  signed_headers
(** Like {!val:sign_request}, but takes structured query parameters and avoids
    double-encoding. Prefer this when building queries programmatically. *)

val sign_request :
  credentials:Credentials.t ->
  region:string ->
  service:string ->
  meth:string ->
  path:string ->
  query:string ->
  headers:(string * string) list ->
  payload:string ->
  now:Ptime.t ->
  signed_headers
(** Full AWS SigV4. [headers] must include exactly one non-empty [host] header.
    Raises [Invalid_argument] if required headers are missing or malformed. *)
