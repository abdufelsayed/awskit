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

val canonical_query : string -> string
(** Sort parameters by name, URI-encode keys and values. *)

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
(** Full AWS SigV4. [headers] must include [("host", ...)]. *)
