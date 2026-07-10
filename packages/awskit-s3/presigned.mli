(** Runtime-neutral S3 presigned request artifacts and signing. *)

type method_ = [ `GET | `PUT | `HEAD | `DELETE ]
(** HTTP method a caller must use with the generated request. *)

type result
(** Opaque generated presigned request artifact.

    Presigned URLs are bearer tokens. The raw URL is intentionally hidden behind
    {!val:reveal_url}; use the safe accessors for logs and diagnostics.
    Consumers must use {!val:reveal_url}, {!val:method_}, and every
    {!val:reveal_request_headers} value exactly as returned. *)

val method_ : result -> method_

val safe_uri : result -> Uri.t
(** URI with SigV4 bearer query parameters removed. Operation query parameters
    are preserved. *)

val signed_header_names : result -> string list
(** Names of canonical headers included in the signature, including [host]. *)

val request_header_names : result -> string list
(** Names of signed non-[host] headers the eventual requester must send. *)

val reveal_request_headers : result -> (string * string) list
(** Deliberately reveal signed non-[host] headers for the HTTP request handoff.

    Values may contain application secrets or SSE-C customer keys. Use every
    header exactly as returned and avoid diagnostics. *)

val requested_expires_in : result -> Ptime.Span.t

val effective_expires_in : result -> Ptime.Span.t
(** Effective lifetime after capping to temporary credential lifetime. *)

val expires_at : result -> Ptime.t
(** Absolute expiration of the generated artifact. *)

val reveal_url : result -> string
(** Deliberately reveal the fully signed bearer URL. *)

val pp : Format.formatter -> result -> unit
(** Safe printer that omits the bearer URL and header values. *)

module Lifetime : sig
  type t
  (** Valid whole-second SigV4 query lifetime between one second and seven days
      inclusive. *)

  val default : t
  (** One hour. *)

  val of_span : Ptime.Span.t -> (t, Awskit.Error.t) Stdlib.result
  val of_span_exn : Ptime.Span.t -> t
  val to_span : t -> Ptime.Span.t
end

module Additional_headers : sig
  type t
  (** Valid, case-insensitively unique additional signed headers.

      Signer-owned [host], [authorization], and SigV4 authentication controls
      are rejected. Values may be sensitive. *)

  val empty : t
  val of_list : (string * string) list -> (t, Awskit.Error.t) Stdlib.result
  val of_list_exn : (string * string) list -> t
end

module Signer : sig
  type t
  (** Pure configured presigner. Runtime adapters expose client-bound helpers
      that refresh credentials and time before delegating here. *)

  val create :
    region:Awskit.Region.t ->
    credentials:Awskit.Credentials.t ->
    ?endpoint_config:Endpoint_config.t ->
    unit ->
    t

  val get_object :
    t ->
    now:Ptime.t ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?expires_in:Lifetime.t ->
    ?additional_headers:Additional_headers.t ->
    ?response_overrides:Object.Response_overrides.t ->
    ?options:Object.Get.options ->
    unit ->
    (result, Awskit.Error.t) Stdlib.result

  val put_object :
    t ->
    now:Ptime.t ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?expires_in:Lifetime.t ->
    ?additional_headers:Additional_headers.t ->
    ?options:Object.Put.options ->
    unit ->
    (result, Awskit.Error.t) Stdlib.result

  val head_object :
    t ->
    now:Ptime.t ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?expires_in:Lifetime.t ->
    ?additional_headers:Additional_headers.t ->
    ?response_overrides:Object.Response_overrides.t ->
    ?options:Object.Head.options ->
    unit ->
    (result, Awskit.Error.t) Stdlib.result

  val delete_object :
    t ->
    now:Ptime.t ->
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    ?expires_in:Lifetime.t ->
    ?additional_headers:Additional_headers.t ->
    ?options:Object.Delete.options ->
    unit ->
    (result, Awskit.Error.t) Stdlib.result

  val upload_part :
    t ->
    now:Ptime.t ->
    upload:_ Multipart.Upload.t ->
    part_number:Multipart.Part_number.t ->
    ?expires_in:Lifetime.t ->
    ?additional_headers:Additional_headers.t ->
    ?options:Multipart.Upload_part.options ->
    unit ->
    (result, Awskit.Error.t) Stdlib.result
end
