(** Object encryption domains for S3 request and response metadata. *)

module Customer_key : sig
  type t
  (** SSE-C customer key material rendered as AES256 headers.

      Values store only the base64-encoded key and its base64-encoded MD5 digest
      as required by S3 request headers. *)

  val of_bytes : bytes -> (t, Awskit.Error.t) result
  (** Build an AES256 SSE-C customer key from raw key bytes. The input must be
      exactly 32 bytes. *)

  val of_bytes_exn : bytes -> t
  (** Like {!val:of_bytes}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val of_base64 : string -> (t, Awskit.Error.t) result
  (** Build an AES256 SSE-C customer key from a base64-encoded raw key. The
      decoded input must be exactly 32 bytes. Empty and invalid base64 values
      are rejected. *)

  val of_base64_exn : string -> t
  (** Like {!val:of_base64}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val algorithm : t -> string
  (** Return the SSE-C algorithm header value, always ["AES256"]. *)

  val key_base64 : t -> string
  (** Return the base64-encoded raw key.

      This value is sensitive; it is exposed so callers can inspect or forward
      the exact request headers they must send. *)

  val key_md5_base64 : t -> string
  (** Return the base64-encoded MD5 digest of the raw key bytes. *)
end

module Destination : sig
  (** Valid encryption settings callers may send for a destination object. The
      private variant keeps alternatives inspectable while requiring validated
      KMS key ids and making DSSE-KMS bucket keys unconstructible. *)

  type t = private
    | Sse_s3
    | Sse_kms of { key_id : string option; bucket_key_enabled : bool option }
    | Dsse_kms of { key_id : string option }
    | Sse_c of Customer_key.t

  val sse_s3 : t
  (** S3-managed AES256 encryption. *)

  val sse_kms :
    ?key_id:string ->
    ?bucket_key_enabled:bool ->
    unit ->
    (t, Awskit.Error.t) result
  (** Validated SSE-KMS encryption. An omitted key uses the service default. *)

  val sse_kms_exn : ?key_id:string -> ?bucket_key_enabled:bool -> unit -> t
  (** Like {!val:sse_kms}, but raises on validation failure. *)

  val dsse_kms : ?key_id:string -> unit -> (t, Awskit.Error.t) result
  (** Validated DSSE-KMS encryption. Bucket keys are absent by construction. *)

  val dsse_kms_exn : ?key_id:string -> unit -> t
  (** Like {!val:dsse_kms}, but raises on validation failure. *)

  val sse_c : Customer_key.t -> t
  (** SSE-C encryption with a validated AES256 customer key. *)
end

module Source : sig
  (** Encryption settings callers may send when reading an SSE-C source. *)

  type t = Sse_c of Customer_key.t
end

module Observed : sig
  (** Encryption metadata observed in S3 responses.

      Unknown wire values are preserved for forward compatibility and are
      read-side only. *)

  type kms = { key_id : string option; bucket_key_enabled : bool option }
  (** KMS metadata returned by S3. *)

  type t =
    | Sse_s3
    | Sse_kms of kms
    | Dsse_kms of kms
    | Sse_c
    | Aws_fsx
    | Unknown of string
end
