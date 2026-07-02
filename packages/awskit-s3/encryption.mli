(** Object encryption domains for S3 request and response metadata. *)

module Kms : sig
  type t
  (** KMS options for ordinary and dual-layer KMS object encryption.

      [bucket_key_enabled] applies to ordinary SSE-KMS destinations. DSSE-KMS
      request builders reject bucket-key settings because S3 does not support S3
      bucket keys for dual-layer KMS encryption. *)

  val create :
    ?key_id:string ->
    ?bucket_key_enabled:bool ->
    unit ->
    (t, Awskit.Error.t) result
  (** Build KMS encryption options. [key_id], when present, must be a valid HTTP
      header value. *)

  val create_exn : ?key_id:string -> ?bucket_key_enabled:bool -> unit -> t
  (** Like {!val:create}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val key_id : t -> string option
  (** Return the optional KMS key id. *)

  val bucket_key_enabled : t -> bool option
  (** Return the optional S3 bucket-key setting. *)
end

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
  (** Encryption settings callers may send for a destination object. *)

  type t =
    | Sse_s3
    | Sse_kms of Kms.t
    | Dsse_kms of Kms.t
    | Sse_c of Customer_key.t

  val validate_request : t -> (unit, Awskit.Error.t) result
  (** Validate that a destination encryption value can be sent in an S3 request.
  *)
end

module Source : sig
  (** Encryption settings callers may send when reading an SSE-C source. *)

  type t = Sse_c of Customer_key.t
end

module Observed : sig
  (** Encryption metadata observed in S3 responses.

      Unknown wire values are preserved for forward compatibility and are
      read-side only. *)

  type t =
    | Sse_s3
    | Sse_kms of Kms.t
    | Dsse_kms of Kms.t
    | Sse_c
    | Aws_fsx
    | Unknown of string
end
