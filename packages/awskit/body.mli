(** Runtime-neutral body metadata.

    Runtime adapters own concrete request and response body values. Core modules
    use this metadata for signing, headers, retry planning, and buffer limits.
*)

module Payload_hash : sig
  (** Payload hash value used for SigV4 signing and the [x-amz-content-sha256]
      header. *)
  type t = Sha256_hex of string | Unsigned_payload

  val of_sha256_hex : string -> (t, Error.t) result
  (** Validate and wrap a lowercase or uppercase SHA-256 hex digest. *)

  val of_sha256_hex_exn : string -> t
  (** Like {!val:of_sha256_hex}, but raises [Error.Awskit_error] carrying the
      structured validation error on invalid input. *)

  val sha256_of_string : string -> t
  (** Compute the SHA-256 payload hash for an in-memory string. *)

  val unsigned_payload : t
  (** SigV4 [UNSIGNED-PAYLOAD], used by APIs such as S3 presigned URLs where the
      body is not signed directly. *)

  val to_header_value : t -> string
  (** Render the value for [x-amz-content-sha256]. *)
end

module Request : sig
  type descriptor = {
    content_length : int64 option;
        (** Exact body size when known. S3 upload operations currently require a
            known content length. *)
    payload_hash : Payload_hash.t;
        (** Payload hash or [UNSIGNED-PAYLOAD] value used for signing. *)
    replayable : bool;
        (** Whether the body writer can be run again for a retry. Set this to
            [false] for one-shot streams. *)
  }
  (** Request body facts known before a runtime writes the body. Service
      packages use this to sign requests and decide whether retry is possible.
  *)

  val validate_descriptor : descriptor -> (unit, Error.t) result
  (** Validate descriptor invariants such as non-negative lengths. *)
end

module Response : sig
  type descriptor = {
    content_length : int64 option;
    content_type : string option;
    headers : (string * string) list;
  }
  (** Response body metadata that can be known before or while consuming the
      stream. The response body itself remains runtime-owned. *)
end
