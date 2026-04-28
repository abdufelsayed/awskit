(** Runtime-neutral body metadata.

    Runtime adapters own concrete upload and download body values. Core modules
    use this metadata for signing, headers, retry planning, and buffer limits.
*)

module Payload_hash : sig
  type t = Sha256_hex of string | Unsigned_payload

  val of_sha256_hex : string -> (t, Error.t) result
  val of_sha256_hex_exn : string -> t
  val sha256_of_string : string -> t
  val unsigned_payload : t
  val to_header_value : t -> string
end

module Upload : sig
  type descriptor = {
    content_length : int64 option;
    payload_hash : Payload_hash.t;
    replayable : bool;
  }

  val validate_descriptor : descriptor -> (unit, Error.t) result
end

module Download : sig
  type descriptor = {
    content_length : int64 option;
    content_type : string option;
    headers : (string * string) list;
  }
end
