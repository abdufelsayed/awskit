module Aws_error = Error
open Base

module Payload_hash = struct
  type t = Sha256_hex of string | Unsigned_payload [@@deriving eq]

  let is_hex c =
    Char.is_digit c
    || (Char.( >= ) c 'a' && Char.( <= ) c 'f')
    || (Char.( >= ) c 'A' && Char.( <= ) c 'F')

  let of_sha256_hex value =
    if String.length value <> 64 then
      Error
        (Aws_error.Internal.validation ~field:"payload_hash"
           "SHA256 payload hash must be 64 hex characters")
    else if not (String.for_all value ~f:is_hex) then
      Error
        (Aws_error.Internal.validation ~field:"payload_hash"
           "SHA256 payload hash must be hex")
    else Ok (Sha256_hex (String.lowercase value))

  let of_sha256_hex_exn value =
    Aws_error.Internal.get_ok_exn (of_sha256_hex value)

  let sha256_of_string value =
    Sha256_hex Digestif.SHA256.(digest_string value |> to_hex)

  let unsigned_payload = Unsigned_payload

  let to_header_value = function
    | Sha256_hex value -> value
    | Unsigned_payload -> "UNSIGNED-PAYLOAD"
end

module Request = struct
  type descriptor = {
    content_length : int64 option;
    payload_hash : Payload_hash.t;
    replayable : bool;
  }

  let validate_descriptor { content_length; _ } =
    match content_length with
    | Some length when Int64.(length < zero) ->
        Error
          (Aws_error.Internal.validation ~field:"content_length"
             "request content length must be non-negative")
    | _ -> Ok ()
end

module Response = struct
  type descriptor = {
    content_length : int64 option;
    content_type : string option;
    headers : (string * string) list;
  }
end
