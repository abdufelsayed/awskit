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
        (Aws_error.Producer.validation ~field:"payload_hash"
           "SHA256 payload hash must be 64 hex characters")
    else if not (String.for_all value ~f:is_hex) then
      Error
        (Aws_error.Producer.validation ~field:"payload_hash"
           "SHA256 payload hash must be hex")
    else Ok (Sha256_hex (String.lowercase value))

  let of_sha256_hex_exn value =
    Aws_error.Producer.get_ok_exn (of_sha256_hex value)

  let sha256_of_string value =
    Sha256_hex Digestif.SHA256.(digest_string value |> to_hex)

  let unsigned_payload = Unsigned_payload

  let to_header_value = function
    | Sha256_hex value -> value
    | Unsigned_payload -> "UNSIGNED-PAYLOAD"
end

module Request = struct
  exception Escaped_exn of exn

  type descriptor = {
    content_length : int64 option;
    payload_hash : Payload_hash.t;
    replayable : bool;
  }

  let validate_descriptor { content_length; _ } =
    match content_length with
    | Some length when Int64.(length < zero) ->
        Error
          (Aws_error.Producer.validation ~field:"content_length"
             "request content length must be non-negative")
    | _ -> Ok ()

  let descriptor ?content_length ~payload_hash ~replayable () =
    let descriptor = { content_length; payload_hash; replayable } in
    match validate_descriptor descriptor with
    | Ok () -> Ok descriptor
    | Error _ as error -> error

  let descriptor_exn ?content_length ~payload_hash ~replayable () =
    Aws_error.Producer.get_ok_exn
      (descriptor ?content_length ~payload_hash ~replayable ())

  let raise_escaped_exn exn = raise (Escaped_exn exn)
  let escaped_exn = function Escaped_exn exn -> Some exn | _ -> None
end

module Response = struct
  type descriptor = {
    content_length : int64 option;
    content_type : string option;
    headers : (string * string) list;
  }
end
