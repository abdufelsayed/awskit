module Aws_error = Error
open Base
module Format = Stdlib.Format

type t = string

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let of_string region =
  if String.is_empty region then
    Error
      (Aws_error.Internal.validation ~field:"region"
         "AWS region must be non-empty")
  else if not (String.equal region (String.strip region)) then
    Error
      (Aws_error.Internal.validation ~field:"region"
         "AWS region must not have leading/trailing whitespace")
  else if has_ctl_or_del region then
    Error
      (Aws_error.Internal.validation ~field:"region"
         "AWS region contains control characters")
  else Ok region

let of_string_exn region = Aws_error.Internal.get_ok_exn (of_string region)
let to_string t = t
let pp formatter t = Format.pp_print_string formatter t
let equal = String.equal
