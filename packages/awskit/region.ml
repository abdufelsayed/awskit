module Aws_error = Error
open Base
module Format = Stdlib.Format

type t = string

let of_string region =
  if String.is_empty region then
    Error
      (Aws_error.Producer.validation ~field:"region"
         "AWS region must be non-empty")
  else if Aws_validation.has_leading_or_trailing_whitespace region then
    Error
      (Aws_error.Producer.validation ~field:"region"
         "AWS region must not have leading/trailing whitespace")
  else if Aws_validation.has_control_or_delete region then
    Error
      (Aws_error.Producer.validation ~field:"region"
         "AWS region contains control characters")
  else Ok region

let of_string_exn region = Aws_error.Producer.get_ok_exn (of_string region)
let to_string t = t
let pp formatter t = Format.pp_print_string formatter t
let equal = String.equal
