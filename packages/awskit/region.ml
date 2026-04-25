open Base

type t = string

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let of_string region =
  let region = String.strip region in
  if String.is_empty region then
    Error (`Invalid_request "AWS region must be non-empty")
  else if has_ctl_or_del region then
    Error (`Invalid_request "AWS region contains control characters")
  else Ok region

let to_string t = t
