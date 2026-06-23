type t = string

let field = "content_type"
let invalid message = Error (Awskit.Error.Producer.validation ~field message)

let has_ctl_or_del value =
  String.exists
    (fun c ->
      let code = Char.code c in
      code < 0x20 || code = 0x7F)
    value

let of_string value =
  if value = "" then invalid "content_type must be non-empty"
  else if has_ctl_or_del value then
    invalid "content_type contains control characters"
  else Ok value

let of_string_exn value = Awskit.Error.Producer.get_ok_exn (of_string value)
let to_string value = value
let pp fmt value = Format.pp_print_string fmt value
let equal = String.equal
