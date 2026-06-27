type t = string

let invalid ~field message =
  Error (Awskit.Error.Producer.validation ~field message)

let of_string ~field value =
  if value = "" then invalid ~field (Fmt.str "%s must be non-empty" field)
  else if S3_string.has_ctl_or_del value then
    invalid ~field (Fmt.str "%s contains control characters" field)
  else Ok value

let of_string_exn ~field value =
  Awskit.Error.Producer.get_ok_exn (of_string ~field value)

let to_string value = value
let pp fmt value = Format.pp_print_string fmt value
let equal = String.equal
