type t = string

let field = "account_id"
let invalid message = Error (Awskit.Error.Producer.validation ~field message)
let is_ascii_digit = function '0' .. '9' -> true | _ -> false

let of_string value =
  if String.length value <> 12 then
    invalid "account_id must be exactly 12 digits"
  else if not (String.for_all is_ascii_digit value) then
    invalid "account_id must contain only ASCII digits"
  else Ok value

let of_string_exn value = Awskit.Error.Producer.get_ok_exn (of_string value)
let to_string value = value
let pp fmt value = Format.pp_print_string fmt value
let equal = String.equal
