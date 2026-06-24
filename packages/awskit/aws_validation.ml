module Aws_error = Error
open Base

let invalid ?field message =
  Error (Aws_error.Producer.validation ?field message)

let has_control_or_delete value =
  String.exists value ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let has_leading_or_trailing_whitespace value =
  not (String.equal value (String.strip value))

let validate_port = function
  | None -> Ok ()
  | Some port when port > 0 && port <= 65_535 -> Ok ()
  | Some port ->
      invalid ~field:"port" (Fmt.str "invalid port %d (expected 1-65535)" port)

module Host = struct
  let validate ?(allow_ipv6_brackets = true) host =
    if String.is_empty host then invalid ~field:"host" "host must be non-empty"
    else if has_leading_or_trailing_whitespace host then
      invalid ~field:"host" "host must not have leading/trailing whitespace"
    else if has_control_or_delete host then
      invalid ~field:"host" "host contains control characters"
    else if String.is_substring host ~substring:"://" then
      invalid ~field:"host" "host must not include a URL scheme"
    else if
      String.exists host ~f:(function
        | '/' | '?' | '#' | '@' -> true
        | _ -> false)
    then invalid ~field:"host" "host must be a bare hostname or IP"
    else if
      (not allow_ipv6_brackets)
      && String.exists host ~f:(function '[' | ']' -> true | _ -> false)
    then
      invalid ~field:"host"
        "host must not include IPv6 brackets; brackets are URL syntax"
    else Ok ()
end

module Header = struct
  let validate_name name =
    if String.is_empty name then
      invalid ~field:"header" "header name must be non-empty"
    else if has_control_or_delete name then
      invalid ~field:"header"
        (Fmt.str "header %s contains control characters" name)
    else if String.exists name ~f:(function ':' -> true | _ -> false) then
      invalid ~field:"header" (Fmt.str "header %s must not contain ':'" name)
    else Ok ()

  let validate_value name value =
    if String.exists value ~f:(function '\r' | '\n' -> true | _ -> false) then
      invalid ~field:"header" (Fmt.str "header %s contains a newline" name)
    else Ok ()

  let validate_list headers =
    let rec loop = function
      | [] -> Ok ()
      | (name, value) :: rest -> (
          match validate_name name with
          | Error _ as error -> error
          | Ok () -> (
              match validate_value name value with
              | Error _ as error -> error
              | Ok () -> loop rest))
    in
    loop headers
end
