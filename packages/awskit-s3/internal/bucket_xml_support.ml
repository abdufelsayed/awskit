open Core

let xml_body node = Xml.to_string node
let bool_text value = if value then "true" else "false"

let validate_opt_header field = function
  | None -> Ok ()
  | Some value -> validate_header_value ~field value

let validate_string_list ~field values =
  let rec loop = function
    | [] -> Ok ()
    | value :: rest ->
        let* () = validate_header_value ~field value in
        loop rest
  in
  loop values

let validate_all validations =
  let rec loop = function
    | [] -> Ok ()
    | validation :: rest ->
        let* () = validation in
        loop rest
  in
  loop validations
