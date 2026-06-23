module Aws_error = Error
open Base

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let has_leading_or_trailing_ws s = not (String.equal s (String.strip s))

let invalid ?field message =
  Error (Aws_error.Producer.validation ?field message)

let result_exn = Aws_error.Producer.get_ok_exn

module Method = struct
  type t = [ `GET | `PUT | `POST | `DELETE | `HEAD | `PATCH ]

  let to_string = function
    | `GET -> "GET"
    | `PUT -> "PUT"
    | `POST -> "POST"
    | `DELETE -> "DELETE"
    | `HEAD -> "HEAD"
    | `PATCH -> "PATCH"

  let of_string value =
    match String.uppercase value with
    | "GET" -> Ok `GET
    | "PUT" -> Ok `PUT
    | "POST" -> Ok `POST
    | "DELETE" -> Ok `DELETE
    | "HEAD" -> Ok `HEAD
    | "PATCH" -> Ok `PATCH
    | _ -> invalid ~field:"method" (Fmt.str "unsupported HTTP method: %s" value)

  let of_string_exn value = result_exn (of_string value)
end

let encode_query_component value =
  let buf = Buffer.create (String.length value) in
  String.iter value ~f:(fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' | '~' | '.' ->
          Buffer.add_char buf c
      | c -> Buffer.add_string buf (Fmt.str "%%%02X" (Char.to_int c)));
  Buffer.contents buf

let query_to_string query =
  query
  |> List.concat_map ~f:(fun (key, values) ->
      match values with
      | [] -> [ (key, "") ]
      | values -> List.map values ~f:(fun value -> (key, value)))
  |> List.map ~f:(fun (key, value) ->
      Fmt.str "%s=%s"
        (encode_query_component key)
        (encode_query_component value))
  |> String.concat ~sep:"&"

let validate_host host =
  if String.is_empty host then invalid ~field:"host" "host must be non-empty"
  else if has_leading_or_trailing_ws host then
    invalid ~field:"host" "host must not have leading/trailing whitespace"
  else if has_ctl_or_del host then
    invalid ~field:"host" "host contains control characters"
  else if String.is_substring host ~substring:"://" then
    invalid ~field:"host" "host must not include a URL scheme"
  else if
    String.exists host ~f:(function
      | '/' | '?' | '#' | '@' -> true
      | _ -> false)
  then invalid ~field:"host" "host must be a bare hostname or IP"
  else Ok ()

let validate_port = function
  | None -> Ok ()
  | Some port when port > 0 && port <= 65_535 -> Ok ()
  | Some port ->
      invalid ~field:"port" (Fmt.str "invalid port %d (expected 1-65535)" port)

let validate_path path =
  if String.is_empty path then invalid ~field:"path" "path must be non-empty"
  else if not (String.is_prefix path ~prefix:"/") then
    invalid ~field:"path" "path must start with /"
  else if has_ctl_or_del path then
    invalid ~field:"path" "path contains control characters"
  else Ok ()

let validate_query query =
  let validate_piece ~field value =
    if has_ctl_or_del value then
      invalid ~field (Fmt.str "%s contains control characters" field)
    else Ok ()
  in
  let rec loop = function
    | [] -> Ok ()
    | (key, values) :: rest -> (
        match validate_piece ~field:"query key" key with
        | Error _ as error -> error
        | Ok () -> (
            match List.find values ~f:(fun value -> has_ctl_or_del value) with
            | Some _ ->
                invalid ~field:"query value"
                  "query value contains control characters"
            | None -> loop rest))
  in
  loop query

let validate_header_name name =
  if String.is_empty name then
    invalid ~field:"header" "header name must be non-empty"
  else if has_ctl_or_del name then
    invalid ~field:"header"
      (Fmt.str "header %s contains control characters" name)
  else if String.exists name ~f:(function ':' -> true | _ -> false) then
    invalid ~field:"header" (Fmt.str "header %s must not contain ':'" name)
  else Ok ()

let validate_header_value name value =
  if String.exists value ~f:(function '\r' | '\n' -> true | _ -> false) then
    invalid ~field:"header" (Fmt.str "header %s contains a newline" name)
  else Ok ()

let validate_headers headers =
  let rec loop = function
    | [] -> Ok ()
    | (name, value) :: rest -> (
        match validate_header_name name with
        | Error _ as error -> error
        | Ok () -> (
            match validate_header_value name value with
            | Error _ as error -> error
            | Ok () -> loop rest))
  in
  loop headers

module Target = struct
  type t = {
    scheme : Endpoint.Scheme.t;
    host : string;
    port : int option;
    path : string;
    query : (string * string list) list;
  }

  let create ~scheme ~host ?port ~path ?(query = []) () =
    match validate_host host with
    | Error _ as error -> error
    | Ok () -> (
        match validate_port port with
        | Error _ as error -> error
        | Ok () -> (
            match validate_path path with
            | Error _ as error -> error
            | Ok () -> (
                match validate_query query with
                | Error _ as error -> error
                | Ok () -> Ok { scheme; host; port; path; query })))

  let create_exn ~scheme ~host ?port ~path ?query () =
    result_exn (create ~scheme ~host ?port ~path ?query ())

  let authority t =
    match t.port with
    | None -> t.host
    | Some port -> Fmt.str "%s:%d" t.host port

  let path_and_query t =
    match query_to_string t.query with
    | "" -> t.path
    | query -> t.path ^ "?" ^ query
end

type t = {
  method_ : Method.t;
  target : Target.t;
  headers : (string * string) list;
}

let create ~method_ ~target ?(headers = []) () =
  match validate_headers headers with
  | Error _ as error -> error
  | Ok () -> Ok { method_; target; headers }

let create_exn ~method_ ~target ?headers () =
  result_exn (create ~method_ ~target ?headers ())

let with_headers t headers =
  match validate_headers headers with
  | Error _ as error -> error
  | Ok () -> Ok { t with headers }

let add_header t ~name ~value =
  match validate_header_name name with
  | Error _ as error -> error
  | Ok () -> (
      match validate_header_value name value with
      | Error _ as error -> error
      | Ok () -> Ok { t with headers = (name, value) :: t.headers })
