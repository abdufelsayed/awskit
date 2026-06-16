module Aws_error = Error
open Base

type t = {
  status : int;
  headers : (string * string) list;
  request_id : string option;
  host_id : string option;
}

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let invalid ?field message =
  Error (Aws_error.Internal.validation ?field message)

let decode message = Error (Aws_error.Internal.decode message)
let result_exn = Aws_error.Internal.get_ok_exn

let validate_status status =
  if status >= 100 && status <= 599 then Ok ()
  else invalid ~field:"status" (Fmt.str "invalid HTTP status: %d" status)

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

let header_in headers name =
  List.find_map headers ~f:(fun (key, value) ->
      if String.Caseless.equal key name then Some value else None)

let create ~status ?(headers = []) () =
  match validate_status status with
  | Error _ as error -> error
  | Ok () -> (
      match validate_headers headers with
      | Error _ as error -> error
      | Ok () ->
          Ok
            {
              status;
              headers;
              request_id = header_in headers "x-amz-request-id";
              host_id = header_in headers "x-amz-id-2";
            })

let create_exn ~status ?headers () = result_exn (create ~status ?headers ())
let status t = t.status
let headers t = t.headers
let header t name = header_in t.headers name

let required_header t name =
  match header t name with
  | Some value when not (String.is_empty value) -> Ok value
  | Some _ -> decode (Fmt.str "required response header is empty: %s" name)
  | None -> decode (Fmt.str "required response header is missing: %s" name)

let header_int t name =
  match header t name with
  | None -> Ok None
  | Some value -> (
      match Int.of_string_opt value with
      | Some parsed when parsed >= 0 -> Ok (Some parsed)
      | _ ->
          decode
            (Fmt.str "expected non-negative integer response header %s, got %s"
               name value))

let is_success t = t.status >= 200 && t.status < 300
let request_id t = t.request_id
let host_id t = t.host_id
