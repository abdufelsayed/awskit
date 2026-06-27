module Aws_error = Error
open Base

let invalid = Aws_validation.invalid

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

  let of_string_exn value = Aws_error.Producer.get_ok_exn (of_string value)
end

let validate_host = Aws_validation.Host.validate
let validate_port = Aws_validation.validate_port

let validate_path path =
  if String.is_empty path then invalid ~field:"path" "path must be non-empty"
  else if not (String.is_prefix path ~prefix:"/") then
    invalid ~field:"path" "path must start with /"
  else if Aws_validation.has_control_or_delete path then
    invalid ~field:"path" "path contains control characters"
  else Ok ()

let validate_query query =
  let validate_piece ~field value =
    if Aws_validation.has_control_or_delete value then
      invalid ~field (Fmt.str "%s contains control characters" field)
    else Ok ()
  in
  let rec loop = function
    | [] -> Ok ()
    | (key, values) :: rest -> (
        match validate_piece ~field:"query key" key with
        | Error _ as error -> error
        | Ok () -> (
            match List.find values ~f:Aws_validation.has_control_or_delete with
            | Some _ ->
                invalid ~field:"query value"
                  "query value contains control characters"
            | None -> loop rest))
  in
  loop query

let validate_headers = Aws_validation.Header.validate_list

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
    Aws_error.Producer.get_ok_exn (create ~scheme ~host ?port ~path ?query ())

  let authority t =
    match t.port with
    | None -> t.host
    | Some port -> Fmt.str "%s:%d" t.host port

  let path_and_query t =
    match Aws_uri.render_query_params t.query with
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
  Aws_error.Producer.get_ok_exn (create ~method_ ~target ?headers ())

let with_headers t headers =
  match validate_headers headers with
  | Error _ as error -> error
  | Ok () -> Ok { t with headers }

let add_header t ~name ~value = with_headers t (t.headers @ [ (name, value) ])
