module Aws_error = Error
open Base
module Format = Stdlib.Format

module Scheme = struct
  type t = [ `Http | `Https ] [@@deriving show, eq]

  let to_string = function `Http -> "http" | `Https -> "https"
end

type t = { scheme : Scheme.t; host : string; port : int option } [@@deriving eq]

let pp fmt t =
  Format.fprintf fmt "%s://%s"
    (Scheme.to_string t.scheme)
    (match t.port with
    | None -> t.host
    | Some port -> Fmt.str "%s:%d" t.host port)

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let invalid ?field message = Error (Aws_error.validation ?field message)

let validate_host host =
  if String.is_empty host then invalid ~field:"host" "host must be non-empty"
  else if not (String.equal host (String.strip host)) then
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

let create ~scheme ~host ?port () =
  match validate_host host with
  | Error _ as error -> error
  | Ok () -> (
      match validate_port port with
      | Error _ as error -> error
      | Ok () -> Ok { scheme; host; port })

let result_exn = Aws_error.get_ok_exn

let create_exn ~scheme ~host ?port () =
  result_exn (create ~scheme ~host ?port ())

let split_scheme input =
  match String.substr_index input ~pattern:"://" with
  | None -> Ok (`Https, input)
  | Some index ->
      let scheme = String.prefix input index |> String.lowercase in
      let rest = String.drop_prefix input (index + 3) in
      let scheme =
        match scheme with
        | "http" -> Ok `Http
        | "https" -> Ok `Https
        | _ ->
            invalid ~field:"scheme"
              (Fmt.str "unsupported endpoint scheme: %s" scheme)
      in
      Result.map scheme ~f:(fun scheme -> (scheme, rest))

let split_host_port authority =
  match String.rsplit2 authority ~on:':' with
  | None -> Ok (authority, None)
  | Some (host, port_string) -> (
      match Int.of_string_opt port_string with
      | Some port -> Ok (host, Some port)
      | None ->
          invalid ~field:"port"
            (Fmt.str "invalid endpoint port: %s" port_string))

let of_string input =
  if String.is_empty input then
    invalid ~field:"endpoint" "endpoint must be non-empty"
  else if not (String.equal input (String.strip input)) then
    invalid ~field:"endpoint"
      "endpoint must not have leading/trailing whitespace"
  else
    match split_scheme input with
    | Error _ as error -> error
    | Ok (scheme, authority) -> (
        if
          String.exists authority ~f:(function
            | '/' | '?' | '#' | '@' -> true
            | _ -> false)
        then
          invalid ~field:"endpoint"
            "endpoint must not include path, query, fragment, or userinfo"
        else
          match split_host_port authority with
          | Error _ as error -> error
          | Ok (host, port) -> create ~scheme ~host ?port ())

let of_string_exn input = result_exn (of_string input)
let http ~host ?port () = create ~scheme:`Http ~host ?port ()
let http_exn ~host ?port () = result_exn (http ~host ?port ())
let https ~host ?port () = create ~scheme:`Https ~host ?port ()
let https_exn ~host ?port () = result_exn (https ~host ?port ())
let scheme t = t.scheme
let host t = t.host
let port t = t.port

let authority t =
  match t.port with None -> t.host | Some port -> Fmt.str "%s:%d" t.host port

let to_url_prefix t =
  Fmt.str "%s://%s" (Scheme.to_string t.scheme) (authority t)
