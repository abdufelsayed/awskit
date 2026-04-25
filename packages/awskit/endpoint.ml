open Base

module Scheme = struct
  type t = [ `Http | `Https ] [@@deriving show, eq]

  let to_string = function `Http -> "http" | `Https -> "https"
end

type t = { scheme : Scheme.t; host : string; port : int option }
[@@deriving show, eq]

let fail format = Fmt.kstr invalid_arg format

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let validate_host host =
  if String.is_empty host then
    fail "Awskit.Endpoint.create: host must be non-empty";
  if has_ctl_or_del host then
    fail "Awskit.Endpoint.create: host contains control characters";
  if String.is_substring host ~substring:"://" then
    fail "Awskit.Endpoint.create: host must not include a URL scheme";
  if
    String.exists host ~f:(function
      | '/' | '?' | '#' | '@' -> true
      | _ -> false)
  then fail "Awskit.Endpoint.create: host must be a bare hostname or IP"

let validate_port = function
  | None -> ()
  | Some port when port > 0 && port <= 65_535 -> ()
  | Some port ->
      fail "Awskit.Endpoint.create: invalid port %d (expected 1-65535)" port

let create ~scheme ~host ?port () =
  let host = String.strip host in
  validate_host host;
  validate_port port;
  { scheme; host; port }

let split_scheme input =
  match String.substr_index input ~pattern:"://" with
  | None -> Ok (`Https, input)
  | Some i ->
      let scheme = String.prefix input i |> String.lowercase in
      let rest = String.drop_prefix input (i + 3) in
      let scheme =
        match scheme with
        | "http" -> Ok `Http
        | "https" -> Ok `Https
        | _ -> Error (Fmt.str "unsupported endpoint scheme: %s" scheme)
      in
      Result.map scheme ~f:(fun scheme -> (scheme, rest))

let split_host_port authority =
  match String.rsplit2 authority ~on:':' with
  | None -> Ok (authority, None)
  | Some (host, port_s) -> (
      match Stdlib.int_of_string_opt port_s with
      | Some port -> Ok (host, Some port)
      | None -> Error (Fmt.str "invalid endpoint port: %s" port_s))

let of_string input =
  let input = String.strip input in
  try
    if String.is_empty input then Error "endpoint must be non-empty"
    else
      match split_scheme input with
      | Error _ as error -> error
      | Ok (scheme, authority) -> (
          if
            String.exists authority ~f:(function
              | '/' | '?' | '#' | '@' -> true
              | _ -> false)
          then
            Error "endpoint must not include path, query, fragment, or userinfo"
          else
            match split_host_port authority with
            | Error _ as error -> error
            | Ok (host, port) -> Ok (create ~scheme ~host ?port ()))
  with Invalid_argument msg -> Error msg

let http ~host ?port () = create ~scheme:`Http ~host ?port ()
let https ~host ?port () = create ~scheme:`Https ~host ?port ()
let scheme t = t.scheme
let host t = t.host
let port t = t.port

let authority t =
  match t.port with None -> t.host | Some port -> Fmt.str "%s:%d" t.host port

let to_url_prefix t =
  Fmt.str "%s://%s" (Scheme.to_string t.scheme) (authority t)
