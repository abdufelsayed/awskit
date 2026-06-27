module Aws_error = Error
open Base
module Format = Stdlib.Format

module Scheme = struct
  type t = [ `Http | `Https ] [@@deriving show, eq]

  let to_string = function `Http -> "http" | `Https -> "https"
end

type t = { scheme : Scheme.t; host : string; port : int option } [@@deriving eq]

let bracket_ipv6_host host =
  if String.contains host ':' then Fmt.str "[%s]" host else host

let authority_host_port ~host ~port =
  let host = bracket_ipv6_host host in
  match port with None -> host | Some port -> Fmt.str "%s:%d" host port

let pp fmt t =
  Format.fprintf fmt "%s://%s"
    (Scheme.to_string t.scheme)
    (authority_host_port ~host:t.host ~port:t.port)

let invalid = Aws_validation.invalid
let validate_host = Aws_validation.Host.validate ~allow_ipv6_brackets:false
let validate_port = Aws_validation.validate_port

let create ~scheme ~host ?port () =
  match validate_host host with
  | Error _ as error -> error
  | Ok () -> (
      match validate_port port with
      | Error _ as error -> error
      | Ok () -> Ok { scheme; host; port })

let create_exn ~scheme ~host ?port () =
  Aws_error.Producer.get_ok_exn (create ~scheme ~host ?port ())

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

let split_ipv6_authority authority =
  match String.substr_index authority ~pattern:"]" with
  | None -> invalid ~field:"endpoint" "bracketed IPv6 endpoint is missing ']'"
  | Some close_index ->
      let host = String.sub authority ~pos:1 ~len:(close_index - 1) in
      let rest = String.drop_prefix authority (close_index + 1) in
      if String.is_empty rest then Ok (host, None)
      else if String.is_prefix rest ~prefix:":" then
        let port_string = String.drop_prefix rest 1 in
        match Int.of_string_opt port_string with
        | Some port -> Ok (host, Some port)
        | None ->
            invalid ~field:"port"
              (Fmt.str "invalid endpoint port: %s" port_string)
      else invalid ~field:"endpoint" "invalid bracketed IPv6 endpoint authority"

let split_host_port authority =
  if String.is_prefix authority ~prefix:"[" then split_ipv6_authority authority
  else
    match String.rsplit2 authority ~on:':' with
    | None -> Ok (authority, None)
    | Some (host, port_string) when String.contains host ':' ->
        invalid ~field:"endpoint" "IPv6 endpoint hosts must use brackets"
    | Some (host, port_string) -> (
        match Int.of_string_opt port_string with
        | Some port -> Ok (host, Some port)
        | None ->
            invalid ~field:"port"
              (Fmt.str "invalid endpoint port: %s" port_string))

let of_string input =
  if String.is_empty input then
    invalid ~field:"endpoint" "endpoint must be non-empty"
  else if Aws_validation.has_leading_or_trailing_whitespace input then
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

let of_string_exn input = Aws_error.Producer.get_ok_exn (of_string input)
let http ~host ?port () = create ~scheme:`Http ~host ?port ()

let http_exn ~host ?port () =
  Aws_error.Producer.get_ok_exn (http ~host ?port ())

let https ~host ?port () = create ~scheme:`Https ~host ?port ()

let https_exn ~host ?port () =
  Aws_error.Producer.get_ok_exn (https ~host ?port ())

let scheme t = t.scheme
let host t = t.host
let port t = t.port
let authority t = authority_host_port ~host:t.host ~port:t.port

let to_url_prefix t =
  Fmt.str "%s://%s" (Scheme.to_string t.scheme) (authority t)
