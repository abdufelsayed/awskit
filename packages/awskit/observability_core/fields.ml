module Aws_error = Error
module Aws_request = Request
open Base

module Outcome = struct
  type t =
    | Ok
    | Not_found
    | Conflict
    | Throttled
    | Error
    | Exception
    | Cancelled
    | Timeout

  let to_string = function
    | Ok -> "ok"
    | Not_found -> "not_found"
    | Conflict -> "conflict"
    | Throttled -> "throttled"
    | Error -> "error"
    | Exception -> "exception"
    | Cancelled -> "cancelled"
    | Timeout -> "timeout"

  let of_error error =
    if Aws_error.is_cancelled error then Cancelled
    else if Aws_error.is_timeout error then Timeout
    else
      match Aws_error.retry_class error with
      | Not_found -> Not_found
      | Conflict -> Conflict
      | Throttled -> Throttled
      | Retryable | Auth | Fatal | Unknown -> Error

  let pp ppf t = Fmt.string ppf (to_string t)
end

module Span_kind = struct
  type t = Internal | Client
end

module Dimension = struct
  type t = { name : string; value : string }

  let create ~name ~value = { name; value }
  let name t = t.name
  let value t = t.value
  let pp ppf t = Fmt.pf ppf "%s=%s" t.name t.value

  module Enum = struct
    type 'a t = {
      name : string;
      equal : 'a -> 'a -> bool;
      values : ('a * string) list;
    }

    let define ~name ~equal ~values =
      if String.is_empty name then
        invalid_arg "Awskit.Observability.Dimension.Enum: empty name";
      if List.is_empty values then
        invalid_arg "Awskit.Observability.Dimension.Enum: empty values";
      List.iter values ~f:(fun (_, encoded) ->
          if String.is_empty encoded then
            invalid_arg "Awskit.Observability.Dimension.Enum: empty value");
      let encoded_values = List.map values ~f:snd in
      if List.contains_dup encoded_values ~compare:String.compare then
        invalid_arg "Awskit.Observability.Dimension.Enum: duplicate value";
      let rec has_duplicate_domain = function
        | [] -> false
        | (value, _) :: rest ->
            List.exists rest ~f:(fun (candidate, _) -> equal value candidate)
            || has_duplicate_domain rest
      in
      if has_duplicate_domain values then
        invalid_arg "Awskit.Observability.Dimension.Enum: duplicate domain";
      { name; equal; values }

    let encode t value =
      match
        List.find t.values ~f:(fun (candidate, _) -> t.equal value candidate)
      with
      | Some (_, encoded) -> encoded
      | None ->
          invalid_arg
            "Awskit.Observability.Dimension.Enum: undeclared dimension value"

    let value t value = create ~name:t.name ~value:(encode t value)
    let name t = t.name
    let allowed_values t = List.map t.values ~f:snd
    let cardinality t = List.length t.values
  end

  let http_method : Aws_request.Method.t Enum.t =
    Enum.define ~name:"http.request.method" ~equal:Poly.equal
      ~values:
        [
          (`GET, "GET");
          (`HEAD, "HEAD");
          (`POST, "POST");
          (`PUT, "PUT");
          (`DELETE, "DELETE");
          (`PATCH, "PATCH");
        ]

  let outcome =
    Enum.define ~name:"outcome" ~equal:Poly.equal
      ~values:
        [
          (Outcome.Ok, "ok");
          (Not_found, "not_found");
          (Conflict, "conflict");
          (Throttled, "throttled");
          (Error, "error");
          (Exception, "exception");
          (Cancelled, "cancelled");
          (Timeout, "timeout");
        ]

  type status_class =
    [ `Informational | `Success | `Redirection | `Client_error | `Server_error ]

  let status_class : status_class Enum.t =
    Enum.define ~name:"http.response.status_class" ~equal:Poly.equal
      ~values:
        [
          (`Informational, "1xx");
          (`Success, "2xx");
          (`Redirection, "3xx");
          (`Client_error, "4xx");
          (`Server_error, "5xx");
        ]

  let status_class_of_code status : status_class =
    if status < 200 then `Informational
    else if status < 300 then `Success
    else if status < 400 then `Redirection
    else if status < 500 then `Client_error
    else `Server_error

  let retry_class =
    Enum.define ~name:"retry.class" ~equal:Poly.equal
      ~values:
        [
          (Aws_error.Retryable, "retryable");
          (Throttled, "throttled");
          (Auth, "auth");
          (Conflict, "conflict");
          (Not_found, "not_found");
          (Fatal, "fatal");
          (Unknown, "unknown");
        ]

  type replayability = Replayable | Non_replayable

  let replayability =
    Enum.define ~name:"request.replayability" ~equal:Poly.equal
      ~values:[ (Replayable, "replayable"); (Non_replayable, "non_replayable") ]

  type credential_source =
    [ `Static
    | `Env
    | `Shared_file of string
    | `Config_file of string
    | `Container
    | `Imds
    | `Custom of string ]

  let equal_credential_source (left : credential_source)
      (right : credential_source) =
    match (left, right) with
    | `Static, `Static
    | `Env, `Env
    | `Container, `Container
    | `Imds, `Imds
    | `Shared_file _, `Shared_file _
    | `Config_file _, `Config_file _
    | `Custom _, `Custom _ ->
        true
    | _ -> false

  let credential_source : credential_source Enum.t =
    Enum.define ~name:"credentials.source" ~equal:equal_credential_source
      ~values:
        [
          (`Static, "static");
          (`Env, "environment");
          (`Shared_file "", "shared_file");
          (`Config_file "", "config_file");
          (`Container, "container");
          (`Imds, "imds");
          (`Custom "", "custom");
        ]
end

module Measurement = struct
  type value = Int of int | Int64 of int64 | Float of float
  type t = { name : string; unit_ : string option; value : value }

  let create ?unit_ ~name value =
    if String.is_empty name then
      invalid_arg "Awskit.Observability.Measurement: empty name";
    { name; unit_; value }

  let int ?unit_ ~name value = create ?unit_ ~name (Int value)
  let int64 ?unit_ ~name value = create ?unit_ ~name (Int64 value)
  let float ?unit_ ~name value = create ?unit_ ~name (Float value)
  let name t = t.name
  let unit_ t = t.unit_
  let value t = t.value

  let pp_value ppf = function
    | Int value -> Fmt.int ppf value
    | Int64 value -> Fmt.int64 ppf value
    | Float value -> Fmt.float ppf value

  let pp ppf t =
    match t.unit_ with
    | None -> Fmt.pf ppf "%s=%a" t.name pp_value t.value
    | Some unit_ -> Fmt.pf ppf "%s=%a%s" t.name pp_value t.value unit_
end

module Diagnostic = struct
  type value =
    | String of string
    | Bool of bool
    | Int of int
    | Int64 of int64
    | Float of float

  type t = { name : string; value : value }

  module Public = struct
    type nonrec value = value
    type nonrec t = t

    let name t = t.name
    let value t = t.value

    let pp_value ppf = function
      | String value -> Fmt.pf ppf "%S" value
      | Bool value -> Fmt.bool ppf value
      | Int value -> Fmt.int ppf value
      | Int64 value -> Fmt.int64 ppf value
      | Float value -> Fmt.float ppf value

    let pp ppf t = Fmt.pf ppf "%s=%a" t.name pp_value t.value
  end

  let create ~name value = { name; value }

  let attempt_number value =
    if value > 0 then Some (create ~name:"attempt" (Int value)) else None

  let http_status_code value =
    if value >= 100 && value <= 599 then
      Some (create ~name:"http.response.status_code" (Int value))
    else None

  let max_provider_identifier_bytes = 1024

  let is_printable_ascii character =
    let code = Char.to_int character in
    code >= 0x20 && code <= 0x7e

  let provider_identifier ~name value =
    let length = String.length value in
    if
      length > 0
      && length <= max_provider_identifier_bytes
      && String.for_all value ~f:is_printable_ascii
    then Some (create ~name (String value))
    else None

  let aws_request_id = provider_identifier ~name:"aws.request_id"

  let aws_extended_request_id =
    provider_identifier ~name:"aws.extended_request_id"
end

module Fields = struct
  type t = {
    dimensions : Dimension.t list;
    measurements : Measurement.t list;
    diagnostics : Diagnostic.t list;
  }

  let empty = { dimensions = []; measurements = []; diagnostics = [] }

  let create ?(dimensions = []) ?(measurements = []) ?(diagnostics = []) () =
    { dimensions; measurements; diagnostics }

  let dimensions t = t.dimensions
  let measurements t = t.measurements
  let diagnostics t = t.diagnostics
end

module Correlation = struct
  let is_hex character =
    Char.is_digit character
    || Char.between character ~low:'a' ~high:'f'
    || Char.between character ~low:'A' ~high:'F'

  let valid_hex ~length value =
    Int.equal (String.length value) length
    && String.for_all value ~f:is_hex
    && not (String.for_all value ~f:(Char.equal '0'))

  let validated ~name:raw_name ~length raw_value =
    if valid_hex ~length raw_value then
      Ok (Diagnostic.create ~name:raw_name (String raw_value))
    else
      Error
        (Fmt.str "%s must be %d non-zero hexadecimal characters" raw_name length)

  let trace_id = validated ~name:"trace.id" ~length:32
  let span_id = validated ~name:"span.id" ~length:16

  let trace_flags raw_value =
    if raw_value < 0 || raw_value > 255 then
      Error "trace.flags must be between 0 and 255"
    else Ok (Diagnostic.create ~name:"trace.flags" (Int raw_value))
end
