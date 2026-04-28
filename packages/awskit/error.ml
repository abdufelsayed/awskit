open Base
module Format = Stdlib.Format

type validation = { field : string option; message : string } [@@deriving eq]

type transport = { message : string; retryable : bool; cause : string option }
[@@deriving eq]

type service = {
  status : int;
  code : string option;
  message : string option;
  request_id : string option;
  host_id : string option;
  headers : (string * string) list;
  body : string option;
}
[@@deriving eq]

type body = { message : string; limit : int64 option } [@@deriving eq]

type retry_class =
  [ `Retryable
  | `Throttled
  | `Auth
  | `Conflict
  | `Not_found
  | `Fatal
  | `Unknown ]
[@@deriving eq]

type t =
  | Validation of validation
  | Signing of string
  | Transport of transport
  | Service of service
  | Decode of string
  | Body of body
[@@deriving eq]

let validation ?field message = Validation { field; message }
let signing message = Signing message

let transport ?cause ~retryable message =
  Transport { message; retryable; cause }

let service service = Service service
let decode message = Decode message
let body ?limit message = Body { message; limit }

let code_is codes = function
  | None -> false
  | Some code -> List.exists codes ~f:(String.Caseless.equal code)

let retry_class = function
  | Validation _ -> `Fatal
  | Signing _ -> `Auth
  | Decode _ -> `Fatal
  | Body _ -> `Fatal
  | Transport { retryable = true; _ } -> `Retryable
  | Transport { retryable = false; _ } -> `Fatal
  | Service { status; code; _ } when status = 429 -> `Throttled
  | Service { code; _ }
    when code_is
           [
             "Throttling";
             "ThrottlingException";
             "TooManyRequestsException";
             "SlowDown";
           ]
           code ->
      `Throttled
  | Service { status; _ } when status = 401 || status = 403 -> `Auth
  | Service { status; _ } when status = 404 -> `Not_found
  | Service { status; _ } when status = 409 || status = 412 -> `Conflict
  | Service { status; _ }
    when status = 500 || status = 502 || status = 503 || status = 504 ->
      `Retryable
  | Service { status; _ } when status >= 400 && status < 500 -> `Fatal
  | Service _ -> `Unknown

let is_not_found t = Poly.equal (retry_class t) `Not_found

let pp_retry_class fmt = function
  | `Retryable -> Format.pp_print_string fmt "retryable"
  | `Throttled -> Format.pp_print_string fmt "throttled"
  | `Auth -> Format.pp_print_string fmt "auth"
  | `Conflict -> Format.pp_print_string fmt "conflict"
  | `Not_found -> Format.pp_print_string fmt "not_found"
  | `Fatal -> Format.pp_print_string fmt "fatal"
  | `Unknown -> Format.pp_print_string fmt "unknown"

let pp_option pp fmt = function
  | None -> Format.pp_print_string fmt "none"
  | Some value -> pp fmt value

let pp fmt = function
  | Validation { field; message } ->
      Format.fprintf fmt "validation";
      Option.iter field ~f:(fun field -> Format.fprintf fmt ":%s" field);
      Format.fprintf fmt ": %s" message
  | Signing message -> Format.fprintf fmt "signing: %s" message
  | Transport { message; retryable; cause } ->
      Format.fprintf fmt "transport%s: %s"
        (if retryable then " retryable" else "")
        message;
      Option.iter cause ~f:(fun cause -> Format.fprintf fmt " (%s)" cause)
  | Service ({ status; code; message; request_id; host_id; _ } as service) ->
      Format.fprintf fmt "service status=%d retry=%a code=%a message=%a" status
        pp_retry_class
        (retry_class (Service service))
        (pp_option Format.pp_print_string)
        code
        (pp_option Format.pp_print_string)
        message;
      Option.iter request_id ~f:(fun id ->
          Format.fprintf fmt " request_id=%s" id);
      Option.iter host_id ~f:(fun id -> Format.fprintf fmt " host_id=%s" id)
  | Decode message -> Format.fprintf fmt "decode: %s" message
  | Body { message; limit } ->
      Format.fprintf fmt "body: %s" message;
      Option.iter limit ~f:(fun limit -> Format.fprintf fmt " limit=%Ld" limit)

let to_string_hum t = Format.asprintf "%a" pp t
