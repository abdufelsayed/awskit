open Base
module Format = Stdlib.Format

type validation = { field : string option; message : string }
[@@deriving eq, sexp_of]

type transport = { message : string; retryable : bool; cause : string option }
[@@deriving eq, sexp_of]

type service = {
  status : int;
  code : string option;
  message : string option;
  request_id : string option;
  host_id : string option;
  headers : (string * string) list;
  body : string option;
}
[@@deriving eq, sexp_of]

type body = { message : string; limit : int64 option } [@@deriving eq, sexp_of]

type operation = {
  service : string option;
  name : string;
  resource : string option;
}
[@@deriving eq, sexp_of]

type retry = { attempt : int; max_attempts : int option; reason : string }
[@@deriving eq, sexp_of]

type context =
  | Message of string
  | Operation of operation
  | Retry of retry
  | Sexp of Sexp.t
[@@deriving eq, sexp_of]

type retry_class =
  | Retryable
  | Throttled
  | Auth
  | Conflict
  | Not_found
  | Fatal
  | Unknown
[@@deriving eq, sexp_of]

type kind =
  | Validation of validation
  | Signing of string
  | Transport of transport
  | Service of service
  | Decode of string
  | Body of body
  | Multiple of t list

and t = { kind : kind; context : context list } [@@deriving eq, sexp_of]

exception Awskit_error of t

let kind t = t.kind
let context t = t.context
let make kind = { kind; context = [] }
let validation ?field message = make (Validation { field; message })
let signing message = make (Signing message)

let transport ?cause ~retryable message =
  make (Transport { message; retryable; cause })

let service service = make (Service service)
let decode message = make (Decode message)
let body ?limit message = make (Body { message; limit })

let multiple = function
  | [] -> validation ~field:"errors" "no errors"
  | [ error ] -> error
  | errors -> make (Multiple errors)

let with_raw_context context t = { t with context = context :: t.context }
let with_context message t = with_raw_context (Message message) t
let with_sexp_context sexp t = with_raw_context (Sexp sexp) t

let with_operation ?service ~name ?resource () t =
  with_raw_context (Operation { service; name; resource }) t

let with_retry ~attempt ?max_attempts ~reason t =
  with_raw_context (Retry { attempt; max_attempts; reason }) t

let code_is codes = function
  | None -> false
  | Some code -> List.exists codes ~f:(String.Caseless.equal code)

let retry_class_priority = function
  | Auth -> 0
  | Throttled -> 1
  | Retryable -> 2
  | Conflict -> 3
  | Not_found -> 4
  | Fatal -> 5
  | Unknown -> 6

let higher_priority_retry_class left right =
  if retry_class_priority left <= retry_class_priority right then left
  else right

let rec retry_class t =
  match t.kind with
  | Validation _ -> Fatal
  | Signing _ -> Auth
  | Decode _ -> Fatal
  | Body _ -> Fatal
  | Transport { retryable = true; _ } -> Retryable
  | Transport { retryable = false; _ } -> Fatal
  | Service { status; code; _ } when status = 429 -> Throttled
  | Service { code; _ }
    when code_is
           [
             "Throttling";
             "ThrottlingException";
             "TooManyRequestsException";
             "SlowDown";
           ]
           code ->
      Throttled
  | Service { status; _ } when status = 401 || status = 403 -> Auth
  | Service { status; _ } when status = 404 -> Not_found
  | Service { status; _ } when status = 409 || status = 412 -> Conflict
  | Service { status; _ }
    when status = 500 || status = 502 || status = 503 || status = 504 ->
      Retryable
  | Service { status; _ } when status >= 400 && status < 500 -> Fatal
  | Service _ -> Unknown
  | Multiple [] -> Unknown
  | Multiple errors ->
      List.fold errors ~init:Unknown ~f:(fun best error ->
          higher_priority_retry_class best (retry_class error))

let rec is_validation t =
  match t.kind with
  | Validation _ -> true
  | Multiple errors -> List.exists errors ~f:is_validation
  | _ -> false

let rec validation_field t =
  match t.kind with
  | Validation { field = Some _ as field; _ } -> field
  | Validation { field = None; _ } -> None
  | Multiple errors -> List.find_map errors ~f:validation_field
  | _ -> None

let is_not_found t = equal_retry_class (retry_class t) Not_found

let rec service_code t =
  match t.kind with
  | Service { code = Some _ as code; _ } -> code
  | Service { code = None; _ } -> None
  | Multiple errors -> List.find_map errors ~f:service_code
  | _ -> None

let rec service_status t =
  match t.kind with
  | Service { status; _ } -> Some status
  | Multiple errors -> List.find_map errors ~f:service_status
  | _ -> None

let pp_retry_class fmt class_ =
  Format.pp_print_string fmt
    (match class_ with
    | Retryable -> "retryable"
    | Throttled -> "throttled"
    | Auth -> "auth"
    | Conflict -> "conflict"
    | Not_found -> "not_found"
    | Fatal -> "fatal"
    | Unknown -> "unknown")

let pp_option pp fmt = function
  | None -> Format.pp_print_string fmt "none"
  | Some value -> pp fmt value

let pp_operation fmt { service; name; resource } =
  Option.iter service ~f:(fun service -> Format.fprintf fmt "%s " service);
  Format.pp_print_string fmt name;
  Option.iter resource ~f:(fun resource -> Format.fprintf fmt " %s" resource)

let pp_context fmt = function
  | Message message -> Format.pp_print_string fmt message
  | Operation operation -> pp_operation fmt operation
  | Retry { attempt; max_attempts; reason } ->
      Format.fprintf fmt "retry attempt=%d max=%a reason=%s" attempt
        (pp_option Format.pp_print_int)
        max_attempts reason
  | Sexp sexp -> Sexp.pp_hum fmt sexp

let rec pp_kind fmt = function
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
        (retry_class { kind = Service service; context = [] })
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
  | Multiple errors ->
      Format.fprintf fmt "multiple errors:";
      List.iter errors ~f:(fun error -> Format.fprintf fmt "@;  %a" pp error)

and pp fmt t =
  List.iter (List.rev t.context) ~f:(fun context ->
      Format.fprintf fmt "%a:@;" pp_context context);
  pp_kind fmt t.kind

let pp_sexp fmt t = Sexp.pp_hum fmt (sexp_of_t t)
let to_string_hum t = Format.asprintf "%a" pp t
let to_sexp_string_hum t = Sexp.to_string_hum (sexp_of_t t)

module Internal = struct
  let validation = validation
  let signing = signing
  let transport = transport

  let service ~status ?code ?message ?request_id ?host_id ~headers ?body () =
    service { status; code; message; request_id; host_id; headers; body }

  let decode = decode
  let body = body
  let multiple = multiple
  let with_context = with_context
  let with_sexp_context = with_sexp_context
  let with_operation = with_operation
  let with_retry = with_retry
  let raise t = Stdlib.raise (Awskit_error t)
  let get_ok_exn = function Ok value -> value | Error error -> raise error
end
