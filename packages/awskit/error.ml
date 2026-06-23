open Base
module Format = Stdlib.Format

type validation = { field : string option; message : string }
[@@deriving eq, sexp_of]

type credentials_error = { source : string option; message : string }
[@@deriving eq, sexp_of]

type signing_error = { message : string } [@@deriving eq, sexp_of]

type endpoint_error = { uri : string option; message : string }
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
type decode = { message : string } [@@deriving eq, sexp_of]

type timeout = { operation : string option; message : string }
[@@deriving eq, sexp_of]

type cancellation = { reason : string option } [@@deriving eq, sexp_of]

type not_supported = { feature : string option; message : string }
[@@deriving eq, sexp_of]

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

type retry_exhausted = {
  attempts : int;
  max_attempts : int option;
  last_error : t option;
  message : string;
}

and kind =
  | Validation of validation
  | Credentials of credentials_error
  | Signing of signing_error
  | Endpoint of endpoint_error
  | Transport of transport
  | Timeout of timeout
  | Cancelled of cancellation
  | Service of service
  | Body of body
  | Decode of decode
  | Retry_exhausted of retry_exhausted
  | Not_supported of not_supported
  | Multiple of t list

and t = { kind : kind; context : context list } [@@deriving eq, sexp_of]

exception Awskit_error of t

let sexp_of_t_unredacted = sexp_of_t
let redacted = "<redacted>"

let contains_insensitive text substring =
  String.is_substring (String.lowercase text)
    ~substring:(String.lowercase substring)

let has_literal_secret_sentinel text =
  String.is_substring text ~substring:"SECRET"
  || String.is_substring text ~substring:"SESSION"

let sensitive_name_fragments =
  [
    "authorization";
    "cookie";
    "set-cookie";
    "token";
    "secret";
    "signature";
    "credential";
    "password";
    "api-key";
    "apikey";
    "api_key";
  ]

let sensitive_material_fragments =
  [
    "authorization:";
    "authorization=";
    "cookie:";
    "cookie=";
    "set-cookie";
    "x-amz-signature";
    "x-amz-credential";
    "x-amz-security-token";
    "aws_secret_access_key";
    "aws_session_token";
    "secret_access_key";
    "session_token";
    "signature=";
    "credential=";
    "token=";
    "secret=";
    "session=";
    "password=";
    "api-key=";
    "apikey=";
    "api_key=";
  ]

let is_sensitive_name name =
  List.exists sensitive_name_fragments ~f:(contains_insensitive name)

let contains_sensitive_material text =
  has_literal_secret_sentinel text
  || List.exists sensitive_material_fragments ~f:(contains_insensitive text)

let redact_string text =
  if contains_sensitive_material text then redacted else text

let redact_name name =
  if is_sensitive_name name || contains_sensitive_material name then redacted
  else name

let redact_option_string option = Option.map option ~f:redact_string
let redact_option_name option = Option.map option ~f:redact_name

let redact_header (name, value) =
  if is_sensitive_name name || contains_sensitive_material name then
    (redacted, redacted)
  else (redact_string name, redact_string value)

let redact_body_marker body =
  Some (Printf.sprintf "%s:%d bytes" redacted (String.length body))

let redact_validation ({ field; message } : validation) : validation =
  { field = redact_option_name field; message = redact_string message }

let redact_credentials_error ({ source; message } : credentials_error) :
    credentials_error =
  { source = redact_option_string source; message = redact_string message }

let redact_signing_error ({ message } : signing_error) : signing_error =
  { message = redact_string message }

let redact_endpoint_error ({ uri; message } : endpoint_error) : endpoint_error =
  { uri = redact_option_string uri; message = redact_string message }

let redact_transport ({ message; retryable; cause } : transport) : transport =
  {
    message = redact_string message;
    retryable;
    cause = redact_option_string cause;
  }

let redact_service
    ({ status; code; message; request_id; host_id; headers; body } : service) :
    service =
  {
    status;
    code = redact_option_string code;
    message = redact_option_string message;
    request_id = redact_option_string request_id;
    host_id = redact_option_string host_id;
    headers = List.map headers ~f:redact_header;
    body = Option.bind body ~f:redact_body_marker;
  }

let redact_body ({ message; limit } : body) : body =
  { message = redact_string message; limit }

let redact_decode ({ message } : decode) : decode =
  { message = redact_string message }

let redact_timeout ({ operation; message } : timeout) : timeout =
  {
    operation = redact_option_string operation;
    message = redact_string message;
  }

let redact_cancellation ({ reason } : cancellation) : cancellation =
  { reason = redact_option_string reason }

let redact_not_supported ({ feature; message } : not_supported) : not_supported
    =
  { feature = redact_option_string feature; message = redact_string message }

let redact_operation ({ service; name; resource } : operation) : operation =
  {
    service = redact_option_string service;
    name = redact_string name;
    resource = redact_option_string resource;
  }

let redact_retry ({ attempt; max_attempts; reason } : retry) : retry =
  { attempt; max_attempts; reason = redact_string reason }

let rec redact_context_sexp sexp =
  match sexp with
  | Sexp.Atom atom -> Sexp.Atom (redact_string atom)
  | Sexp.List (Sexp.Atom name :: values)
    when is_sensitive_name name || contains_sensitive_material name ->
      Sexp.List
        (Sexp.Atom redacted :: List.map values ~f:(fun _ -> Sexp.Atom redacted))
  | Sexp.List values -> Sexp.List (List.map values ~f:redact_context_sexp)

let redact_context = function
  | Message message -> Message (redact_string message)
  | Operation operation -> Operation (redact_operation operation)
  | Retry retry -> Retry (redact_retry retry)
  | Sexp sexp -> Sexp (redact_context_sexp sexp)

let rec redact_retry_exhausted
    ({ attempts; max_attempts; last_error; message } : retry_exhausted) :
    retry_exhausted =
  {
    attempts;
    max_attempts;
    last_error = Option.map last_error ~f:redact_t;
    message = redact_string message;
  }

and redact_kind = function
  | Validation validation -> Validation (redact_validation validation)
  | Credentials credentials ->
      Credentials (redact_credentials_error credentials)
  | Signing signing -> Signing (redact_signing_error signing)
  | Endpoint endpoint -> Endpoint (redact_endpoint_error endpoint)
  | Transport transport -> Transport (redact_transport transport)
  | Timeout timeout -> Timeout (redact_timeout timeout)
  | Cancelled cancellation -> Cancelled (redact_cancellation cancellation)
  | Service service -> Service (redact_service service)
  | Body body -> Body (redact_body body)
  | Decode decode -> Decode (redact_decode decode)
  | Retry_exhausted retry -> Retry_exhausted (redact_retry_exhausted retry)
  | Not_supported not_supported ->
      Not_supported (redact_not_supported not_supported)
  | Multiple errors -> Multiple (List.map errors ~f:redact_t)

and redact_t { kind; context } =
  { kind = redact_kind kind; context = List.map context ~f:redact_context }

let kind t = redact_kind t.kind
let context t = List.map t.context ~f:redact_context
let make kind = { kind; context = [] }
let validation ?field message = make (Validation { field; message })
let credentials ?source message = make (Credentials { source; message })
let signing message = make (Signing { message })
let endpoint ?uri message = make (Endpoint { uri; message })

let transport ?cause ~retryable message =
  make (Transport { message; retryable; cause })

let timeout ?operation message = make (Timeout { operation; message })
let cancelled ?reason () = make (Cancelled { reason })
let service service = make (Service service)
let body ?limit message = make (Body { message; limit })
let decode message = make (Decode { message })

let retry_exhausted ~attempts ?max_attempts ?last_error message =
  make (Retry_exhausted { attempts; max_attempts; last_error; message })

let not_supported ?feature message = make (Not_supported { feature; message })

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
  | Credentials _ -> Auth
  | Signing _ -> Auth
  | Endpoint _ -> Fatal
  | Timeout _ -> Retryable
  | Cancelled _ -> Fatal
  | Decode _ -> Fatal
  | Body _ -> Fatal
  | Retry_exhausted { last_error = Some error; _ } -> retry_class error
  | Retry_exhausted { last_error = None; _ } -> Fatal
  | Not_supported _ -> Fatal
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

let rec has_kind t ~f =
  match t.kind with
  | Multiple errors -> List.exists errors ~f:(has_kind ~f)
  | kind -> f kind

let rec is_validation t =
  has_kind t ~f:(function Validation _ -> true | _ -> false)

let is_credentials t =
  has_kind t ~f:(function Credentials _ -> true | _ -> false)

let is_endpoint t = has_kind t ~f:(function Endpoint _ -> true | _ -> false)
let is_transport t = has_kind t ~f:(function Transport _ -> true | _ -> false)
let is_timeout t = has_kind t ~f:(function Timeout _ -> true | _ -> false)
let is_cancelled t = has_kind t ~f:(function Cancelled _ -> true | _ -> false)

let rec validation_field t =
  match t.kind with
  | Validation { field = Some field; _ } -> Some (redact_name field)
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

let sexp_field name sexp = Sexp.List [ Sexp.Atom name; sexp ]
let sexp_of_record fields = Sexp.List fields
let sexp_of_string_option option = sexp_of_option sexp_of_string option
let sexp_of_int_option option = sexp_of_option sexp_of_int option

let sexp_of_string_pair (left, right) =
  Sexp.List [ sexp_of_string left; sexp_of_string right ]

let sexp_of_validation validation =
  let ({ field; message } : validation) = redact_validation validation in
  sexp_of_record
    [
      sexp_field "field" (sexp_of_string_option field);
      sexp_field "message" (sexp_of_string message);
    ]

let sexp_of_credentials_error credentials =
  let ({ source; message } : credentials_error) =
    redact_credentials_error credentials
  in
  sexp_of_record
    [
      sexp_field "source" (sexp_of_string_option source);
      sexp_field "message" (sexp_of_string message);
    ]

let sexp_of_signing_error signing =
  let ({ message } : signing_error) = redact_signing_error signing in
  sexp_of_record [ sexp_field "message" (sexp_of_string message) ]

let sexp_of_endpoint_error endpoint =
  let ({ uri; message } : endpoint_error) = redact_endpoint_error endpoint in
  sexp_of_record
    [
      sexp_field "uri" (sexp_of_string_option uri);
      sexp_field "message" (sexp_of_string message);
    ]

let sexp_of_transport transport =
  let ({ message; retryable; cause } : transport) =
    redact_transport transport
  in
  sexp_of_record
    [
      sexp_field "message" (sexp_of_string message);
      sexp_field "retryable" (sexp_of_bool retryable);
      sexp_field "cause" (sexp_of_string_option cause);
    ]

let sexp_of_service service =
  let ({ status; code; message; request_id; host_id; headers; body } : service)
      =
    redact_service service
  in
  sexp_of_record
    [
      sexp_field "status" (sexp_of_int status);
      sexp_field "code" (sexp_of_string_option code);
      sexp_field "message" (sexp_of_string_option message);
      sexp_field "request_id" (sexp_of_string_option request_id);
      sexp_field "host_id" (sexp_of_string_option host_id);
      sexp_field "headers" (sexp_of_list sexp_of_string_pair headers);
      sexp_field "body" (sexp_of_string_option body);
    ]

let sexp_of_body body =
  let ({ message; limit } : body) = redact_body body in
  sexp_of_record
    [
      sexp_field "message" (sexp_of_string message);
      sexp_field "limit" (sexp_of_option sexp_of_int64 limit);
    ]

let sexp_of_decode decode =
  let ({ message } : decode) = redact_decode decode in
  sexp_of_record [ sexp_field "message" (sexp_of_string message) ]

let sexp_of_timeout timeout =
  let ({ operation; message } : timeout) = redact_timeout timeout in
  sexp_of_record
    [
      sexp_field "operation" (sexp_of_string_option operation);
      sexp_field "message" (sexp_of_string message);
    ]

let sexp_of_cancellation cancellation =
  let ({ reason } : cancellation) = redact_cancellation cancellation in
  sexp_of_record [ sexp_field "reason" (sexp_of_string_option reason) ]

let sexp_of_operation operation =
  let ({ service; name; resource } : operation) = redact_operation operation in
  sexp_of_record
    [
      sexp_field "service" (sexp_of_string_option service);
      sexp_field "name" (sexp_of_string name);
      sexp_field "resource" (sexp_of_string_option resource);
    ]

let sexp_of_retry retry =
  let ({ attempt; max_attempts; reason } : retry) = redact_retry retry in
  sexp_of_record
    [
      sexp_field "attempt" (sexp_of_int attempt);
      sexp_field "max_attempts" (sexp_of_int_option max_attempts);
      sexp_field "reason" (sexp_of_string reason);
    ]

let sexp_of_not_supported not_supported =
  let ({ feature; message } : not_supported) =
    redact_not_supported not_supported
  in
  sexp_of_record
    [
      sexp_field "feature" (sexp_of_string_option feature);
      sexp_field "message" (sexp_of_string message);
    ]

let sexp_of_context context =
  match redact_context context with
  | Message message -> Sexp.List [ Sexp.Atom "Message"; sexp_of_string message ]
  | Operation operation ->
      Sexp.List [ Sexp.Atom "Operation"; sexp_of_operation operation ]
  | Retry retry -> Sexp.List [ Sexp.Atom "Retry"; sexp_of_retry retry ]
  | Sexp sexp -> Sexp.List [ Sexp.Atom "Sexp"; sexp ]

let rec sexp_of_retry_exhausted retry =
  let ({ attempts; max_attempts; last_error; message } : retry_exhausted) =
    redact_retry_exhausted retry
  in
  sexp_of_record
    [
      sexp_field "attempts" (sexp_of_int attempts);
      sexp_field "max_attempts" (sexp_of_int_option max_attempts);
      sexp_field "last_error" (sexp_of_option sexp_of_t last_error);
      sexp_field "message" (sexp_of_string message);
    ]

and sexp_of_kind kind =
  match redact_kind kind with
  | Validation validation ->
      Sexp.List [ Sexp.Atom "Validation"; sexp_of_validation validation ]
  | Credentials credentials ->
      Sexp.List
        [ Sexp.Atom "Credentials"; sexp_of_credentials_error credentials ]
  | Signing signing ->
      Sexp.List [ Sexp.Atom "Signing"; sexp_of_signing_error signing ]
  | Endpoint endpoint ->
      Sexp.List [ Sexp.Atom "Endpoint"; sexp_of_endpoint_error endpoint ]
  | Transport transport ->
      Sexp.List [ Sexp.Atom "Transport"; sexp_of_transport transport ]
  | Timeout timeout ->
      Sexp.List [ Sexp.Atom "Timeout"; sexp_of_timeout timeout ]
  | Cancelled cancellation ->
      Sexp.List [ Sexp.Atom "Cancelled"; sexp_of_cancellation cancellation ]
  | Service service ->
      Sexp.List [ Sexp.Atom "Service"; sexp_of_service service ]
  | Body body -> Sexp.List [ Sexp.Atom "Body"; sexp_of_body body ]
  | Decode decode -> Sexp.List [ Sexp.Atom "Decode"; sexp_of_decode decode ]
  | Retry_exhausted retry ->
      Sexp.List [ Sexp.Atom "Retry_exhausted"; sexp_of_retry_exhausted retry ]
  | Not_supported not_supported ->
      Sexp.List
        [ Sexp.Atom "Not_supported"; sexp_of_not_supported not_supported ]
  | Multiple errors ->
      Sexp.List [ Sexp.Atom "Multiple"; sexp_of_list sexp_of_t errors ]

and sexp_of_t t =
  let t = redact_t t in
  sexp_of_record
    [
      sexp_field "kind" (sexp_of_kind t.kind);
      sexp_field "context" (sexp_of_list sexp_of_context t.context);
    ]

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
  | Credentials { source; message } ->
      Format.fprintf fmt "credentials";
      Option.iter source ~f:(fun source -> Format.fprintf fmt ":%s" source);
      Format.fprintf fmt ": %s" message
  | Signing { message } -> Format.fprintf fmt "signing: %s" message
  | Endpoint { uri; message } ->
      Format.fprintf fmt "endpoint";
      Option.iter uri ~f:(fun uri -> Format.fprintf fmt ":%s" uri);
      Format.fprintf fmt ": %s" message
  | Transport { message; retryable; cause } ->
      Format.fprintf fmt "transport%s: %s"
        (if retryable then " retryable" else "")
        message;
      Option.iter cause ~f:(fun cause -> Format.fprintf fmt " (%s)" cause)
  | Timeout { operation; message } ->
      Format.fprintf fmt "timeout";
      Option.iter operation ~f:(fun operation ->
          Format.fprintf fmt ":%s" operation);
      Format.fprintf fmt ": %s" message
  | Cancelled { reason } ->
      Format.fprintf fmt "cancelled";
      Option.iter reason ~f:(fun reason -> Format.fprintf fmt ": %s" reason)
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
  | Body { message; limit } ->
      Format.fprintf fmt "body: %s" message;
      Option.iter limit ~f:(fun limit -> Format.fprintf fmt " limit=%Ld" limit)
  | Decode { message } -> Format.fprintf fmt "decode: %s" message
  | Retry_exhausted { attempts; max_attempts; last_error; message } ->
      Format.fprintf fmt "retry exhausted attempts=%d max=%a: %s" attempts
        (pp_option Format.pp_print_int)
        max_attempts message;
      Option.iter last_error ~f:(fun error ->
          Format.fprintf fmt "@;last error: %a" pp error)
  | Not_supported { feature; message } ->
      Format.fprintf fmt "not supported";
      Option.iter feature ~f:(fun feature -> Format.fprintf fmt ":%s" feature);
      Format.fprintf fmt ": %s" message
  | Multiple errors ->
      Format.fprintf fmt "multiple errors:";
      List.iter errors ~f:(fun error -> Format.fprintf fmt "@;  %a" pp error)

and pp fmt t =
  let t = redact_t t in
  List.iter (List.rev t.context) ~f:(fun context ->
      Format.fprintf fmt "%a:@;" pp_context context);
  pp_kind fmt t.kind

let pp_sexp fmt t = Sexp.pp_hum fmt (sexp_of_t t)
let to_string_hum t = Format.asprintf "%a" pp t
let to_sexp_string_hum t = Sexp.to_string_hum (sexp_of_t t)

let () =
  Stdlib.Printexc.register_printer (function
    | Awskit_error error -> Some (Format.asprintf "Awskit_error: %a" pp error)
    | _ -> None)

module Unsafe_diagnostics = struct
  let rec service t =
    match t.kind with
    | Service service -> Some service
    | Retry_exhausted { last_error = Some error; _ } -> service error
    | Retry_exhausted { last_error = None; _ } -> None
    | Multiple errors -> List.find_map errors ~f:service
    | Validation _ | Credentials _ | Signing _ | Endpoint _ | Transport _
    | Timeout _ | Cancelled _ | Body _ | Decode _ | Not_supported _ ->
        None

  let service_headers t =
    Option.map (service t) ~f:(fun service -> service.headers)

  let service_body t = Option.bind (service t) ~f:(fun service -> service.body)
  let to_sexp_unredacted = sexp_of_t_unredacted
end

module Producer = struct
  let validation = validation
  let credentials = credentials
  let signing = signing
  let endpoint = endpoint
  let transport = transport
  let timeout = timeout
  let cancelled = cancelled

  let service ~status ?code ?message ?request_id ?host_id ~headers ?body () =
    service { status; code; message; request_id; host_id; headers; body }

  let decode = decode
  let body = body
  let retry_exhausted = retry_exhausted
  let not_supported = not_supported
  let multiple = multiple
  let with_context = with_context
  let with_sexp_context = with_sexp_context
  let with_operation = with_operation
  let with_retry = with_retry
  let raise t = Stdlib.raise (Awskit_error t)
  let get_ok_exn = function Ok value -> value | Error error -> raise error
end
