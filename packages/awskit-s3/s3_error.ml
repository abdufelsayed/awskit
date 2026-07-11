type t = Awskit.Error.t

let pp = Awskit.Error.pp
let pp_sexp = Awskit.Error.pp_sexp
let equal = Awskit.Error.equal
let to_string_hum = Awskit.Error.to_string_hum
let to_sexp_string_hum = Awskit.Error.to_sexp_string_hum
let sexp_of_t = Awskit.Error.sexp_of_t
let kind = Awskit.Error.kind
let context = Awskit.Error.context
let retry_class = Awskit.Error.retry_class
let is_validation = Awskit.Error.is_validation
let is_credentials = Awskit.Error.is_credentials
let is_endpoint = Awskit.Error.is_endpoint
let is_transport = Awskit.Error.is_transport
let is_timeout = Awskit.Error.is_timeout
let is_cancelled = Awskit.Error.is_cancelled
let validation_field = Awskit.Error.validation_field
let service_code = Awskit.Error.service_code
let service_status = Awskit.Error.service_status

let code_is expected error =
  match service_code error with
  | None -> false
  | Some code ->
      String.equal
        (String.lowercase_ascii code)
        (String.lowercase_ascii expected)

let is_not_found = Awskit.Error.is_not_found
let is_no_such_bucket error = code_is "NoSuchBucket" error
let is_no_such_key error = code_is "NoSuchKey" error

let is_precondition_failed error =
  Option.equal Int.equal (Awskit.Error.service_status error) (Some 412)
  || code_is "PreconditionFailed" error

let is_conditional_request_conflict error =
  code_is "ConditionalRequestConflict" error

let is_conditional_failure error =
  is_precondition_failed error || is_conditional_request_conflict error
