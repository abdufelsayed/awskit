type t = Awskit.Error.t

let pp = Awskit.Error.pp
let equal = Awskit.Error.equal
let to_string_hum = Awskit.Error.to_string_hum
let service_code = Awskit.Error.service_code

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
