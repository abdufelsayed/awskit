open Base

type t = {
  access_key_id : string;
  secret_access_key : string;
  session_token : string option;
}

let fail format = Fmt.kstr invalid_arg format

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let validate_required ~field value =
  if String.is_empty value then
    fail "Awskit.Credentials.make: %s is empty" field;
  if has_ctl_or_del value then
    fail "Awskit.Credentials.make: %s contains control characters" field;
  if not (String.equal value (String.strip value)) then
    fail "Awskit.Credentials.make: %s must not have leading/trailing whitespace"
      field

let validate_optional ~field = function
  | None -> ()
  | Some value ->
      if String.is_empty value then
        fail "Awskit.Credentials.make: %s is empty" field;
      if has_ctl_or_del value then
        fail "Awskit.Credentials.make: %s contains control characters" field;
      if not (String.equal value (String.strip value)) then
        fail
          "Awskit.Credentials.make: %s must not have leading/trailing \
           whitespace"
          field

let make ~access_key_id ~secret_access_key ?session_token () =
  validate_required ~field:"access_key_id" access_key_id;
  validate_required ~field:"secret_access_key" secret_access_key;
  validate_optional ~field:"session_token" session_token;
  { access_key_id; secret_access_key; session_token }

let access_key_id t = t.access_key_id
let secret_access_key t = t.secret_access_key
let session_token t = t.session_token
