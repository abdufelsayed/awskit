module Aws_error = Error
open Base

type t = {
  access_key_id : string;
  secret_access_key : string;
  session_token : string option;
}

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let invalid ~field message =
  Error (Aws_error.Internal.validation ~field message)

let validate_required ~field value =
  if String.is_empty value then invalid ~field (Fmt.str "%s is empty" field)
  else if has_ctl_or_del value then
    invalid ~field (Fmt.str "%s contains control characters" field)
  else if not (String.equal value (String.strip value)) then
    invalid ~field
      (Fmt.str "%s must not have leading/trailing whitespace" field)
  else Ok ()

let validate_optional ~field = function
  | None -> Ok ()
  | Some value -> validate_required ~field value

let create ~access_key_id ~secret_access_key ?session_token () =
  match validate_required ~field:"access_key_id" access_key_id with
  | Error _ as error -> error
  | Ok () -> (
      match validate_required ~field:"secret_access_key" secret_access_key with
      | Error _ as error -> error
      | Ok () -> (
          match validate_optional ~field:"session_token" session_token with
          | Error _ as error -> error
          | Ok () -> Ok { access_key_id; secret_access_key; session_token }))

let create_exn ~access_key_id ~secret_access_key ?session_token () =
  Aws_error.Internal.get_ok_exn
    (create ~access_key_id ~secret_access_key ?session_token ())

let hmac_sha256 ~key data =
  Digestif.SHA256.(hmac_string ~key data |> to_raw_string)

let access_key_id t = t.access_key_id
let session_token t = t.session_token

module Provider = struct
  type credentials = t
  type t = unit -> (credentials, Aws_error.t) Result.t

  let create f = f
  let resolve t = t ()
  let static credentials = fun () -> Ok credentials

  let chain providers =
   fun () ->
    let rec loop errors = function
      | [] ->
          let errors = List.rev errors in
          let error =
            match errors with
            | [] ->
                Aws_error.Internal.validation ~field:"credentials"
                  "no credential providers configured"
            | errors ->
                Aws_error.Internal.multiple errors
                |> Aws_error.Internal.with_context
                     "no credential provider resolved credentials"
          in
          Error error
      | provider :: rest -> (
          match provider () with
          | Ok _ as ok -> ok
          | Error error -> loop (error :: errors) rest)
    in
    loop [] providers
end

let signing_key t ~datestamp ~region ~service =
  hmac_sha256 ~key:("AWS4" ^ t.secret_access_key) datestamp |> fun key ->
  hmac_sha256 ~key (Region.to_string region) |> fun key ->
  hmac_sha256 ~key service |> fun key -> hmac_sha256 ~key "aws4_request"
