module Aws_error = Error
open Base

type source =
  [ `Static
  | `Env
  | `Shared_file of string
  | `Config_file of string
  | `Container
  | `Imds
  | `Custom of string ]

type t = {
  access_key_id : string;
  secret_access_key : string;
  session_token : string option;
  source : source option;
  expires_at : Ptime.t option;
}

let source_label_of_source = function
  | `Static -> "static"
  | `Env -> "environment"
  | `Shared_file path -> path
  | `Config_file path -> path
  | `Container -> "container"
  | `Imds -> "instance metadata"
  | `Custom source -> source

module Provider = struct
  type credentials = t
  type nonrec source = source
  type unavailable = { source : source; reason : string }

  type resolution =
    | Resolved of credentials
    | Unavailable of unavailable
    | Invalid of Aws_error.t
    | Failed of Aws_error.t

  type t = unit -> resolution

  let create f = f
  let resolve t = t ()

  let static (credentials : credentials) =
    let credentials =
      match credentials.source with
      | Some _ -> credentials
      | None -> { credentials with source = Some `Static }
    in
    fun () -> Resolved credentials

  let chain providers =
   fun () ->
    let rec loop last_unavailable = function
      | [] ->
          Unavailable
            (Option.value last_unavailable
               ~default:
                 {
                   source = `Custom "chain";
                   reason = "no credential providers configured";
                 })
      | provider :: rest -> (
          match provider () with
          | Resolved _ as resolved -> resolved
          | Unavailable unavailable -> loop (Some unavailable) rest
          | Invalid _ as invalid -> invalid
          | Failed _ as failed -> failed)
    in
    loop None providers

  let source_label = source_label_of_source
end

let has_ctl_or_del s =
  String.exists s ~f:(fun c ->
      let code = Char.to_int c in
      code < 0x20 || code = 0x7F)

let invalid ~field message =
  Error (Aws_error.Producer.validation ~field message)

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

let create ~access_key_id ~secret_access_key ?session_token ?source ?expires_at
    () =
  match validate_required ~field:"access_key_id" access_key_id with
  | Error _ as error -> error
  | Ok () -> (
      match validate_required ~field:"secret_access_key" secret_access_key with
      | Error _ as error -> error
      | Ok () -> (
          match validate_optional ~field:"session_token" session_token with
          | Error _ as error -> error
          | Ok () ->
              Ok
                {
                  access_key_id;
                  secret_access_key;
                  session_token;
                  source;
                  expires_at;
                }))

let create_exn ~access_key_id ~secret_access_key ?session_token ?source
    ?expires_at () =
  Aws_error.Producer.get_ok_exn
    (create ~access_key_id ~secret_access_key ?session_token ?source ?expires_at
       ())

let hmac_sha256 ~key data =
  Digestif.SHA256.(hmac_string ~key data |> to_raw_string)

let access_key_id t = t.access_key_id
let session_token t = t.session_token
let source t = t.source
let source_label t = Option.map t.source ~f:source_label_of_source
let expires_at t = t.expires_at
let usable_until = expires_at

let is_expired ~now t =
  match t.expires_at with
  | None -> false
  | Some expires_at -> Ptime.compare expires_at now <= 0

let validate_usable ~now ~operation t =
  if is_expired ~now t then
    Error
      (Aws_error.Producer.credentials
         ?source:(Option.map t.source ~f:source_label_of_source)
         "credentials expired before signing"
      |> Aws_error.Producer.with_operation ~service:"aws" ~name:operation ())
  else Ok ()

let validate_fresh t ~now = validate_usable ~now ~operation:"SignRequest" t

let signing_key t ~datestamp ~region ~service =
  hmac_sha256 ~key:("AWS4" ^ t.secret_access_key) datestamp |> fun key ->
  hmac_sha256 ~key (Region.to_string region) |> fun key ->
  hmac_sha256 ~key service |> fun key -> hmac_sha256 ~key "aws4_request"
