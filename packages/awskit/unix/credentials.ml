open Base

module Env = struct
  type 'a t = ('a, Awskit.Error.t) Result.t

  module Let_syntax = struct
    let ( let* ) result f = Result.bind result ~f
    let ( let+ ) result f = Result.map result ~f
  end

  let getenv_opt name = Stdlib.Sys.getenv_opt name

  let required name =
    match getenv_opt name with
    | None ->
        Error (Awskit.Error.validation ~field:name (Fmt.str "%s not set" name))
    | Some value when String.is_empty value ->
        Error (Awskit.Error.validation ~field:name (Fmt.str "%s is empty" name))
    | Some value -> Ok value

  let optional name =
    match getenv_opt name with
    | None -> Ok None
    | Some value when String.is_empty value ->
        Error (Awskit.Error.validation ~field:name (Fmt.str "%s is empty" name))
    | Some value -> Ok (Some value)
end

let from_env () =
  let open Env.Let_syntax in
  let* access_key_id = Env.required "AWS_ACCESS_KEY_ID" in
  let* secret_access_key = Env.required "AWS_SECRET_ACCESS_KEY" in
  let* session_token = Env.optional "AWS_SESSION_TOKEN" in
  Awskit.Credentials.create ~access_key_id ~secret_access_key ?session_token ()
