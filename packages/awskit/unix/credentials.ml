open Base

module Env = struct
  type 'a t = ('a, Awskit.Error.t) Result.t

  module Let_syntax = struct
    let ( let* ) result f = Result.bind result ~f
    let ( let+ ) result f = Result.map result ~f
  end

  type getenv = string -> string option

  let getenv_opt name = Stdlib.Sys.getenv_opt name

  let required ?(getenv = getenv_opt) name =
    match getenv name with
    | None ->
        Error
          (Awskit.Error.Producer.validation ~field:name
             (Fmt.str "%s not set" name))
    | Some value when String.is_empty value ->
        Error
          (Awskit.Error.Producer.validation ~field:name
             (Fmt.str "%s is empty" name))
    | Some value -> Ok value

  let optional ?(getenv = getenv_opt) name =
    match getenv name with
    | None -> Ok None
    | Some value when String.is_empty value ->
        Error
          (Awskit.Error.Producer.validation ~field:name
             (Fmt.str "%s is empty" name))
    | Some value -> Ok (Some value)
end

let from_env ?getenv () =
  let open Env.Let_syntax in
  let* access_key_id = Env.required ?getenv "AWS_ACCESS_KEY_ID" in
  let* secret_access_key = Env.required ?getenv "AWS_SECRET_ACCESS_KEY" in
  let* session_token = Env.optional ?getenv "AWS_SESSION_TOKEN" in
  Awskit.Credentials.create ~access_key_id ~secret_access_key ?session_token
    ~source:`Env ()

module Ini = struct
  type section = { name : string; values : (string * string) list }
  type t = section list

  let strip_comment line =
    let rec loop index =
      if index >= String.length line then line
      else
        match line.[index] with
        | '#' | ';' -> String.sub line ~pos:0 ~len:index
        | _ -> loop (index + 1)
    in
    loop 0 |> String.strip

  let parse_section line =
    if
      String.length line >= 2
      && Char.equal line.[0] '['
      && Char.equal line.[String.length line - 1] ']'
    then
      Some (String.sub line ~pos:1 ~len:(String.length line - 2) |> String.strip)
    else None

  let parse_binding line =
    match String.lsplit2 line ~on:'=' with
    | None -> None
    | Some (key, value) -> Some (String.strip key, String.strip value)

  let parse contents =
    let push current sections =
      match current with
      | None -> sections
      | Some section -> section :: sections
    in
    let lines = String.split_lines contents in
    let current, sections =
      List.fold lines ~init:(None, []) ~f:(fun (current, sections) line ->
          let line = strip_comment line in
          if String.is_empty line then (current, sections)
          else
            match parse_section line with
            | Some name -> (Some { name; values = [] }, push current sections)
            | None -> (
                match (current, parse_binding line) with
                | Some section, Some binding ->
                    ( Some { section with values = binding :: section.values },
                      sections )
                | _ -> (current, sections)))
    in
    push current sections
    |> List.rev
    |> List.map ~f:(fun section ->
        { section with values = List.rev section.values })

  let section t name =
    List.find t ~f:(fun section -> String.equal section.name name)

  let get section key =
    List.Assoc.find section.values key ~equal:String.Caseless.equal
end

let read_file path =
  match Stdlib.Sys.file_exists path with
  | false -> Ok None
  | true -> (
      try
        let channel = Stdlib.open_in_bin path in
        Stdlib.Fun.protect
          ~finally:(fun () -> Stdlib.close_in_noerr channel)
          (fun () ->
            let length = Stdlib.in_channel_length channel in
            Ok (Some (Stdlib.really_input_string channel length)))
      with exn ->
        Error
          (Awskit.Error.Producer.validation ~field:path
             (Fmt.str "failed to read AWS credentials file: %s"
                (Stdlib.Printexc.to_string exn))))

let default_home ?getenv () =
  match Env.optional ?getenv "HOME" with
  | Ok (Some home) -> Ok home
  | Ok None ->
      Error (Awskit.Error.Producer.validation ~field:"HOME" "HOME not set")
  | Error _ as error -> error

let default_credentials_file ?getenv ?home () =
  match Env.optional ?getenv "AWS_SHARED_CREDENTIALS_FILE" with
  | Ok (Some path) -> Ok path
  | Ok None ->
      let home =
        match home with Some home -> Ok home | None -> default_home ?getenv ()
      in
      Result.map home ~f:(fun home ->
          Stdlib.Filename.concat home ".aws/credentials")
  | Error _ as error -> error

let default_config_file ?getenv ?home () =
  match Env.optional ?getenv "AWS_CONFIG_FILE" with
  | Ok (Some path) -> Ok path
  | Ok None ->
      let home =
        match home with Some home -> Ok home | None -> default_home ?getenv ()
      in
      Result.map home ~f:(fun home -> Stdlib.Filename.concat home ".aws/config")
  | Error _ as error -> error

let default_profile ?getenv () =
  match Env.optional ?getenv "AWS_PROFILE" with
  | Ok (Some profile) -> Ok profile
  | Ok None -> Ok "default"
  | Error _ as error -> error

let config_section_name profile =
  if String.equal profile "default" then "default" else "profile " ^ profile

type profile_sections = {
  merged : Ini.section;
  credentials_section : Ini.section option;
  config_section : Ini.section option;
}

let profile_sections ~profile ~credentials_ini ~config_ini =
  let credentials_section = Ini.section credentials_ini profile in
  let config_section = Ini.section config_ini (config_section_name profile) in
  match (credentials_section, config_section) with
  | None, None -> None
  | Some merged, None ->
      Some { merged; credentials_section; config_section = None }
  | None, Some merged ->
      Some { merged; credentials_section = None; config_section }
  | Some credentials_section, Some config_section ->
      Some
        {
          merged =
            {
              Ini.name = credentials_section.name;
              values = credentials_section.values @ config_section.values;
            };
          credentials_section = Some credentials_section;
          config_section = Some config_section;
        }

type section_static_material =
  | Static_absent
  | Static_partial
  | Static_complete of string * string * string option

let section_static_material section =
  let access_key_id = Ini.get section "aws_access_key_id" in
  let secret_access_key = Ini.get section "aws_secret_access_key" in
  let session_token = Ini.get section "aws_session_token" in
  match (access_key_id, secret_access_key, session_token) with
  | None, None, None -> Static_absent
  | Some access_key_id, Some secret_access_key, session_token ->
      Static_complete (access_key_id, secret_access_key, session_token)
  | _ -> Static_partial

let partial_profile_error profile =
  Error
    (Awskit.Error.Producer.validation ~field:"AWS_PROFILE"
       (Fmt.str "AWS profile %S contains partial static credentials" profile))

let profile_static_credentials ~profile ~credentials_file ~config_file sections
    =
  match
    Option.value_map sections.credentials_section ~default:Static_absent
      ~f:section_static_material
  with
  | Static_complete (access_key_id, secret_access_key, session_token) ->
      Ok
        (Some
           ( access_key_id,
             secret_access_key,
             session_token,
             `Shared_file credentials_file ))
  | Static_partial -> partial_profile_error profile
  | Static_absent -> (
      match
        Option.value_map sections.config_section ~default:Static_absent
          ~f:section_static_material
      with
      | Static_complete (access_key_id, secret_access_key, session_token) ->
          Ok
            (Some
               ( access_key_id,
                 secret_access_key,
                 session_token,
                 `Config_file config_file ))
      | Static_partial -> partial_profile_error profile
      | Static_absent -> Ok None)

let from_profile ?getenv ?home ?credentials_file ?config_file ?profile () =
  let open Env.Let_syntax in
  let* profile =
    match profile with
    | Some profile -> Ok profile
    | None -> default_profile ?getenv ()
  in
  let* credentials_file =
    match credentials_file with
    | Some path -> Ok path
    | None -> default_credentials_file ?getenv ?home ()
  in
  let* config_file =
    match config_file with
    | Some path -> Ok path
    | None -> default_config_file ?getenv ?home ()
  in
  let* credentials_contents = read_file credentials_file in
  let* config_contents = read_file config_file in
  let credentials_ini =
    Option.value_map credentials_contents ~default:[] ~f:Ini.parse
  in
  let config_ini = Option.value_map config_contents ~default:[] ~f:Ini.parse in
  match profile_sections ~profile ~credentials_ini ~config_ini with
  | None ->
      Error
        (Awskit.Error.Producer.validation ~field:"AWS_PROFILE"
           (Fmt.str "AWS profile %S not found" profile))
  | Some sections -> (
      let section = sections.merged in
      match
        profile_static_credentials ~profile ~credentials_file ~config_file
          sections
      with
      | Error _ as error -> error
      | Ok (Some (access_key_id, secret_access_key, session_token, source)) ->
          Awskit.Credentials.create ~access_key_id ~secret_access_key
            ?session_token ~source ()
      | Ok None -> (
          match
            (Ini.get section "role_arn", Ini.get section "source_profile")
          with
          | Some _, _ | _, Some _ ->
              Error
                (Awskit.Error.Producer.validation ~field:"AWS_PROFILE"
                   (Fmt.str
                      "AWS profile %S requires assume-role support, which is \
                       not implemented yet"
                      profile))
          | None, None ->
              Error
                (Awskit.Error.Producer.validation ~field:"AWS_PROFILE"
                   (Fmt.str "AWS profile %S does not contain static credentials"
                      profile))))

let env_has_credential_configuration ?(getenv = Env.getenv_opt) () =
  Option.is_some (getenv "AWS_ACCESS_KEY_ID")
  || Option.is_some (getenv "AWS_SECRET_ACCESS_KEY")
  || Option.is_some (getenv "AWS_SESSION_TOKEN")

let env_has ?(getenv = Env.getenv_opt) name = Option.is_some (getenv name)

let provider_resolution_to_result resolution =
  let open Awskit.Credentials.Provider in
  match resolution with
  | Resolved credentials -> Ok credentials
  | Unavailable { source; reason } ->
      Error
        (Awskit.Error.Producer.credentials
           ~source:(Awskit.Credentials.Provider.source_label source)
           reason)
  | Invalid error | Failed error -> Error error

let result_to_provider_resolution result =
  let open Awskit.Credentials.Provider in
  match result with
  | Ok credentials -> Resolved credentials
  | Error error -> Invalid error

let env_provider_resolution ?getenv () =
  let open Awskit.Credentials.Provider in
  if env_has_credential_configuration ?getenv () then
    from_env ?getenv () |> result_to_provider_resolution
  else
    Unavailable
      {
        source = `Env;
        reason =
          "AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, and AWS_SESSION_TOKEN not \
           configured";
      }

let explicit_profile_configuration ?(getenv = Env.getenv_opt) () =
  env_has ~getenv "AWS_PROFILE"
  || env_has ~getenv "AWS_SHARED_CREDENTIALS_FILE"
  || env_has ~getenv "AWS_CONFIG_FILE"

let file_exists path = Stdlib.Sys.file_exists path

let default_profile_file_exists ?getenv ?home () =
  let credentials_file = default_credentials_file ?getenv ?home () in
  let config_file = default_config_file ?getenv ?home () in
  match (credentials_file, config_file) with
  | Ok credentials_file, Ok config_file ->
      file_exists credentials_file || file_exists config_file
  | Ok path, Error _ | Error _, Ok path -> file_exists path
  | Error _, Error _ -> false

let profile_provider_resolution ?(getenv = Env.getenv_opt) ?home () =
  let open Awskit.Credentials.Provider in
  if
    explicit_profile_configuration ~getenv ()
    || default_profile_file_exists ~getenv ?home ()
  then
    match from_profile ~getenv ?home () with
    | Ok credentials -> Resolved credentials
    | Error error -> Invalid error
  else
    Unavailable
      { source = `Shared_file "default"; reason = "profile not configured" }

let default_provider ?getenv ?home () =
  Awskit.Credentials.Provider.chain
    [
      Awskit.Credentials.Provider.create (fun () ->
          env_provider_resolution ?getenv ());
      Awskit.Credentials.Provider.create (fun () ->
          profile_provider_resolution ?getenv ?home ());
    ]

let default_chain ?getenv ?home () =
  Awskit.Credentials.Provider.resolve (default_provider ?getenv ?home ())
  |> provider_resolution_to_result
