module Strict = Awskit_lwt.Make (Cohttp_lwt_unix.Client)
include Strict

module Credentials = struct
  module Provider = Awskit_lwt.Credentials.Provider

  type http_response = {
    status : int;
    headers : (string * string) list;
    body : string;
  }

  type http_call =
    meth:Cohttp.Code.meth ->
    headers:(string * string) list ->
    Uri.t ->
    (http_response, Awskit.Error.t) result Lwt.t

  type imdsv1_fallback = [ `Enabled | `Disabled ]

  module Result_syntax = struct
    let ( let* ) = Result.bind
  end

  let metadata_timeout_s = 1.0
  let container_host = "169.254.170.2"
  let eks_pod_identity_host = "169.254.170.23"
  let imds_host = "169.254.169.254"
  let refresh_before = Ptime.Span.of_int_s (5 * 60)

  let validation ~field message =
    Awskit.Error.Internal.validation ~field message

  let trim value = String.trim value

  let http_call ~meth ~headers uri =
    let headers = Cohttp.Header.of_list headers in
    Lwt.catch
      (fun () ->
        Lwt_unix.with_timeout metadata_timeout_s (fun () ->
            Lwt.bind (Cohttp_lwt_unix.Client.call ~headers meth uri)
              (fun (response, body) ->
                Lwt.bind (Cohttp_lwt.Body.to_string body) (fun body ->
                    Lwt.return_ok
                      {
                        status =
                          Cohttp.Response.status response
                          |> Cohttp.Code.code_of_status;
                        headers =
                          Cohttp.Response.headers response
                          |> Cohttp.Header.to_list;
                        body;
                      }))))
      (function
        | Lwt_unix.Timeout ->
            Lwt.return_error
              (Awskit.Error.Internal.transport ~retryable:true
                 "credential metadata request timed out")
        | exn ->
            Lwt.return_error
              (Awskit.Error.Internal.transport ~retryable:true
                 (Printexc.to_string exn)))

  let expect_success ~field response =
    if response.status >= 200 && response.status < 300 then Ok response
    else
      Error
        (validation ~field
           (Printf.sprintf "metadata service returned HTTP %d" response.status))

  let json_member_string ~field json name =
    match Yojson.Basic.Util.member name json with
    | `String value when not (String.equal value "") -> Ok value
    | _ ->
        Error
          (validation ~field
             (Printf.sprintf "metadata response missing %s" name))

  let parse_expiration ~field json =
    match Yojson.Basic.Util.member "Expiration" json with
    | `String value -> (
        match Ptime.of_rfc3339 ~strict:false value with
        | Ok (time, _, _) -> Ok (Some time)
        | Error _ ->
            Error
              (validation ~field "metadata response contains invalid Expiration")
        )
    | `Null -> Ok None
    | _ -> Ok None

  let parse_metadata_credentials ~field body =
    try
      let json = Yojson.Basic.from_string body in
      let code =
        match Yojson.Basic.Util.member "Code" json with
        | `String value -> Some value
        | _ -> None
      in
      match code with
      | Some code when not (String.equal code "Success") ->
          Error
            (validation ~field
               (Printf.sprintf "metadata service returned Code=%s" code))
      | _ ->
          let open Result_syntax in
          let* access_key_id = json_member_string ~field json "AccessKeyId" in
          let* secret_access_key =
            json_member_string ~field json "SecretAccessKey"
          in
          let* session_token = json_member_string ~field json "Token" in
          let* expires_at = parse_expiration ~field json in
          let* credentials =
            Awskit.Credentials.create ~access_key_id ~secret_access_key
              ~session_token ()
          in
          Ok (credentials, expires_at)
    with Yojson.Json_error message ->
      Error
        (validation ~field
           (Printf.sprintf "metadata response is not valid JSON: %s" message))

  let needs_refresh ~now = function
    | None -> true
    | Some (_, None) -> false
    | Some (_, Some expires_at) -> (
        match Ptime.add_span now refresh_before with
        | None -> true
        | Some refresh_at -> Ptime.compare refresh_at expires_at >= 0)

  let cached ~clock fetch =
    let cache = ref None in
    Provider.create (fun () ->
        if not (needs_refresh ~now:(clock ()) !cache) then
          match !cache with
          | Some (credentials, _) -> Lwt.return_ok credentials
          | None -> assert false
        else
          Lwt.bind (fetch ()) (function
            | Error _ as error -> Lwt.return error
            | Ok (credentials, expires_at) ->
                cache := Some (credentials, expires_at);
                Lwt.return_ok credentials))

  let read_file path =
    Lwt.catch
      (fun () ->
        Lwt.bind (Lwt_io.with_file ~mode:Lwt_io.Input path Lwt_io.read)
          (fun contents -> Lwt.return_ok (trim contents)))
      (fun exn ->
        Lwt.return_error
          (validation ~field:path
             (Printf.sprintf "failed to read file: %s" (Printexc.to_string exn))))

  let getenv_opt name = Stdlib.Sys.getenv_opt name

  let is_loopback_host = function
    | "localhost" | "::1" -> true
    | host -> String.length host >= 4 && String.sub host 0 4 = "127."

  let is_allowed_http_container_host = function
    | None -> false
    | Some host ->
        is_loopback_host host
        || String.equal host container_host
        || String.equal host eks_pod_identity_host

  let validate_container_full_uri uri =
    match Uri.scheme uri with
    | Some "https" -> Ok uri
    | Some "http" when is_allowed_http_container_host (Uri.host uri) -> Ok uri
    | Some "http" ->
        Error
          (validation ~field:"AWS_CONTAINER_CREDENTIALS_FULL_URI"
             "plain HTTP container credential endpoints must use loopback or \
              AWS container metadata hosts")
    | _ ->
        Error
          (validation ~field:"AWS_CONTAINER_CREDENTIALS_FULL_URI"
             "container credential endpoint must use http or https")

  let container_endpoint ?(getenv = getenv_opt) () =
    match getenv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" with
    | Some path when not (String.equal path "") ->
        if String.length path > 0 && Char.equal path.[0] '/' then
          Ok (Uri.of_string (Printf.sprintf "http://%s%s" container_host path))
        else
          Error
            (validation ~field:"AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
               "container credential relative URI must start with /")
    | _ -> (
        match getenv "AWS_CONTAINER_CREDENTIALS_FULL_URI" with
        | Some uri when not (String.equal uri "") ->
            validate_container_full_uri (Uri.of_string uri)
        | _ ->
            Error
              (validation ~field:"AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
                 "container credential endpoint not configured"))

  let container_authorization_headers ?(getenv = getenv_opt) () =
    match getenv "AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE" with
    | Some path when not (String.equal path "") ->
        Lwt.bind (read_file path) (function
          | Ok token -> Lwt.return_ok [ ("Authorization", token) ]
          | Error _ as error -> Lwt.return error)
    | _ -> (
        match getenv "AWS_CONTAINER_AUTHORIZATION_TOKEN" with
        | Some token when not (String.equal token "") ->
            Lwt.return_ok [ ("Authorization", token) ]
        | _ -> Lwt.return_ok [])

  let container_provider ?getenv ?(http_call = http_call)
      ?(clock = Ptime_clock.now) () =
    cached ~clock (fun () ->
        match container_endpoint ?getenv () with
        | Error _ as error -> Lwt.return error
        | Ok uri ->
            Lwt.bind (container_authorization_headers ?getenv ()) (function
              | Error _ as error -> Lwt.return error
              | Ok headers ->
                  Lwt.bind (http_call ~meth:`GET ~headers uri) (function
                    | Error _ as error -> Lwt.return error
                    | Ok response -> (
                        match
                          expect_success ~field:"container credentials" response
                        with
                        | Error _ as error -> Lwt.return error
                        | Ok response ->
                            Lwt.return
                              (parse_metadata_credentials
                                 ~field:"container credentials" response.body)))))

  let imds_base_uri path =
    Uri.of_string (Printf.sprintf "http://%s/latest/%s" imds_host path)

  let imds_token ?(http_call = http_call) () =
    let headers = [ ("X-aws-ec2-metadata-token-ttl-seconds", "21600") ] in
    Lwt.bind
      (http_call ~meth:`PUT ~headers (imds_base_uri "api/token"))
      (function
        | Ok response when response.status >= 200 && response.status < 300 ->
            Lwt.return_ok (Some response.body)
        | Ok response
          when response.status = 403
               || response.status = 404
               || response.status = 405 ->
            Lwt.return_ok None
        | Ok response ->
            Lwt.return_error
              (validation ~field:"instance metadata token"
                 (Printf.sprintf "metadata token service returned HTTP %d"
                    response.status))
        | Error _ as error -> Lwt.return error)

  let imds_headers token =
    match token with
    | None -> []
    | Some token -> [ ("X-aws-ec2-metadata-token", token) ]

  let imds_get ?(http_call = http_call) ?token path =
    http_call ~meth:`GET ~headers:(imds_headers token) (imds_base_uri path)

  let truthy value = String.equal (String.lowercase_ascii value) "true"

  let imdsv1_fallback_from_env ?(getenv = getenv_opt) () =
    match getenv "AWS_EC2_METADATA_V1_DISABLED" with
    | Some value when truthy (trim value) -> `Disabled
    | _ -> `Enabled

  let imdsv1_fallback_policy ?imdsv1_fallback ?getenv () =
    match imdsv1_fallback with
    | Some policy -> policy
    | None -> imdsv1_fallback_from_env ?getenv ()

  let first_non_empty_line body =
    String.split_on_char '\n' body
    |> List.map trim
    |> List.find_opt (fun value -> not (String.equal value ""))

  let fetch_imds_role ~http_call ?token () =
    Lwt.bind (imds_get ~http_call ?token "meta-data/iam/security-credentials/")
      (function
      | Error _ as error -> Lwt.return error
      | Ok response -> (
          match expect_success ~field:"instance metadata role" response with
          | Error _ as error -> Lwt.return error
          | Ok response -> (
              match first_non_empty_line response.body with
              | Some role -> Lwt.return_ok role
              | None ->
                  Lwt.return_error
                    (validation ~field:"instance metadata role"
                       "metadata response did not include an IAM role"))))

  let fetch_imds_credentials ~http_call ?token role =
    Lwt.bind
      (imds_get ~http_call ?token
         ("meta-data/iam/security-credentials/" ^ Uri.pct_encode role))
      (function
        | Error _ as error -> Lwt.return error
        | Ok response -> (
            match
              expect_success ~field:"instance metadata credentials" response
            with
            | Error _ as error -> Lwt.return error
            | Ok response ->
                Lwt.return
                  (parse_metadata_credentials
                     ~field:"instance metadata credentials" response.body)))

  let instance_metadata_provider ?(getenv = getenv_opt) ?(http_call = http_call)
      ?(clock = Ptime_clock.now) ?imdsv1_fallback () =
    cached ~clock (fun () ->
        match getenv "AWS_EC2_METADATA_DISABLED" with
        | Some value when truthy (trim value) ->
            Lwt.return_error
              (validation ~field:"AWS_EC2_METADATA_DISABLED"
                 "EC2 instance metadata credentials are disabled")
        | _ ->
            Lwt.bind (imds_token ~http_call ()) (function
              | Error _ as error -> Lwt.return error
              | Ok token -> (
                  let fetch ?token () =
                    Lwt.bind (fetch_imds_role ~http_call ?token ()) (function
                      | Error _ as error -> Lwt.return error
                      | Ok role -> fetch_imds_credentials ~http_call ?token role)
                  in
                  match token with
                  | Some token -> fetch ~token ()
                  | None -> (
                      match
                        imdsv1_fallback_policy ~getenv ?imdsv1_fallback ()
                      with
                      | `Enabled -> fetch ()
                      | `Disabled ->
                          Lwt.return_error
                            (validation ~field:"AWS_EC2_METADATA_V1_DISABLED"
                               "IMDSv1 fallback is disabled")))))

  let local_provider ?getenv ?home () =
    let provider = Awskit_unix.Credentials.default_provider ?getenv ?home () in
    Provider.create (fun () ->
        Lwt.return (Awskit.Credentials.Provider.resolve provider))

  let default_provider ?getenv ?home ?http_call ?clock ?imdsv1_fallback () =
    Provider.chain
      [
        local_provider ?getenv ?home ();
        container_provider ?getenv ?http_call ?clock ();
        instance_metadata_provider ?getenv ?http_call ?clock ?imdsv1_fallback ();
      ]
end

let create ?ctx ?endpoint ?region ?credentials ?(clock = Ptime_clock.now)
    ?retry_policy ?max_response_drain_bytes ?imdsv1_fallback () =
  match
    ( (match region with
      | Some region -> Awskit.Region.of_string region
      | None -> Awskit_unix.Region.from_env ()),
      match credentials with
      | Some credentials ->
          Ok (Awskit_lwt.Credentials.Provider.static credentials)
      | None -> Ok (Credentials.default_provider ~clock ?imdsv1_fallback ()) )
  with
  | Ok region, Ok credentials_provider ->
      let sleep span = Lwt_unix.sleep (Ptime.Span.to_float_s span) in
      Ok
        (Strict.create_with_credentials_provider ?ctx ?endpoint ~region
           ~credentials_provider ~clock ?retry_policy ~sleep
           ?max_response_drain_bytes ())
  | Error error, _ | _, Error error -> Error error
