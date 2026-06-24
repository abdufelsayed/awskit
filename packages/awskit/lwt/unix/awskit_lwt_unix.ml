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
  let metadata_max_response_bytes = 1 * 1024 * 1024
  let container_host = "169.254.170.2"
  let eks_pod_identity_host = "169.254.170.23"
  let imds_host = "169.254.169.254"
  let refresh_before = Ptime.Span.of_int_s (5 * 60)

  let validation ~field message =
    Awskit.Error.Producer.validation ~field message

  let trim value = String.trim value

  let metadata_response_too_large () =
    Awskit.Error.Producer.body
      ~limit:(Int64.of_int metadata_max_response_bytes)
      (Printf.sprintf "credential metadata response exceeded %d bytes"
         metadata_max_response_bytes)

  let read_metadata_body body =
    let stream = Cohttp_lwt.Body.to_stream body in
    let buffer = Buffer.create 256 in
    let rec loop remaining =
      Lwt.bind (Lwt_stream.get stream) (function
        | None -> Lwt.return_ok (Buffer.contents buffer)
        | Some chunk ->
            let length = String.length chunk in
            if length > remaining then
              Lwt.return_error (metadata_response_too_large ())
            else (
              Buffer.add_string buffer chunk;
              loop (remaining - length)))
    in
    loop metadata_max_response_bytes

  let metadata_content_length_error response =
    match
      Cohttp.Header.get (Cohttp.Response.headers response) "content-length"
    with
    | None -> None
    | Some value -> (
        match int_of_string_opt value with
        | Some content_length when content_length > metadata_max_response_bytes
          ->
            Some (metadata_response_too_large ())
        | Some _ | None -> None)

  let http_call ~meth ~headers uri =
    let headers = Cohttp.Header.of_list headers in
    Lwt.catch
      (fun () ->
        Lwt_unix.with_timeout metadata_timeout_s (fun () ->
            Lwt.bind (Cohttp_lwt_unix.Client.call ~headers meth uri)
              (fun (response, body) ->
                match metadata_content_length_error response with
                | Some error -> Lwt.return_error error
                | None ->
                    Lwt.bind (read_metadata_body body) (function
                      | Error _ as error -> Lwt.return error
                      | Ok body ->
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
        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
        | Lwt_unix.Timeout ->
            Lwt.return_error
              (Awskit.Error.Producer.timeout ~operation:"credential metadata"
                 "credential metadata request timed out")
        | exn ->
            Lwt.return_error
              (Awskit.Error.Producer.transport ~retryable:true
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
        | Ok (time, _, _) -> Ok time
        | Error _ ->
            Error
              (validation ~field "metadata response contains invalid Expiration")
        )
    | `Null -> Error (validation ~field "metadata response missing Expiration")
    | _ -> Error (validation ~field "metadata response missing Expiration")

  let parse_metadata_credentials ~source ~field body =
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
              ~session_token ~source ~expires_at ()
          in
          Ok (credentials, Some expires_at)
    with Yojson.Json_error message ->
      Error
        (validation ~field
           (Printf.sprintf "metadata response is not valid JSON: %s" message))

  type metadata_fetch =
    | Fetch_resolved of Awskit.Credentials.t * Ptime.t option
    | Fetch_unavailable of string
    | Fetch_invalid of Awskit.Error.t
    | Fetch_failed of Awskit.Error.t

  type 'a metadata_lookup =
    | Lookup_resolved of 'a
    | Lookup_invalid of Awskit.Error.t
    | Lookup_failed of Awskit.Error.t

  let needs_refresh ~now = function
    | None -> true
    | Some (_, None) -> false
    | Some (_, Some expires_at) -> (
        match Ptime.add_span now refresh_before with
        | None -> true
        | Some refresh_at -> Ptime.compare refresh_at expires_at >= 0)

  let cached ~source ~clock fetch =
    let cache = ref None in
    Provider.create (fun () ->
        let open Provider in
        if not (needs_refresh ~now:(clock ()) !cache) then
          match !cache with
          | Some (credentials, _) -> Lwt.return (Resolved credentials)
          | None -> assert false
        else
          Lwt.bind (fetch ()) (function
            | Fetch_unavailable reason ->
                Lwt.return (Unavailable { source; reason })
            | Fetch_invalid error -> Lwt.return (Invalid error)
            | Fetch_failed error -> Lwt.return (Failed error)
            | Fetch_resolved (credentials, expires_at) ->
                cache := Some (credentials, expires_at);
                Lwt.return (Resolved credentials)))

  let metadata_result_to_fetch = function
    | Ok (credentials, expires_at) -> Fetch_resolved (credentials, expires_at)
    | Error error -> Fetch_invalid error

  let metadata_lookup_to_fetch = function
    | Lookup_resolved (credentials, expires_at) ->
        Fetch_resolved (credentials, expires_at)
    | Lookup_invalid error -> Fetch_invalid error
    | Lookup_failed error -> Fetch_failed error

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

  type container_endpoint =
    | Container_endpoint of Uri.t
    | Container_endpoint_unavailable of string
    | Container_endpoint_invalid of Awskit.Error.t

  let container_endpoint ?(getenv = getenv_opt) () =
    match getenv "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI" with
    | Some path when not (String.equal path "") ->
        if String.length path > 0 && Char.equal path.[0] '/' then
          Container_endpoint
            (Uri.of_string (Printf.sprintf "http://%s%s" container_host path))
        else
          Container_endpoint_invalid
            (validation ~field:"AWS_CONTAINER_CREDENTIALS_RELATIVE_URI"
               "container credential relative URI must start with /")
    | _ -> (
        match getenv "AWS_CONTAINER_CREDENTIALS_FULL_URI" with
        | Some uri when not (String.equal uri "") -> (
            match validate_container_full_uri (Uri.of_string uri) with
            | Ok uri -> Container_endpoint uri
            | Error error -> Container_endpoint_invalid error)
        | _ ->
            Container_endpoint_unavailable
              "container credential endpoint not configured")

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
    cached ~source:`Container ~clock (fun () ->
        match container_endpoint ?getenv () with
        | Container_endpoint_unavailable reason ->
            Lwt.return (Fetch_unavailable reason)
        | Container_endpoint_invalid error -> Lwt.return (Fetch_invalid error)
        | Container_endpoint uri ->
            Lwt.bind (container_authorization_headers ?getenv ()) (function
              | Error error -> Lwt.return (Fetch_failed error)
              | Ok headers ->
                  Lwt.bind (http_call ~meth:`GET ~headers uri) (function
                    | Error error -> Lwt.return (Fetch_failed error)
                    | Ok response -> (
                        match
                          expect_success ~field:"container credentials" response
                        with
                        | Error error -> Lwt.return (Fetch_failed error)
                        | Ok response ->
                            Lwt.return
                              (parse_metadata_credentials ~source:`Container
                                 ~field:"container credentials" response.body
                              |> metadata_result_to_fetch)))))

  let imds_base_uri path =
    Uri.of_string (Printf.sprintf "http://%s/latest/%s" imds_host path)

  let imds_token ?(http_call = http_call) () =
    let headers = [ ("X-aws-ec2-metadata-token-ttl-seconds", "21600") ] in
    Lwt.bind
      (http_call ~meth:`PUT ~headers (imds_base_uri "api/token"))
      (function
        | Ok response when response.status >= 200 && response.status < 300 ->
            Lwt.return (Lookup_resolved (Some response.body))
        | Ok response
          when response.status = 403
               || response.status = 404
               || response.status = 405 ->
            Lwt.return (Lookup_resolved None)
        | Ok response ->
            Lwt.return
              (Lookup_failed
                 (validation ~field:"instance metadata token"
                    (Printf.sprintf "metadata token service returned HTTP %d"
                       response.status)))
        | Error error -> Lwt.return (Lookup_failed error))

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
      | Error error -> Lwt.return (Lookup_failed error)
      | Ok response -> (
          match expect_success ~field:"instance metadata role" response with
          | Error error -> Lwt.return (Lookup_failed error)
          | Ok response -> (
              match first_non_empty_line response.body with
              | Some role -> Lwt.return (Lookup_resolved role)
              | None ->
                  Lwt.return
                    (Lookup_invalid
                       (validation ~field:"instance metadata role"
                          "metadata response did not include an IAM role")))))

  let fetch_imds_credentials ~http_call ?token role =
    Lwt.bind
      (imds_get ~http_call ?token
         ("meta-data/iam/security-credentials/" ^ Uri.pct_encode role))
      (function
        | Error error -> Lwt.return (Lookup_failed error)
        | Ok response -> (
            match
              expect_success ~field:"instance metadata credentials" response
            with
            | Error error -> Lwt.return (Lookup_failed error)
            | Ok response ->
                Lwt.return
                  (match
                     parse_metadata_credentials ~source:`Imds
                       ~field:"instance metadata credentials" response.body
                   with
                  | Ok credentials -> Lookup_resolved credentials
                  | Error error -> Lookup_invalid error)))

  let instance_metadata_provider ?(getenv = getenv_opt) ?(http_call = http_call)
      ?(clock = Ptime_clock.now) ?imdsv1_fallback () =
    cached ~source:`Imds ~clock (fun () ->
        match getenv "AWS_EC2_METADATA_DISABLED" with
        | Some value when truthy (trim value) ->
            Lwt.return
              (Fetch_unavailable
                 "EC2 instance metadata credentials are disabled")
        | _ ->
            Lwt.bind (imds_token ~http_call ()) (function
              | Lookup_invalid error -> Lwt.return (Fetch_invalid error)
              | Lookup_failed error -> Lwt.return (Fetch_failed error)
              | Lookup_resolved token -> (
                  let fetch ?token () =
                    Lwt.bind (fetch_imds_role ~http_call ?token ()) (function
                      | Lookup_resolved role ->
                          fetch_imds_credentials ~http_call ?token role
                      | Lookup_invalid _ as invalid -> Lwt.return invalid
                      | Lookup_failed _ as failed -> Lwt.return failed)
                  in
                  match token with
                  | Some token ->
                      Lwt.map metadata_lookup_to_fetch (fetch ~token ())
                  | None -> (
                      match
                        imdsv1_fallback_policy ~getenv ?imdsv1_fallback ()
                      with
                      | `Enabled -> Lwt.map metadata_lookup_to_fetch (fetch ())
                      | `Disabled ->
                          Lwt.return
                            (Fetch_invalid
                               (validation ~field:"AWS_EC2_METADATA_V1_DISABLED"
                                  "IMDSv1 fallback is disabled"))))))

  let provider_source_to_lwt = function
    | `Static -> `Static
    | `Env -> `Env
    | `Shared_file path -> `Shared_file path
    | `Config_file path -> `Config_file path
    | `Container -> `Container
    | `Imds -> `Imds
    | `Custom source -> `Custom source

  let core_provider_resolution_to_lwt resolution =
    let open Provider in
    match resolution with
    | Awskit.Credentials.Provider.Resolved credentials -> Resolved credentials
    | Awskit.Credentials.Provider.Unavailable { source; reason } ->
        Unavailable { source = provider_source_to_lwt source; reason }
    | Awskit.Credentials.Provider.Invalid error -> Invalid error
    | Awskit.Credentials.Provider.Failed error -> Failed error

  let local_provider ?getenv ?home () =
    let provider = Awskit_unix.Credentials.default_provider ?getenv ?home () in
    Provider.create (fun () ->
        Lwt.return
          (Awskit.Credentials.Provider.resolve provider
          |> core_provider_resolution_to_lwt))

  let default_provider ?getenv ?home ?http_call ?clock ?imdsv1_fallback () =
    Provider.chain
      [
        local_provider ?getenv ?home ();
        container_provider ?getenv ?http_call ?clock ();
        instance_metadata_provider ?getenv ?http_call ?clock ?imdsv1_fallback ();
      ]
end

let create ?ctx ?endpoint ?region ?credentials ?(clock = Ptime_clock.now)
    ?retry_policy ?random_float ?timeout_policy ?max_response_drain_bytes
    ?imdsv1_fallback () =
  match
    ( (match region with
      | Some region -> Awskit.Region.of_string region
      | None -> Awskit_unix.Region.from_env ()),
      (match endpoint with
      | Some endpoint -> (
          match Awskit.Endpoint.of_string endpoint with
          | Ok endpoint -> Ok (Some endpoint)
          | Error _ as error -> error)
      | None -> Ok None),
      match credentials with
      | Some credentials ->
          Ok (Awskit_lwt.Credentials.Provider.static credentials)
      | None -> Ok (Credentials.default_provider ~clock ?imdsv1_fallback ()) )
  with
  | Ok region, Ok endpoint, Ok credentials_provider ->
      let sleep span = Lwt_unix.sleep (Ptime.Span.to_float_s span) in
      let random_float =
        match random_float with
        | Some random_float -> random_float
        | None ->
            let state = Random.State.make_self_init () in
            fun ~upper_bound -> Random.State.float state upper_bound
      in
      let region = Awskit.Region.to_string region in
      let endpoint =
        match endpoint with
        | None -> None
        | Some endpoint -> Some (Awskit.Endpoint.to_url_prefix endpoint)
      in
      Strict.create_with_credentials_provider ?ctx ?endpoint ~region
        ~credentials_provider ~clock ?retry_policy ~sleep ~random_float
        ?timeout_policy ?max_response_drain_bytes ()
  | Error error, _, _ | _, Error error, _ | _, _, Error error -> Error error
