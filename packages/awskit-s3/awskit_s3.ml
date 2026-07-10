module Implementor = Implementor

module type S = Implementor.Client

module Credentials = Awskit.Credentials
module Endpoint = Awskit.Endpoint
module Region = Awskit.Region
module Error = S3_error
module Bucket_name = Bucket_name
module Object_key = Object_key
module Account_id = Account_id
module Content_type = Content_type
module Header_value = Header_value
module Metadata = Metadata
module Storage_class = Storage_class
module Tag = Tag
module Range = Range
module Encryption = Encryption
module Endpoint_config = Endpoint_config
module Object = Object
module Bucket = Bucket
module Multipart = Multipart
module Transfer = Transfer
module Policy = Policy
module Presigned = Presigned

module Make (R : Awskit.Runtime.S) = struct
  type runtime_connection = R.connection

  type t = {
    runtime_connection : R.connection;
    endpoint_config : Endpoint_config.t;
  }

  type 'a io = 'a R.t
  type request_body = R.request_body
  type response_body_reader = R.response_body_reader

  let create ?(endpoint_config = Endpoint_config.default) runtime_connection =
    { runtime_connection; endpoint_config }

  let runtime_connection client = client.runtime_connection

  module Streaming = Streaming.Make (R)
  module Body = Streaming.Body
  module Reader = Streaming.Reader

  module Context = struct
    module R = R

    type connection = t
    type 'a io = 'a R.t
    type request_body = R.request_body
    type response_body_reader = R.response_body_reader

    let bind = R.IO.bind
    let ( let* ) = R.IO.bind
    let return = R.IO.return
    let return_ok value = R.IO.return (Ok value)
    let return_error error = R.IO.return (Error error)
    let empty_hash = Awskit.Body.Payload_hash.sha256_of_string ""
    let runtime_connection client = client.runtime_connection
    let endpoint_config client = client.endpoint_config
    let region client = R.Endpoint.region (runtime_connection client)
    let now client = R.Clock.now (runtime_connection client)
    let credentials client = R.Credentials.resolve (runtime_connection client)

    let object_request conn ~bucket ~key =
      match Bucket_name.of_string bucket with
      | Error _ as error -> error
      | Ok bucket -> (
          match Object_key.of_string key with
          | Error _ as error -> error
          | Ok key ->
              Endpoint_config.Resolver.resolve_object_request
                (endpoint_config conn) ~region:(region conn) ~bucket ~key)

    let bucket_request conn ~bucket ~suffix ~signing_suffix =
      match Bucket_name.of_string bucket with
      | Error _ as error -> error
      | Ok bucket ->
          Endpoint_config.Resolver.resolve_bucket_request (endpoint_config conn)
            ~region:(region conn) ~bucket ~suffix ~signing_suffix

    let root_request conn =
      match
        Endpoint_config.endpoint (endpoint_config conn) ~region:(region conn)
      with
      | Error _ as error -> error
      | Ok endpoint ->
          Ok
            {
              Endpoint_config.Resolver.Request.endpoint;
              path = "/";
              signing_path = "/";
              signing_region =
                Endpoint_config.signing_region (endpoint_config conn)
                  ~client_region:(region conn);
              style = `Path;
            }

    let bounded_body_chunk_size = 8192

    let bounded_body_context error =
      Awskit.Error.Producer.with_context "reading bounded response body" error

    let with_bounded_body_context result =
      let* result = result in
      match result with
      | Ok _ as ok -> return ok
      | Error error -> return_error (bounded_body_context error)

    let read_body reader ~max_size =
      with_bounded_body_context
        (Reader.to_string ~chunk_size:bounded_body_chunk_size
           ~max_bytes:max_size reader)

    let read_body_bytes reader ~max_size =
      with_bounded_body_context
        (Reader.to_bytes ~chunk_size:bounded_body_chunk_size ~max_bytes:max_size
           reader)

    let read_response_body body ~max_size =
      R.Response_body.with_reader body ~consume:(read_body ~max_size)

    let discard_response_body = R.Response_body.discard

    let first_some first second =
      match first with Some _ -> first | None -> second

    let service_error response body =
      let error =
        match body with
        | None -> S3_xml.empty_service_error
        | Some body -> S3_xml.service_error body
      in
      Awskit.Error.Producer.service
        ~status:(Awskit.Response.status response)
        ?code:error.code ?message:error.message
        ?request_id:
          (first_some (Awskit.Response.request_id response) error.request_id)
        ?host_id:(first_some (Awskit.Response.host_id response) error.host_id)
        ~headers:(Awskit.Response.headers response)
        ?body ()

    let signed_request conn ~method_
        ~(request : Endpoint_config.Resolver.Request.t) ~query ~headers
        ~payload_hash =
      let headers =
        ("host", Awskit.Endpoint.authority request.endpoint) :: headers
      in
      let* credentials = credentials conn in
      match credentials with
      | Error error -> return_error error
      | Ok credentials -> (
          match
            Awskit.Signing.sign_request_params ~credentials
              ~region:request.signing_region ~service:"s3" ~method_
              ~path:request.signing_path ~query_params:query ~headers
              ~payload_hash ~now:(now conn)
          with
          | Error error -> return_error error
          | Ok signed -> (
              match
                Awskit.Request.Target.create
                  ~scheme:(Awskit.Endpoint.scheme request.endpoint)
                  ~host:(Awskit.Endpoint.host request.endpoint)
                  ?port:(Awskit.Endpoint.port request.endpoint)
                  ~path:request.path ~query ()
              with
              | Error error -> return_error error
              | Ok target -> (
                  match
                    Awskit.Request.create ~method_ ~target
                      ~headers:signed.headers ()
                  with
                  | Error error -> return_error error
                  | Ok request -> return_ok request)))

    let retry_or_error conn ~attempt ~budget_state ~replayable error retry =
      let policy = R.Retry.policy (runtime_connection conn) in
      let max_attempts = Awskit.Retry.max_attempts policy in
      match
        Awskit.Retry.delay policy ~attempt ~error
          ~random_float:(R.Random.float (runtime_connection conn))
      with
      | Some delay when replayable -> (
          match Awskit.Retry.charge_retry policy budget_state error with
          | None ->
              return_error
                (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
                   ~reason:"retry budget exhausted" error)
          | Some budget_state ->
              let* () = R.Sleeper.sleep (runtime_connection conn) delay in
              retry budget_state (attempt + 1))
      | Some _delay ->
          return_error
            (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
               ~reason:"not retried because request body is not replayable"
               error)
      | None when attempt >= max_attempts ->
          return_error
            (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
               ~reason:"retry attempts exhausted" error)
      | None ->
          return_error
            (Awskit.Error.Producer.with_retry ~attempt ~max_attempts
               ~reason:"error is not retryable by policy" error)

    type 'a response_action =
      | Done of ('a, Awskit.Error.t) result
      | Retry of Awskit.Error.t

    let retryable_service_error error =
      let open Awskit.Error in
      match kind error with
      | Service _ -> (
          match retry_class error with
          | Retryable | Throttled -> true
          | Auth | Conflict | Not_found | Fatal | Unknown -> false)
      | Validation _ | Credentials _ | Signing _ | Endpoint _ | Transport _
      | Timeout _ | Cancelled _ | Body _ | Decode _ | Retry_exhausted _
      | Not_supported _ | Multiple _ ->
          false

    let with_response_action conn ~method_ ~request ~query ~headers
        ~payload_hash body ~success_action =
      let replayable = (R.Request_body.descriptor body).replayable in
      let policy = R.Retry.policy (runtime_connection conn) in
      let initial_budget_state = Awskit.Retry.initial_budget_state policy in
      let rec attempt budget_state attempt_number =
        let* request =
          signed_request conn ~method_ ~request ~query ~headers ~payload_hash
        in
        match request with
        | Error error -> return_error error
        | Ok request -> (
            let* response =
              R.Transport.with_response (runtime_connection conn) request ~body
                ~consume:(fun response response_body ->
                  if Awskit.Response.is_success response then
                    success_action response response_body
                  else
                    let* body =
                      read_response_body response_body ~max_size:1_048_576L
                    in
                    match body with
                    | Error error -> return_ok (Done (Error error))
                    | Ok body ->
                        let error = service_error response (Some body) in
                        return_ok (Retry error))
            in
            match response with
            | Error error ->
                retry_or_error conn ~attempt:attempt_number ~budget_state
                  ~replayable error attempt
            | Ok (Done result) -> return result
            | Ok (Retry error) ->
                retry_or_error conn ~attempt:attempt_number ~budget_state
                  ~replayable error attempt)
      in
      attempt initial_budget_state 1

    let with_response conn ~method_ ~request ~query ~headers ~payload_hash body
        ~f =
      with_response_action conn ~method_ ~request ~query ~headers ~payload_hash
        body ~success_action:(fun response response_body ->
          let* result = f response response_body in
          return_ok (Done result))

    let with_retryable_embedded_response conn ~method_ ~request ~query ~headers
        ~payload_hash body ~f =
      with_response_action conn ~method_ ~request ~query ~headers ~payload_hash
        body ~success_action:(fun response response_body ->
          let* result = f response response_body in
          match result with
          | Error error when retryable_service_error error ->
              return_ok (Retry error)
          | Ok _ | Error _ -> return_ok (Done result))

    let with_empty_response conn ~method_ ~request ~query ~headers ~f =
      with_response conn ~method_ ~request ~query ~headers
        ~payload_hash:empty_hash R.Request_body.empty ~f

    let content_md5 body =
      Digestif.MD5.(digest_string body |> to_raw_string) |> Base64.encode_exn
  end

  module Multipart = Multipart_request.Make (Context)
  module Object = Object_request.Make (Context)
  module Bucket = Bucket_request.Make (Context)
  module Presigned = Presigned_request.Make (Context)
end
