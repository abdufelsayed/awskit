(** Private S3 execution kernel. This functor owns request construction,
    signing, physical transport, retry transitions, body cleanup, and logical
    session accounting. It is composed by the public [Awskit_s3] facade and is
    never part of the installed interface. *)

open Base

module Make
    (R : Execution_runtime.S)
    (Observer :
      Awskit.Observability.For_service.Observer
        with type 'a io = 'a R.t
         and type connection = R.connection) =
struct
  module Streaming = Execution_streaming.Make (R)
  module Body = Streaming.Body
  module Reader = Streaming.Reader
  module Observed_operation = Observation_operation.Make (Observer)
  module Observed_attempt = Observation_attempt.Make (Observer)
  module Observed_signing = Observation_signing.Make (Observer)

  module Context = struct
    module R = R

    type connection = R.connection
    type 'a io = 'a R.t
    type request_body = R.request_body
    type response_body_reader = R.response_body_reader
    type session = Execution_session.t

    let bind = R.IO.bind
    let ( let* ) = R.IO.bind
    let return = R.IO.return
    let return_ok value = R.IO.return (Ok value)
    let return_error error = R.IO.return (Error error)
    let empty_hash = Awskit.Body.Payload_hash.sha256_of_string ""
    let endpoint_config conn = R.S3_endpoint.s3_endpoint_config conn
    let region conn = R.Endpoint.region conn
    let now conn = R.Clock.now conn
    let credentials conn = R.Credentials.resolve conn

    let object_request conn ~bucket ~key =
      match Bucket_name.of_string bucket with
      | Error _ as error -> error
      | Ok bucket -> (
          match Object_key.of_string key with
          | Error _ as error -> error
          | Ok key ->
              Endpoint_resolver.resolve_object_request (endpoint_config conn)
                ~region:(region conn) ~bucket ~key)

    let bucket_request conn ~bucket ~suffix ~signing_suffix =
      match Bucket_name.of_string bucket with
      | Error _ as error -> error
      | Ok bucket ->
          Endpoint_resolver.resolve_bucket_request (endpoint_config conn)
            ~region:(region conn) ~bucket ~suffix ~signing_suffix

    let root_request conn =
      match
        Endpoint_resolver.endpoint (endpoint_config conn) ~region:(region conn)
      with
      | Error _ as error -> error
      | Ok endpoint ->
          Ok
            {
              Endpoint_resolver.Request.endpoint;
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

    let signed_request conn ~operation ~method_
        ~(request : Endpoint_resolver.Request.t) ~query ~headers ~payload_hash =
      let headers =
        ("host", Awskit.Endpoint.authority request.endpoint) :: headers
      in
      let* credentials = credentials conn in
      match credentials with
      | Error error -> return_error error
      | Ok credentials -> (
          let* signed =
            Observed_signing.with_signing conn ~operation (fun () ->
                return
                  (Awskit.Signing.sign_request_params ~credentials
                     ~region:request.signing_region ~service:"s3" ~method_
                     ~path:request.signing_path ~query_params:query ~headers
                     ~payload_hash ~now:(now conn)))
          in
          match signed with
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

    let retry_event conn ~operation ~decision ~attempt ~budget_state ~replayable
        ?delay error =
      let retry =
        Observation_attempt.retry ~operation ~attempt ~decision
          ~retry_class:(Awskit.Error.retry_class error)
          ~replayable ?delay
          ~remaining_budget:(Awskit.Retry.available_capacity budget_state)
          ()
      in
      Observed_attempt.emit_retry conn retry

    let retry_or_error conn ~operation ~attempt ~budget_state ~replayable error
        : Execution_retry.decision =
      let policy = R.Retry.policy conn in
      let max_attempts = Awskit.Retry.max_attempts policy in
      match
        Awskit.Retry.delay policy ~attempt ~error
          ~random_float:(R.Random.float conn)
      with
      | Some delay when replayable -> (
          match Awskit.Retry.charge_retry policy budget_state error with
          | None ->
              Execution_retry.Stop
                {
                  error =
                    Awskit.Error.Producer.with_retry ~attempt ~max_attempts
                      ~reason:"retry budget exhausted" error;
                  event_error = error;
                  decision = Execution_retry.Budget_exhausted;
                }
          | Some budget_state ->
              Execution_retry.Retry_after { budget_state; delay; error })
      | Some _delay ->
          Execution_retry.Stop
            {
              error =
                Awskit.Error.Producer.with_retry ~attempt ~max_attempts
                  ~reason:"not retried because request body is not replayable"
                  error;
              event_error = error;
              decision = Execution_retry.Non_replayable_request;
            }
      | None when attempt >= max_attempts ->
          Execution_retry.Stop
            {
              error =
                Awskit.Error.Producer.with_retry ~attempt ~max_attempts
                  ~reason:"retry attempts exhausted" error;
              event_error = error;
              decision = Execution_retry.Attempts_exhausted;
            }
      | None ->
          Execution_retry.Stop
            {
              error =
                Awskit.Error.Producer.with_retry ~attempt ~max_attempts
                  ~reason:"error is not retryable by policy" error;
              event_error = error;
              decision = Execution_retry.Policy_denied;
            }

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

    let with_operation conn ~operation ?bucket callback =
      let session = Execution_session.create operation in
      let region = Awskit.Region.to_string (region conn) in
      Observed_operation.with_operation conn ~operation ~region:(Some region)
        ~bucket
        ~attempts:(fun () -> Execution_session.attempts session)
        ~logical_request_bytes:(fun () ->
          Execution_session.logical_request_bytes session)
        ~logical_response_bytes:(fun () ->
          Execution_session.logical_response_bytes session)
        (fun () ->
          let* result = callback session in
          (match result with
          | Ok _ -> Execution_session.commit_logical_response_bytes session
          | Error _ -> ());
          return result)

    let retry_decision conn ~operation ~attempt ~budget_state ~replayable
        decision =
      match decision with
      | Execution_retry.Stop { error; event_error; decision } ->
          retry_event conn ~operation ~decision ~attempt ~budget_state
            ~replayable event_error;
          Execution_attempt.Complete (Error error)
      | Execution_retry.Retry_after { budget_state; delay; error } ->
          retry_event conn ~operation ~decision:Execution_retry.Scheduled
            ~attempt ~budget_state ~replayable ~delay error;
          Execution_attempt.Retry_after { budget_state; delay; error }

    let with_response_action_in_session conn ~session ~logical_response_body
        ~method_ ~request ~query ~headers ~payload_hash body
        ~(success_action :
           Awskit.Response.t ->
           R.response_body ->
           _ Execution_attempt.response R.t) =
      let operation = Execution_session.operation session in
      let replayable = (R.Request_body.descriptor body).replayable in
      Execution_session.set_logical_request_bytes session
        (R.Request_body.content_length body);
      let policy = R.Retry.policy conn in
      let initial_budget_state = Awskit.Retry.initial_budget_state policy in
      let rec attempt budget_state attempt_number =
        Execution_session.record_attempt session;
        let* result =
          Observed_attempt.with_attempt conn ~operation ~number:attempt_number
            ~replayable (fun () ->
              let* request =
                signed_request conn ~operation ~method_ ~request ~query ~headers
                  ~payload_hash
              in
              match request with
              | Error error -> return (Execution_attempt.Complete (Error error))
              | Ok request -> (
                  let* response =
                    R.Transport.with_response conn request ~body
                      ~consume:(fun response response_body ->
                        let* action =
                          if Awskit.Response.is_success response then
                            success_action response response_body
                          else
                            let* body =
                              read_response_body response_body
                                ~max_size:1_048_576L
                            in
                            match body with
                            | Error error ->
                                return (Execution_attempt.Success (Error error))
                            | Ok body ->
                                let error =
                                  service_error response (Some body)
                                in
                                return (Execution_attempt.Retry error)
                        in
                        let consumed =
                          R.Response_body.consumed_bytes response_body
                        in
                        return_ok (action, consumed))
                  in
                  match response with
                  | Error error ->
                      return
                        (retry_decision conn ~operation ~attempt:attempt_number
                           ~budget_state ~replayable
                           (retry_or_error conn ~operation
                              ~attempt:attempt_number ~budget_state ~replayable
                              error))
                  | Ok (Execution_attempt.Success result, consumed) ->
                      (if Result.is_ok result then
                         match logical_response_body with
                         | `Caller_visible ->
                             Execution_session.set_logical_response_bytes
                               session (Some consumed)
                         | `Discarded -> ());
                      return (Execution_attempt.Complete result)
                  | Ok (Execution_attempt.Retry error, _) ->
                      return
                        (retry_decision conn ~operation ~attempt:attempt_number
                           ~budget_state ~replayable
                           (retry_or_error conn ~operation
                              ~attempt:attempt_number ~budget_state ~replayable
                              error))))
        in
        match result with
        | Execution_attempt.Complete result -> return result
        | Execution_attempt.Retry_after { budget_state; delay; error = _ } ->
            let* () = R.Sleeper.sleep conn delay in
            attempt budget_state (attempt_number + 1)
      in
      attempt initial_budget_state 1

    let with_response_in_session conn ~session ~method_ ~request ~query ~headers
        ~payload_hash body ~f =
      with_response_action_in_session conn ~session ~method_ ~request ~query
        ~headers ~payload_hash body ~logical_response_body:`Caller_visible
        ~success_action:(fun response body ->
          let* result = f response body in
          return (Execution_attempt.Success result))

    let with_discarded_response_in_session conn ~session ~method_ ~request
        ~query ~headers ~payload_hash body ~f =
      with_response_action_in_session conn ~session ~method_ ~request ~query
        ~headers ~payload_hash body ~logical_response_body:`Discarded
        ~success_action:(fun response body ->
          let* result = f response body in
          return (Execution_attempt.Success result))

    let with_empty_response_in_session conn ~session ~method_ ~request ~query
        ~headers ~f =
      with_response_in_session conn ~session ~method_ ~request ~query ~headers
        ~payload_hash:empty_hash R.Request_body.empty ~f

    let with_empty_discarded_response_in_session conn ~session ~method_ ~request
        ~query ~headers ~f =
      with_discarded_response_in_session conn ~session ~method_ ~request ~query
        ~headers ~payload_hash:empty_hash R.Request_body.empty ~f

    let with_retryable_embedded_response_in_session conn ~session ~method_
        ~request ~query ~headers ~payload_hash body ~f =
      with_response_action_in_session conn ~session ~method_ ~request ~query
        ~headers ~payload_hash body ~logical_response_body:`Caller_visible
        ~success_action:(fun response response_body ->
          let* result = f response response_body in
          match result with
          | Error error when retryable_service_error error ->
              return (Execution_attempt.Retry error)
          | Ok _ | Error _ -> return (Execution_attempt.Success result))

    let content_md5 body =
      Digestif.MD5.(digest_string body |> to_raw_string) |> Base64.encode_exn
  end

  module Multipart = Multipart_request.Make (Context)
  module Object = Object_request.Make (Context)
  module Bucket = Bucket_request.Make (Context)
  module Presigned = Presigned_request.Make (Context) (Observer)
end
