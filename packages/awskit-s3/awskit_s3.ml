include Awskit_s3_intf
module Credentials = Awskit.Credentials
module Endpoint = Awskit.Endpoint
module Region = Awskit.Region
module Error = Common.Error
module Metadata = Metadata
module Storage_class = Storage_class
module Tag = Tag
module Range = Range
module Endpoint_resolver = Endpoint_resolver
module Object = Object
module Bucket = Bucket
module Multipart = Multipart
module Transfer = Transfer
module Policy = Policy
module Presigned = Presigned
module Put_object = Object.Put
module Get_object = Object.Get
module Head_object = Object.Head
module Delete_object = Object.Delete
module Delete_objects = Object.Delete_many
module Copy_object = Object.Copy
module List_objects_v2 = Object.List
module List_object_versions = Object.Versions
module Create_bucket = Bucket.Create
module Delete_bucket = Bucket.Delete
module Head_bucket = Bucket.Head
module List_buckets = Bucket.List_buckets
module Get_bucket_location = Bucket.Get_location
module Create_multipart_upload = Multipart.Create
module Upload_part = Multipart.Upload_part
module Complete_multipart_upload = Multipart.Complete
module Abort_multipart_upload = Multipart.Abort
module List_parts = Multipart.List_parts

type addressing_style = [ `Auto | `Path | `Virtual_hosted ]

type endpoint_variant =
  [ `Regional
  | `Dualstack
  | `Fips
  | `Fips_dualstack
  | `Accelerate
  | `Accelerate_dualstack ]

type endpoint_config = Endpoint_resolver.t

let endpoint_config ?addressing_style ?endpoint_variant ?scheme ?endpoint () =
  Endpoint_resolver.create ?addressing_style ?endpoint_variant ?scheme ?endpoint
    ()

let default_endpoint_config = Endpoint_resolver.default

module Make (R : RUNTIME) = struct
  type connection = R.connection
  type 'a io = 'a R.t
  type request_body = R.request_body
  type response_body_reader = R.response_body_reader

  module Runtime = R
  module Streaming = Streaming.Make (R)
  module Body = Streaming.Body
  module Reader = Streaming.Reader

  module Context = struct
    module R = R

    type connection = R.connection
    type 'a io = 'a R.t
    type request_body = R.request_body
    type response_body_reader = R.response_body_reader

    let bind = R.bind
    let ( let* ) = R.bind
    let return = R.return
    let return_ok value = R.return (Ok value)
    let return_error error = R.return (Error error)
    let empty_hash = Awskit.Body.Payload_hash.sha256_of_string ""
    let endpoint_config conn = R.s3_endpoint_config conn

    let object_request conn ~bucket ~key =
      Endpoint_resolver.resolve_object_request (endpoint_config conn)
        ~region:(R.region conn) ~bucket ~key

    let bucket_request conn ~bucket ~suffix ~signing_suffix =
      Endpoint_resolver.resolve_bucket_request (endpoint_config conn)
        ~region:(R.region conn) ~bucket ~suffix ~signing_suffix

    let root_request conn =
      match
        Endpoint_resolver.endpoint (endpoint_config conn)
          ~region:(R.region conn)
      with
      | Error _ as error -> error
      | Ok endpoint ->
          Ok
            {
              Endpoint_resolver.Request.endpoint;
              path = "/";
              signing_path = "/";
              style = `Path;
            }

    let read_body reader ~max_size =
      let buffer = Buffer.create 4096 in
      let chunk = Bytes.create 8192 in
      let rec loop total =
        let* read =
          R.Response_body.read reader chunk ~off:0 ~len:(Bytes.length chunk)
        in
        match read with
        | Error error -> return (Error error)
        | Ok 0 -> return_ok (Buffer.contents buffer)
        | Ok n ->
            let total = Int64.add total (Int64.of_int n) in
            if Int64.compare total max_size > 0 then
              return_error
                (Awskit.Error.Internal.body ~limit:max_size
                   "response body exceeded max_bytes"
                |> Awskit.Error.Internal.with_context
                     "reading bounded response body")
            else begin
              Buffer.add_subbytes buffer chunk 0 n;
              loop total
            end
      in
      loop 0L

    let read_response_body body ~max_size =
      R.Response_body.with_reader body ~consume:(read_body ~max_size)

    let discard_response_body = R.Response_body.discard

    let service_error response body =
      Awskit.Error.Internal.service
        ~status:(Awskit.Response.status response)
        ?code:(Option.bind body Common.Xml.service_code)
        ?message:(Option.bind body Common.Xml.service_message)
        ?request_id:(Awskit.Response.request_id response)
        ?host_id:(Awskit.Response.host_id response)
        ~headers:(Awskit.Response.headers response)
        ?body ()

    let signed_request conn ~method_ ~(request : Endpoint_resolver.Request.t)
        ~query ~headers ~payload_hash =
      let headers =
        ("host", Awskit.Endpoint.authority request.endpoint) :: headers
      in
      let* credentials = R.credentials conn in
      match credentials with
      | Error error -> return_error error
      | Ok credentials -> (
          match
            Awskit.Signing.sign_request_params ~credentials
              ~region:(R.region conn) ~service:"s3" ~method_
              ~path:request.signing_path ~query_params:query ~headers
              ~payload_hash ~now:(R.now conn)
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

    let retry_or_error conn ~attempt ~replayable error retry =
      let policy = R.retry_policy conn in
      let max_attempts = Awskit.Retry.max_attempts policy in
      match Awskit.Retry.delay policy ~attempt ~error with
      | Some delay when replayable ->
          let* () = R.sleep conn delay in
          retry (attempt + 1)
      | Some _delay ->
          return_error
            (Awskit.Error.Internal.with_retry ~attempt ~max_attempts
               ~reason:"not retried because request body is not replayable"
               error)
      | None when attempt >= max_attempts ->
          return_error
            (Awskit.Error.Internal.with_retry ~attempt ~max_attempts
               ~reason:"retry attempts exhausted" error)
      | None ->
          return_error
            (Awskit.Error.Internal.with_retry ~attempt ~max_attempts
               ~reason:"error is not retryable by policy" error)

    type 'a response_action =
      | Done of ('a, Awskit.Error.t) result
      | Retry of Awskit.Error.t

    let with_response conn ~method_ ~request ~query ~headers ~payload_hash body
        ~f =
      let replayable = (R.Request_body.descriptor body).replayable in
      let rec attempt attempt_number =
        let* request =
          signed_request conn ~method_ ~request ~query ~headers ~payload_hash
        in
        match request with
        | Error error -> return_error error
        | Ok request -> (
            let* response =
              R.with_response conn request body
                ~f:(fun response response_body ->
                  if Awskit.Response.is_success response then
                    let* result = f response response_body in
                    return_ok (Done result)
                  else
                    let* body =
                      read_response_body response_body ~max_size:1_048_576L
                    in
                    match body with
                    | Error error -> return_ok (Done (Error error))
                    | Ok body ->
                        return_ok (Retry (service_error response (Some body))))
            in
            match response with
            | Error error ->
                retry_or_error conn ~attempt:attempt_number ~replayable error
                  attempt
            | Ok (Done result) -> return result
            | Ok (Retry error) ->
                retry_or_error conn ~attempt:attempt_number ~replayable error
                  attempt)
      in
      attempt 1

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
