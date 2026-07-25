module Make (Client : Cohttp_lwt.S.Client) = struct
  module Aws = Awskit_lwt.Make (Client)

  type t = { aws : Aws.t; endpoint_config : Awskit_s3.endpoint_config }
  type runtime_connection = t

  module Runtime = struct
    type connection = runtime_connection
    type 'a t = 'a Lwt.t
    type request_body = Aws.Runtime.request_body
    type response_body = Aws.Runtime.response_body
    type request_body_writer = Aws.Runtime.request_body_writer
    type response_body_reader = Aws.Runtime.response_body_reader

    module IO = Aws.Runtime.IO
    module Request_body = Aws.Runtime.Request_body
    module Response_body = Aws.Runtime.Response_body

    module Transport = struct
      type 'a io = 'a Lwt.t
      type connection = runtime_connection
      type request_body = Aws.Runtime.request_body
      type response_body = Aws.Runtime.response_body

      let with_response t request ~body ~consume =
        Aws.Runtime.Transport.with_response t.aws request ~body ~consume
    end

    module Clock = struct
      type connection = runtime_connection

      let now t = Aws.Runtime.Clock.now t.aws
    end

    module Sleeper = struct
      type 'a io = 'a Lwt.t
      type connection = runtime_connection

      let sleep t span = Aws.Runtime.Sleeper.sleep t.aws span
    end

    module Random = struct
      type connection = runtime_connection

      let float t ~upper_bound = Aws.Runtime.Random.float t.aws ~upper_bound
    end

    module Credentials = struct
      type 'a io = 'a Lwt.t
      type connection = runtime_connection

      let resolve t = Aws.Runtime.Credentials.resolve t.aws
    end

    module Endpoint = struct
      type connection = runtime_connection

      let region t = Aws.Runtime.Endpoint.region t.aws
      let endpoint t = Aws.Runtime.Endpoint.endpoint t.aws
    end

    module Retry = struct
      type connection = runtime_connection

      let policy t = Aws.Runtime.Retry.policy t.aws
    end

    module Timeout = struct
      type connection = runtime_connection

      let policy t = Aws.Runtime.Timeout.policy t.aws
    end

    module S3_endpoint = struct
      type connection = runtime_connection

      let s3_endpoint_config t = t.endpoint_config
    end
  end

  module Observer = struct
    type 'a io = 'a Lwt.t
    type connection = runtime_connection
    type lease = Aws.Runtime_observer.lease

    let with_operation t = Aws.Runtime_observer.with_operation t.aws
    let emit_event t = Aws.Runtime_observer.emit_event t.aws
    let acquire t = Aws.Runtime_observer.acquire t.aws
    let add = Aws.Runtime_observer.add
    let release = Aws.Runtime_observer.release
    let with_instrument t = Aws.Runtime_observer.with_instrument t.aws
  end

  module S3 = Awskit_s3.Observability.For_runtime.Make (Runtime) (Observer)

  module Body = struct
    include S3.Body

    let of_lwt_stream ~content_length stream =
      let open Lwt.Infix in
      of_stream ~content_length ~replayable:false ~write:(fun writer ->
          let rec loop () =
            Lwt_stream.get stream >>= function
            | None -> Lwt.return_ok ()
            | Some chunk -> (
                Writer.write_string writer chunk >>= function
                | Ok () -> loop ()
                | Error _ as error -> Lwt.return error)
          in
          loop ())
  end

  module Reader = S3.Reader

  let create ?ctx ?(endpoint_config = Awskit_s3.default_endpoint_config) ~region
      ~credentials ~clock ?retry_policy ?sleep ?random_float ?timeout_policy
      ?max_response_drain_bytes ?observability () =
    match
      Aws.create ?ctx ~region ~credentials ~clock ?retry_policy ?sleep
        ?random_float ?timeout_policy ?max_response_drain_bytes ?observability
        ()
    with
    | Error _ as error -> error
    | Ok aws -> Ok { aws; endpoint_config }

  module Object = S3.Object
  module Bucket = S3.Bucket
  module Multipart = S3.Multipart
  module Presigned = S3.Presigned
end
