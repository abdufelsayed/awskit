type t = { aws : Awskit_eio.t; endpoint_config : Awskit_s3.endpoint_config }
type runtime_connection = t

module Runtime = struct
  type connection = runtime_connection
  type 'a t = 'a
  type request_body = Awskit_eio.Runtime.request_body
  type response_body = Awskit_eio.Runtime.response_body
  type request_body_writer = Awskit_eio.Runtime.request_body_writer
  type response_body_reader = Awskit_eio.Runtime.response_body_reader

  module IO = Awskit_eio.Runtime.IO
  module Request_body = Awskit_eio.Runtime.Request_body
  module Response_body = Awskit_eio.Runtime.Response_body

  module Transport = struct
    type 'a io = 'a
    type connection = runtime_connection
    type request_body = Awskit_eio.Runtime.request_body
    type response_body = Awskit_eio.Runtime.response_body

    let with_response t request ~body ~consume =
      Awskit_eio.Runtime.Transport.with_response t.aws request ~body ~consume
  end

  module Clock = struct
    type connection = runtime_connection

    let now t = Awskit_eio.Runtime.Clock.now t.aws
  end

  module Sleeper = struct
    type 'a io = 'a
    type connection = runtime_connection

    let sleep t span = Awskit_eio.Runtime.Sleeper.sleep t.aws span
  end

  module Random = struct
    type connection = runtime_connection

    let float t ~upper_bound =
      Awskit_eio.Runtime.Random.float t.aws ~upper_bound
  end

  module Credentials = struct
    type 'a io = 'a
    type connection = runtime_connection

    let resolve t = Awskit_eio.Runtime.Credentials.resolve t.aws
  end

  module Endpoint = struct
    type connection = runtime_connection

    let region t = Awskit_eio.Runtime.Endpoint.region t.aws
    let endpoint t = Awskit_eio.Runtime.Endpoint.endpoint t.aws
  end

  module Retry = struct
    type connection = runtime_connection

    let policy t = Awskit_eio.Runtime.Retry.policy t.aws
  end

  module Timeout = struct
    type connection = runtime_connection

    let policy t = Awskit_eio.Runtime.Timeout.policy t.aws
  end

  module S3_endpoint = struct
    type connection = runtime_connection

    let s3_endpoint_config t = t.endpoint_config
  end
end

module Observer = struct
  type 'a io = 'a
  type connection = runtime_connection
  type lease = Awskit_eio.Runtime_observer.lease

  let with_operation t = Awskit_eio.Runtime_observer.with_operation t.aws
  let emit_event t = Awskit_eio.Runtime_observer.emit_event t.aws
  let acquire t = Awskit_eio.Runtime_observer.acquire t.aws
  let add = Awskit_eio.Runtime_observer.add
  let release = Awskit_eio.Runtime_observer.release
  let with_instrument t = Awskit_eio.Runtime_observer.with_instrument t.aws
end

module S3 = Awskit_s3.Observability.Make (Runtime) (Observer)
module File_transfer = Transfer
module Body_reader = File_transfer.Make_body_reader (Runtime) (S3)
module Body = Body_reader.Body
module Reader = Body_reader.Reader

let create ~sw ~env ~https ~region ~credentials ?retry_policy ?random_float
    ?timeout_policy ?(endpoint_config = Awskit_s3.default_endpoint_config)
    ?max_response_drain_bytes ?observability () =
  match
    Awskit_eio.create ~sw ~env ~https ~region ~credentials ?retry_policy
      ?random_float ?timeout_policy ?max_response_drain_bytes ?observability ()
  with
  | Error _ as error -> error
  | Ok aws -> Ok { aws; endpoint_config }

module Object = struct
  include S3.Object

  module Transfer = struct
    include File_transfer.Make (Runtime) (Observer) (S3) (Body) (Reader)
  end
end

module Bucket = S3.Bucket
module Multipart = S3.Multipart
module Presigned = S3.Presigned
