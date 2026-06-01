module Make (Client : Cohttp_lwt.S.Client) = struct
  module Aws = Awskit_lwt.Make (Client)

  type t = { aws : Aws.t; endpoint_config : Awskit_s3.endpoint_config }

  module Runtime = struct
    type connection = t
    type 'a t = 'a Lwt.t
    type request_body = Aws.Runtime.request_body
    type response_body = Aws.Runtime.response_body
    type request_body_writer = Aws.Runtime.request_body_writer
    type response_body_reader = Aws.Runtime.response_body_reader

    let return = Aws.Runtime.return
    let bind = Aws.Runtime.bind
    let now t = Aws.Runtime.now t.aws
    let region t = Aws.Runtime.region t.aws
    let credentials t = Aws.Runtime.credentials t.aws
    let retry_policy t = Aws.Runtime.retry_policy t.aws
    let sleep t = Aws.Runtime.sleep t.aws
    let s3_endpoint_config t = t.endpoint_config
    let endpoint _ = None
    let empty_request_body = Aws.Runtime.empty_request_body
    let string_request_body = Aws.Runtime.string_request_body
    let bytes_request_body = Aws.Runtime.bytes_request_body
    let stream_request_body = Aws.Runtime.stream_request_body
    let request_body_descriptor = Aws.Runtime.request_body_descriptor
    let write_request_body_string = Aws.Runtime.write_request_body_string
    let read_response_body = Aws.Runtime.read_response_body
    let with_response_body = Aws.Runtime.with_response_body
    let discard_response_body = Aws.Runtime.discard_response_body

    let with_response t request body ~f =
      Aws.Runtime.with_response t.aws request body ~f
  end

  module S3 = Awskit_s3.Make (Runtime)

  let create ?ctx ?endpoint ?addressing_style ?endpoint_variant ?scheme ~region
      ~credentials ~clock ?retry_policy ?sleep () =
    let aws =
      Aws.create ?ctx ~region ~credentials ~clock ?retry_policy ?sleep ()
    in
    let endpoint_config =
      Awskit_s3.endpoint_config ?addressing_style ?endpoint_variant ?scheme
        ?endpoint ()
    in
    { aws; endpoint_config }

  module Object = S3.Object
  module Bucket = S3.Bucket
  module Multipart = S3.Multipart
  module Presigned = S3.Presigned
end
