type t = { aws : Awskit_eio.t; endpoint_config : Awskit_s3.endpoint_config }

module Runtime = struct
  type connection = t
  type 'a t = 'a
  type request_body = Awskit_eio.Runtime.request_body
  type response_body = Awskit_eio.Runtime.response_body
  type request_body_writer = Awskit_eio.Runtime.request_body_writer
  type response_body_reader = Awskit_eio.Runtime.response_body_reader

  let return = Awskit_eio.Runtime.return
  let bind = Awskit_eio.Runtime.bind
  let now t = Awskit_eio.Runtime.now t.aws
  let region t = Awskit_eio.Runtime.region t.aws
  let credentials t = Awskit_eio.Runtime.credentials t.aws
  let retry_policy t = Awskit_eio.Runtime.retry_policy t.aws
  let sleep t = Awskit_eio.Runtime.sleep t.aws
  let s3_endpoint_config t = t.endpoint_config
  let endpoint _ = None

  module Request_body = Awskit_eio.Runtime.Request_body
  module Response_body = Awskit_eio.Runtime.Response_body

  let with_response t request body ~f =
    Awskit_eio.Runtime.with_response t.aws request body ~f
end

module S3 = Awskit_s3.Make (Runtime)
module File_transfer = Transfer

let create ~sw ~env ~region ~credentials ?retry_policy ?endpoint
    ?addressing_style ?endpoint_variant ?scheme () =
  let aws = Awskit_eio.create ~sw ~env ~region ~credentials ?retry_policy () in
  let endpoint_config =
    Awskit_s3.endpoint_config ?addressing_style ?endpoint_variant ?scheme
      ?endpoint ()
  in
  { aws; endpoint_config }

module Object = struct
  include S3.Object

  module Transfer = struct
    include S3.Object.Transfer
    include File_transfer.Make (Runtime) (S3)
  end
end

module Bucket = S3.Bucket
module Multipart = S3.Multipart
module Presigned = S3.Presigned
