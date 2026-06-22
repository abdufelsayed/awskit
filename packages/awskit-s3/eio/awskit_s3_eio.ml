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
module Body_reader = File_transfer.Make_body_reader (Runtime) (S3)
module Body = Body_reader.Body
module Reader = Body_reader.Reader

let create ~sw ~env ~https ~region ~credentials ?retry_policy
    ?(endpoint_config = Awskit_s3.default_endpoint_config) () =
  match
    Awskit_eio.create ~sw ~env ~https ~region ~credentials ?retry_policy ()
  with
  | Error _ as error -> error
  | Ok aws -> Ok { aws; endpoint_config }

module Object = struct
  include S3.Object

  module Transfer = struct
    include File_transfer.Make (Runtime) (S3) (Body) (Reader)
  end
end

module Bucket = S3.Bucket
module Multipart = S3.Multipart
module Presigned = S3.Presigned
