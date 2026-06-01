type t = {
  aws : Awskit_lwt_unix.t;
  endpoint_config : Awskit_s3.endpoint_config;
}

module Runtime = struct
  type connection = t
  type 'a t = 'a Lwt.t
  type request_body = Awskit_lwt_unix.Runtime.request_body
  type response_body = Awskit_lwt_unix.Runtime.response_body
  type request_body_writer = Awskit_lwt_unix.Runtime.request_body_writer
  type response_body_reader = Awskit_lwt_unix.Runtime.response_body_reader

  let return = Awskit_lwt_unix.Runtime.return
  let bind = Awskit_lwt_unix.Runtime.bind
  let now t = Awskit_lwt_unix.Runtime.now t.aws
  let region t = Awskit_lwt_unix.Runtime.region t.aws
  let credentials t = Awskit_lwt_unix.Runtime.credentials t.aws
  let retry_policy t = Awskit_lwt_unix.Runtime.retry_policy t.aws
  let sleep t = Awskit_lwt_unix.Runtime.sleep t.aws
  let s3_endpoint_config t = t.endpoint_config
  let endpoint _ = None

  module Request_body = Awskit_lwt_unix.Runtime.Request_body
  module Response_body = Awskit_lwt_unix.Runtime.Response_body

  let with_response t request body ~f =
    Awskit_lwt_unix.Runtime.with_response t.aws request body ~f
end

module S3 = Awskit_s3.Make (Runtime)
module File_transfer = Transfer

let create ?ctx ?endpoint ?addressing_style ?endpoint_variant ?scheme ?region
    ?credentials ?clock ?retry_policy ?imdsv1_fallback () =
  let region = Option.map Awskit.Region.to_string region in
  match
    Awskit_lwt_unix.create ?ctx ?region ?credentials ?clock ?retry_policy
      ?imdsv1_fallback ()
  with
  | Error _ as error -> error
  | Ok aws ->
      let endpoint_config =
        Awskit_s3.endpoint_config ?addressing_style ?endpoint_variant ?scheme
          ?endpoint ()
      in
      Ok { aws; endpoint_config }

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
