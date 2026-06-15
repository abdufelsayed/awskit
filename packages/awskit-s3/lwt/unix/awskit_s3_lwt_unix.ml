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
module Body_reader = File_transfer.Make_body_reader (Runtime) (S3)
module Body = Body_reader.Body
module Reader = Body_reader.Reader

let parse_optional parse = function
  | None -> Ok None
  | Some value -> (
      match parse value with
      | Ok value -> Ok (Some value)
      | Error _ as error -> error)

let create ?ctx ?endpoint ?addressing_style ?endpoint_variant ?scheme ?region
    ?credentials ?clock ?retry_policy ?imdsv1_fallback () =
  match
    ( parse_optional Awskit.Region.of_string region,
      parse_optional Awskit.Endpoint.of_string endpoint )
  with
  | Error error, _ | _, Error error -> Error error
  | Ok region, Ok endpoint -> (
      let region =
        match region with
        | None -> None
        | Some region -> Some (Awskit.Region.to_string region)
      in
      let endpoint =
        match endpoint with
        | None -> None
        | Some endpoint -> Some (Awskit.Endpoint.to_url_prefix endpoint)
      in
      match
        Awskit_lwt_unix.create ?ctx ?region ?credentials ?clock ?retry_policy
          ?imdsv1_fallback ()
      with
      | Error _ as error -> error
      | Ok aws ->
          let endpoint_config =
            Awskit_s3.endpoint_config ?addressing_style ?endpoint_variant
              ?scheme ?endpoint ()
          in
          Ok { aws; endpoint_config })

module Object = struct
  include S3.Object

  module Transfer = struct
    include File_transfer.Make (Runtime) (S3) (Body) (Reader)
  end
end

module Bucket = S3.Bucket
module Multipart = S3.Multipart
module Presigned = S3.Presigned
