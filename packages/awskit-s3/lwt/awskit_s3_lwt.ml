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

    module Request_body = Aws.Runtime.Request_body
    module Response_body = Aws.Runtime.Response_body

    let with_response t request body ~f =
      Aws.Runtime.with_response t.aws request body ~f
  end

  module S3 = Awskit_s3.Make (Runtime)

  module Body = struct
    include S3.Body

    let of_lwt_stream ~content_length stream =
      let open Lwt.Infix in
      of_stream ~content_length ~write:(fun writer ->
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

  let parse_endpoint = function
    | None -> Ok None
    | Some endpoint -> (
        match Awskit.Endpoint.of_string endpoint with
        | Ok endpoint -> Ok (Some (Awskit.Endpoint.to_url_prefix endpoint))
        | Error _ as error -> error)

  let create ?ctx ?endpoint ?addressing_style ?endpoint_variant ?scheme ~region
      ~credentials ~clock ?retry_policy ?sleep () =
    match
      ( Aws.create ?ctx ~region ~credentials ~clock ?retry_policy ?sleep (),
        parse_endpoint endpoint )
    with
    | Error error, _ | _, Error error -> Error error
    | Ok aws, Ok endpoint ->
        let endpoint_config =
          Awskit_s3.endpoint_config ?addressing_style ?endpoint_variant ?scheme
            ?endpoint ()
        in
        Ok { aws; endpoint_config }

  module Object = S3.Object
  module Bucket = S3.Bucket
  module Multipart = S3.Multipart
  module Presigned = S3.Presigned
end
