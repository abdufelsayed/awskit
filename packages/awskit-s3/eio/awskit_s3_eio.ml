type t = { aws : Awskit_eio.t; provider : Awskit_s3.Provider.t }

module Runtime = struct
  type connection = t
  type 'a t = 'a
  type upload_body = Awskit_eio.Runtime.upload_body
  type download_body = Awskit_eio.Runtime.download_body
  type upload_writer = Awskit_eio.Runtime.upload_writer
  type download_reader = Awskit_eio.Runtime.download_reader

  let return = Awskit_eio.Runtime.return
  let bind = Awskit_eio.Runtime.bind
  let now t = Awskit_eio.Runtime.now t.aws
  let region t = Awskit_eio.Runtime.region t.aws
  let credentials t = Awskit_eio.Runtime.credentials t.aws
  let s3_provider t = t.provider

  let endpoint t =
    match Awskit_s3.Provider.endpoint t.provider ~region:(region t) with
    | Ok endpoint -> Some endpoint
    | Error _ -> None

  let empty_body = Awskit_eio.Runtime.empty_body
  let string_body = Awskit_eio.Runtime.string_body
  let bytes_body = Awskit_eio.Runtime.bytes_body
  let stream_body = Awskit_eio.Runtime.stream_body
  let upload_descriptor = Awskit_eio.Runtime.upload_descriptor
  let write_string = Awskit_eio.Runtime.write_string
  let read = Awskit_eio.Runtime.read
  let with_download_body = Awskit_eio.Runtime.with_download_body
  let call t request body = Awskit_eio.Runtime.call t.aws request body
end

module S3 = Awskit_s3.Make (Runtime)

let create ~sw ~env ~region ~credentials
    ?(provider = Awskit_s3.Provider.default) () =
  let aws = Awskit_eio.create ~sw ~env ~region ~credentials () in
  { aws; provider }

module Object = struct
  include S3.Object

  module Transfer = struct
    let upload_from_path conn ~bucket ~key ?options ~path () =
      let body = Eio.Path.load path in
      Buffer.put_string conn ~bucket ~key ?options body

    let download_to_path conn ~bucket ~key ?options ~path () =
      match
        Buffer.get_string conn ~bucket ~key ~max_size:Int64.max_int ?options ()
      with
      | Error _ as error -> error
      | Ok (info, body) ->
          Eio.Path.save ~create:(`Or_truncate 0o600) path body;
          Ok info
  end
end

module Bucket = S3.Bucket
module Multipart = S3.Multipart
module Presigned = S3.Presigned
