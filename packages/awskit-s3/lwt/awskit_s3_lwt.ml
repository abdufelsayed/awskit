module Make (Client : Cohttp_lwt.S.Client) = struct
  module Aws = Awskit_lwt.Make (Client)
  module Runtime = Aws.Runtime
  module S3 = Awskit_s3.Make (Runtime)

  type t = S3.t
  type +'a io = 'a Lwt.t

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

  let create ?ctx ?endpoint_config ~region ~credentials ~clock ?retry_policy
      ?sleep ?random_float ?timeout_policy ?max_response_drain_bytes () =
    match
      Aws.create ?ctx ~region ~credentials ~clock ?retry_policy ?sleep
        ?random_float ?timeout_policy ?max_response_drain_bytes ()
    with
    | Error _ as error -> error
    | Ok runtime_connection ->
        Ok (S3.create ?endpoint_config runtime_connection)

  module Object = S3.Object
  module Bucket = S3.Bucket
  module Multipart = S3.Multipart
  module Presigned = S3.Presigned
end
