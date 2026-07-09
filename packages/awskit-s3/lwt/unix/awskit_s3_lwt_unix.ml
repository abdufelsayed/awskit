module Runtime = Awskit_lwt_unix.Runtime
module S3 = Awskit_s3.Make (Runtime)

type t = S3.t

module File_transfer = Transfer
module Body_reader = File_transfer.Make_body_reader (Runtime) (S3)
module Body = Body_reader.Body
module Reader = Body_reader.Reader

let create ?ctx ?endpoint_config ?region ?credentials ?clock ?retry_policy
    ?random_float ?timeout_policy ?max_response_drain_bytes ?imdsv1_fallback ()
    =
  match
    Awskit_lwt_unix.create ?ctx ?region ?credentials ?clock ?retry_policy
      ?random_float ?timeout_policy ?max_response_drain_bytes ?imdsv1_fallback
      ()
  with
  | Error _ as error -> error
  | Ok runtime_connection -> Ok (S3.create ?endpoint_config runtime_connection)

module Object = struct
  include S3.Object

  module Transfer = struct
    include File_transfer.Make (Runtime) (S3) (Body) (Reader)
  end
end

module Bucket = S3.Bucket
module Multipart = S3.Multipart
module Presigned = S3.Presigned
