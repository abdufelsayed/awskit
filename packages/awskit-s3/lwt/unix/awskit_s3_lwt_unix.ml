module Runtime = Awskit_lwt_unix.Runtime
module S3 = Awskit_s3.Make (Runtime)
include S3

type t = Awskit_lwt_unix.t

let create ?ctx ?endpoint ?region ?credentials ?clock ?max_response_body_bytes
    () =
  match
    Awskit_lwt_unix.create ?ctx ?endpoint ?region ?credentials ?clock
      ?max_response_body_bytes ()
  with
  | Ok conn -> Ok conn
  | Error (#Awskit.Error.base as error) -> Error (error :> Awskit_s3.Error.t)
