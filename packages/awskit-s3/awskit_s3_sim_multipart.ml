open Awskit_s3_core
open Awskit_s3_sim_support

module Multipart = struct
  type connection = t
  type 'a io = 'a
  type upload_body = Runtime.upload_body

  let create _conn ~bucket ~key ?options:_ () =
    let* () = validate_bucket_key bucket key in
    Error
      (Awskit.Error.validation ~field:"multipart"
         "multipart simulator port is deferred")

  let upload_part _conn ~bucket ~key ~upload_id:_ ~part_number:_ ~body:_
      ?options:_ () =
    let* () = validate_bucket_key bucket key in
    Error
      (Awskit.Error.validation ~field:"multipart"
         "multipart simulator port is deferred")

  let complete _conn ~bucket ~key ~upload_id:_ _parts =
    let* () = validate_bucket_key bucket key in
    Error
      (Awskit.Error.validation ~field:"multipart"
         "multipart simulator port is deferred")

  let abort _conn ~bucket ~key ~upload_id:_ =
    let* () = validate_bucket_key bucket key in
    Error
      (Awskit.Error.validation ~field:"multipart"
         "multipart simulator port is deferred")

  let list_parts _conn ~bucket ~key ~upload_id:_ =
    let* () = validate_bucket_key bucket key in
    Error
      (Awskit.Error.validation ~field:"multipart"
         "multipart simulator port is deferred")
end
