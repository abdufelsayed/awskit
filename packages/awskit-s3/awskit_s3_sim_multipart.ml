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

  let list_parts _conn ~bucket ~key ~upload_id:_ ?options:_ () =
    let* () = validate_bucket_key bucket key in
    Error
      (Awskit.Error.validation ~field:"multipart"
         "multipart simulator port is deferred")

  module Paginator = struct
    let fold_pages _conn ~bucket ~key ~upload_id:_ ?options:_ ?max_pages:_
        ~init:_ ~f:_ () =
      let* () = validate_bucket_key bucket key in
      Error
        (Awskit.Error.validation ~field:"multipart"
           "multipart simulator port is deferred")

    let pages conn ~bucket ~key ~upload_id ?options ?max_pages () =
      fold_pages conn ~bucket ~key ~upload_id ?options ?max_pages ~init:[]
        ~f:(fun pages page -> Ok (page :: pages))
        ()

    let parts conn ~bucket ~key ~upload_id ?options ?max_pages () =
      fold_pages conn ~bucket ~key ~upload_id ?options ?max_pages ~init:[]
        ~f:(fun parts (page : Multipart.List_parts.page) ->
          Ok (List.rev_append page.parts parts))
        ()
  end
end
