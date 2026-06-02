(** High-level S3 transfer configuration shared by object transfer helpers. *)

val min_part_size : int
val default_part_size : int
val default_multipart_threshold : int64
val default_concurrency : int
val max_parts : int

type upload_options = {
  multipart_threshold : int64;
  part_size : int;
  concurrency : int;
  put_options : Object.Put.options;
  create_options : Multipart.Create.options;
  upload_part_options : Multipart.Upload_part.options;
  complete_options : Multipart.Complete.options;
  abort_options : Multipart.Abort.options;
  list_parts_options : Multipart.List_parts.options;
}

type download_options = {
  multipart_threshold : int64;
  part_size : int;
  concurrency : int;
  get_options : Object.Get.options;
}

type upload_strategy = [ `Put | `Multipart ]
type download_strategy = [ `Get | `Ranged ]

type multipart_upload_result = {
  upload : Multipart.Upload.t;
  parts : Multipart.Part.t list;
  complete : Multipart.Complete.result;
}

type upload_result =
  | Put of Object.Put.result
  | Multipart of multipart_upload_result

type download_result =
  | Get of Object.Get.result
  | Ranged of { info : Object.Head.result; parts : int }

val upload_strategy : upload_result -> upload_strategy
val download_strategy : download_result -> download_strategy
val default_upload_options : upload_options
val default_download_options : download_options
val validate_upload_options : upload_options -> (unit, Awskit.Error.t) result

val validate_upload_multipart_selection :
  upload_options -> (unit, Awskit.Error.t) result

val validate_download_options :
  download_options -> (unit, Awskit.Error.t) result

val validate_multipart_part_count :
  content_length:int64 -> part_size:int -> (unit, Awskit.Error.t) result
