(** High-level S3 transfer configuration shared by object transfer helpers. *)

val min_part_size : int
(** Minimum multipart part size accepted by S3, except for the final part. *)

val default_part_size : int
(** Default multipart transfer part size in bytes. *)

val default_multipart_threshold : int64
(** Default object size at which high-level helpers switch from a single request
    to multipart/ranged transfer. *)

val default_concurrency : int
(** Default number of concurrent part operations for helpers that can use
    parallel transfer. *)

val max_parts : int
(** Maximum number of parts supported by S3 multipart upload. *)

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
(** High-level upload behavior. [put_options] are used for single-request
    uploads; multipart option records are used when the selected strategy is
    multipart or when resuming an upload. *)

type download_options = {
  multipart_threshold : int64;
  part_size : int;
  concurrency : int;
  get_options : Object.Get.options;
}
(** High-level download behavior. [get_options] are used for both single
    [GetObject] downloads and ranged downloads. *)

type upload_strategy = [ `Put | `Multipart ]
type download_strategy = [ `Get | `Ranged ]

type multipart_upload_result = {
  upload : Multipart.Upload.t;
  parts : Multipart.Part.t list;
  complete : Multipart.Complete.result;
}
(** Result of a completed multipart upload. *)

type upload_result =
  | Put of Object.Put.result
  | Multipart of multipart_upload_result
      (** High-level upload result, tagged by the strategy actually used. *)

type download_result =
  | Get of Object.Get.result
  | Ranged of { info : Object.Head.result; parts : int }
      (** High-level download result. Ranged downloads include the initial
          [HeadObject] metadata and the number of downloaded ranges. *)

val upload_strategy : upload_result -> upload_strategy
val download_strategy : download_result -> download_strategy
val default_upload_options : upload_options
val default_download_options : download_options

val validate_upload_options : upload_options -> (unit, Awskit.Error.t) result
(** Validate upload thresholds, part size, concurrency, and nested options. *)

val validate_upload_multipart_selection :
  upload_options -> (unit, Awskit.Error.t) result
(** Validate that multipart-specific settings are usable when multipart upload
    is selected. *)

val validate_download_options :
  download_options -> (unit, Awskit.Error.t) result
(** Validate download thresholds, part size, and concurrency. *)

val validate_multipart_part_count :
  content_length:int64 -> part_size:int -> (unit, Awskit.Error.t) result
(** Check that [content_length] can be represented within S3's maximum part
    count for [part_size]. *)
