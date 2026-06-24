(** High-level S3 transfer configuration shared by object transfer helpers.

    Transfer options cover strategy selection, local file behavior, and the S3
    operation option records used by helper implementations. Retry and timeout
    policies remain client/runtime construction policy; transfer helpers execute
    ordinary S3 operations through that configured client instead of carrying
    separate per-transfer retry or timeout placeholders. *)

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

(** Direction reported by transfer progress callbacks. *)
type direction = Upload | Download

(** Transfer phase reported by progress callbacks.

    [Single_request] is used by ordinary [PutObject] and [GetObject] helpers,
    [Part] by multipart upload parts, and [Ranged_get] by ranged download
    helpers. *)
type phase = Single_request | Part | Ranged_get

type progress = {
  direction : direction;
  phase : phase;
  transferred : int64;
  total : int64 option;
  part_number : Multipart.Part_number.t option;
}
(** Structured progress event. [transferred] is cumulative for the current
    helper invocation. *)

type upload_options = private {
  multipart_threshold : int64;
      (** Object size at or above which high-level upload helpers use multipart
          upload. *)
  part_size : int;  (** Multipart part size in bytes. *)
  concurrency : int;
      (** Maximum number of in-flight part operations for capable adapters. *)
  put_options : Object.Put.options;
      (** Options used by single-request [PutObject] uploads. *)
  create_options : Multipart.Create.options;
      (** Options used by [CreateMultipartUpload]. *)
  upload_part_options : Multipart.Upload_part.options;
      (** Options used by each [UploadPart]. *)
  complete_options : Multipart.Complete.options;
      (** Options used by [CompleteMultipartUpload]. *)
  abort_options : Multipart.Abort.options;
      (** Options used when aborting a failed Awskit-created multipart upload.
      *)
  list_parts_options : Multipart.List_parts.options;
      (** Options used when verifying caller-owned uploads before resumed file
          transfer writes fresh parts. *)
}
(** High-level upload behavior. [put_options] are used for single-request
    uploads; multipart option records are used when the selected strategy is
    multipart or when writing into a caller-owned upload. *)

(** Local target behavior for high-level download helpers.

    [Replace] publishes the completed temporary download over an existing
    target. [Error_if_exists] rejects an existing target before transport. *)
type overwrite = Replace | Error_if_exists

type download_options = private {
  multipart_threshold : int64;
      (** Object size at or above which high-level download helpers use ranged
          requests. *)
  part_size : int;  (** Range size in bytes for ranged downloads. *)
  concurrency : int;
      (** Maximum number of in-flight range requests for capable adapters. *)
  overwrite : overwrite;  (** Local target overwrite policy. *)
  get_options : Object.Get.options;
      (** Options used by [GetObject] and ranged [GetObject] requests. *)
}
(** High-level download behavior. [get_options] are used for both single
    [GetObject] downloads and ranged downloads. *)

type upload_strategy = [ `Put | `Multipart ]
(** Strategy used by a high-level upload result. *)

type download_strategy = [ `Get | `Ranged ]
(** Strategy used by a high-level download result. *)

type put_upload_result = {
  put : Object.Put.result;
      (** Metadata returned by the underlying [PutObject] request. *)
  bytes_transferred : int64;
      (** Number of local file bytes streamed into the request body. *)
}
(** Result of a single-request upload. *)

type multipart_upload_result = {
  upload : Multipart.Upload.caller_owned Multipart.Upload.t;
  parts : Multipart.Part.t list;
  complete : Multipart.Complete.result;
  bytes_transferred : int64;
}
(** Result of a completed multipart upload. *)

type upload_result =
  | Put of put_upload_result
  | Multipart of multipart_upload_result
      (** High-level upload result, tagged by the strategy actually used. *)

type get_download_result = {
  info : Object.Get.info;
      (** Metadata returned by the underlying [GetObject] request. *)
  bytes_transferred : int64;
      (** Number of response-body bytes streamed into the local file. *)
}
(** Result of a single-request download. *)

type ranged_download_result = {
  info : Object.Head.result;
      (** Metadata captured before ranged [GetObject] requests. *)
  parts : int;  (** Number of ranges downloaded. *)
  bytes_transferred : int64;
      (** Number of response-body bytes streamed into the local file. *)
}
(** Result of a ranged download. *)

type download_result =
  | Get of get_download_result
  | Ranged of ranged_download_result
      (** High-level download result. Ranged downloads include the initial
          [HeadObject] metadata and the number of downloaded ranges. *)

val upload_strategy : upload_result -> upload_strategy
(** Return the strategy used by an upload helper result. *)

val download_strategy : download_result -> download_strategy
(** Return the strategy used by a download helper result. *)

val upload_bytes_transferred : upload_result -> int64
(** Return the number of local file bytes streamed by an upload helper. *)

val download_bytes_transferred : download_result -> int64
(** Return the number of response-body bytes streamed by a download helper. *)

val default_upload_options : upload_options
(** Default high-level upload options. *)

val default_download_options : download_options
(** Default high-level download options. *)

val progress :
  direction:direction ->
  phase:phase ->
  transferred:int64 ->
  ?total:int64 ->
  ?part_number:Multipart.Part_number.t ->
  unit ->
  progress
(** Build a structured transfer progress event. *)

val upload_options :
  ?multipart_threshold:int64 ->
  ?part_size:int ->
  ?concurrency:int ->
  ?put_options:Object.Put.options ->
  ?create_options:Multipart.Create.options ->
  ?upload_part_options:Multipart.Upload_part.options ->
  ?complete_options:Multipart.Complete.options ->
  ?abort_options:Multipart.Abort.options ->
  ?list_parts_options:Multipart.List_parts.options ->
  unit ->
  (upload_options, Awskit.Error.t) result
(** Build and validate high-level upload options. *)

val upload_options_exn :
  ?multipart_threshold:int64 ->
  ?part_size:int ->
  ?concurrency:int ->
  ?put_options:Object.Put.options ->
  ?create_options:Multipart.Create.options ->
  ?upload_part_options:Multipart.Upload_part.options ->
  ?complete_options:Multipart.Complete.options ->
  ?abort_options:Multipart.Abort.options ->
  ?list_parts_options:Multipart.List_parts.options ->
  unit ->
  upload_options
(** Like {!val:upload_options}, but raises [Awskit.Error.Awskit_error] carrying
    the structured validation error on validation failure. *)

val download_options :
  ?multipart_threshold:int64 ->
  ?part_size:int ->
  ?concurrency:int ->
  ?overwrite:overwrite ->
  ?get_options:Object.Get.options ->
  unit ->
  (download_options, Awskit.Error.t) result
(** Build and validate high-level download options. *)

val download_options_exn :
  ?multipart_threshold:int64 ->
  ?part_size:int ->
  ?concurrency:int ->
  ?overwrite:overwrite ->
  ?get_options:Object.Get.options ->
  unit ->
  download_options
(** Like {!val:download_options}, but raises [Awskit.Error.Awskit_error]
    carrying the structured validation error on validation failure. *)

val upload_multipart_threshold : upload_options -> int64
val upload_part_size : upload_options -> int
val upload_concurrency : upload_options -> int
val upload_put_options : upload_options -> Object.Put.options
val upload_create_options : upload_options -> Multipart.Create.options
val upload_part_options : upload_options -> Multipart.Upload_part.options
val upload_complete_options : upload_options -> Multipart.Complete.options
val upload_abort_options : upload_options -> Multipart.Abort.options
val upload_list_parts_options : upload_options -> Multipart.List_parts.options
val download_multipart_threshold : download_options -> int64
val download_part_size : download_options -> int
val download_concurrency : download_options -> int
val download_overwrite : download_options -> overwrite
val download_get_options : download_options -> Object.Get.options

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

module Plan : sig
  type upload_part = {
    part_number : Multipart.Part_number.t;
    offset : int64;
    length : int;
  }
  (** Runtime-neutral file part selected for [UploadPart]. *)

  type download_range = {
    index : int;
    offset : int64;
    length : int;
    range : Range.t;
  }
  (** Runtime-neutral byte range selected for ranged [GetObject]. *)

  val upload_parts :
    content_length:int64 ->
    part_size:int ->
    (upload_part list, Awskit.Error.t) result
  (** Compute deterministic multipart upload parts. Rejects non-positive content
      lengths because an empty object should use [PutObject]. *)

  val upload_part_seq :
    content_length:int64 ->
    part_size:int ->
    (upload_part Seq.t, Awskit.Error.t) result
  (** Lazily enumerate deterministic multipart upload parts without building the
      whole plan list. Rejects non-positive content lengths because an empty
      object should use [PutObject]. *)

  val download_ranges :
    content_length:int64 ->
    part_size:int ->
    (download_range list, Awskit.Error.t) result
  (** Compute deterministic ranged-download byte ranges. Empty objects produce
      no ranges. *)

  val download_range_seq :
    content_length:int64 ->
    part_size:int ->
    (download_range Seq.t, Awskit.Error.t) result
  (** Lazily enumerate deterministic ranged-download byte ranges without
      building the whole plan list. Empty objects produce no ranges. *)
end
