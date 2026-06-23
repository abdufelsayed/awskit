open Common

let min_part_size = 5 * 1024 * 1024
let default_part_size = 8 * 1024 * 1024
let default_multipart_threshold = Int64.mul 8L (Int64.mul 1024L 1024L)
let default_concurrency = 4
let max_parts = 10_000

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
  | Get of Object.Get.info
  | Ranged of { info : Object.Head.result; parts : int }

let upload_strategy = function Put _ -> `Put | Multipart _ -> `Multipart
let download_strategy = function Get _ -> `Get | Ranged _ -> `Ranged

let default_upload_options =
  {
    multipart_threshold = default_multipart_threshold;
    part_size = default_part_size;
    concurrency = default_concurrency;
    put_options = Object.Put.default_options;
    create_options = Multipart.Create.default_options;
    upload_part_options = Multipart.Upload_part.default_options;
    complete_options = Multipart.Complete.default_options;
    abort_options = Multipart.Abort.default_options;
    list_parts_options = Multipart.List_parts.default_options;
  }

let default_download_options =
  {
    multipart_threshold = default_multipart_threshold;
    part_size = default_part_size;
    concurrency = default_concurrency;
    get_options = Object.Get.default_options;
  }

let validate_common ~multipart_threshold ~concurrency =
  if Int64.compare multipart_threshold 0L < 0 then
    invalid ~field:"multipart_threshold" "multipart_threshold must be >= 0"
  else if concurrency <= 0 then
    invalid ~field:"concurrency" "concurrency must be positive"
  else Ok ()

let validate_upload_part_size part_size =
  if part_size < min_part_size then
    invalid ~field:"part_size"
      "part_size must be at least 5 MiB for S3 multipart upload"
  else Ok ()

let validate_download_part_size part_size =
  if part_size <= 0 then
    invalid ~field:"part_size" "download part_size must be positive"
  else Ok ()

let validate_upload_options (options : upload_options) =
  let* () =
    validate_common ~multipart_threshold:options.multipart_threshold
      ~concurrency:options.concurrency
  in
  let* () = validate_upload_part_size options.part_size in
  if Option.is_some options.create_options.checksum_algorithm then
    invalid ~field:"create_options.checksum_algorithm"
      "multipart file helpers do not compute per-part checksums"
  else if Option.is_some options.create_options.checksum_type then
    invalid ~field:"create_options.checksum_type"
      "multipart file helpers do not compute per-part checksums"
  else if Option.is_some options.upload_part_options.checksum then
    invalid ~field:"upload_part_options.checksum"
      "multipart file helpers require per-part checksum values from low-level \
       multipart calls"
  else if Option.is_some options.complete_options.checksum then
    invalid ~field:"complete_options.checksum"
      "multipart file helpers do not compute complete-object checksums"
  else if Option.is_some options.complete_options.checksum_type then
    invalid ~field:"complete_options.checksum_type"
      "multipart file helpers do not compute complete-object checksums"
  else Ok ()

let validate_multipart_part_count ~content_length ~part_size =
  if Int64.equal content_length 0L then Ok ()
  else
    let part_size64 = Int64.of_int part_size in
    let part_count =
      Int64.div (Int64.add content_length (Int64.pred part_size64)) part_size64
    in
    if Int64.compare part_count (Int64.of_int max_parts) > 0 then
      invalid ~field:"part_count"
        "multipart file transfer would exceed 10000 parts"
    else Ok ()

let validate_upload_multipart_selection (options : upload_options) =
  match options.put_options.checksum with
  | Some _ ->
      invalid ~field:"put_options.checksum"
        "optimized multipart file upload cannot use a single object checksum"
  | None -> Ok ()

let validate_download_options (options : download_options) =
  let* () =
    validate_common ~multipart_threshold:options.multipart_threshold
      ~concurrency:options.concurrency
  in
  let* () = validate_download_part_size options.part_size in
  match options.get_options.range with
  | Some _ ->
      invalid ~field:"get_options.range"
        "optimized download_file does not accept a caller-supplied range"
  | None -> Ok ()
