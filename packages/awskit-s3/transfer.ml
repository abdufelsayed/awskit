let ( let* ) = S3_result.( let* )
let min_part_size = 5 * 1024 * 1024
let default_part_size = 8 * 1024 * 1024
let default_multipart_threshold = Int64.mul 8L (Int64.mul 1024L 1024L)
let default_concurrency = 4
let max_parts = 10_000

type direction = Upload | Download
type phase = Single_request | Part | Ranged_get

type progress = {
  direction : direction;
  phase : phase;
  transferred : int64;
  total : int64 option;
  part_number : Multipart.Part_number.t option;
}

type upload_options = {
  multipart_threshold : int64;
  part_size : int;
  concurrency : int;
  put_options : Object.Put.options;
  create_options : Multipart.Create.options;
  upload_part_options : Multipart.Upload_part.options;
  complete_options : Multipart.Complete.options;
  abort_expected_bucket_owner : Account_id.t option;
  list_parts_options : Multipart.List_parts.options;
}

type overwrite = Replace | Error_if_exists

type download_options = {
  multipart_threshold : int64;
  part_size : int;
  concurrency : int;
  overwrite : overwrite;
  get_options : Object.Get.options;
}

type upload_strategy = [ `Put | `Multipart ]
type download_strategy = [ `Get | `Ranged ]
type put_upload_result = { put : Object.Put.result; bytes_transferred : int64 }

type multipart_upload_result = {
  bucket : Bucket_name.t;
  key : Object_key.t;
  upload_id : Multipart.Upload_id.t;
  parts : Multipart.Part.t list;
  complete : Multipart.Complete.result;
  bytes_transferred : int64;
}

type upload_result =
  | Put of put_upload_result
  | Multipart of multipart_upload_result

type get_download_result = { info : Object.Get.info; bytes_transferred : int64 }

type ranged_download_result = {
  info : Object.Head.result;
  parts : int;
  bytes_transferred : int64;
}

type download_result =
  | Get of get_download_result
  | Ranged of ranged_download_result

let upload_strategy = function Put _ -> `Put | Multipart _ -> `Multipart
let download_strategy = function Get _ -> `Get | Ranged _ -> `Ranged

let upload_bytes_transferred = function
  | Put result -> result.bytes_transferred
  | Multipart result -> result.bytes_transferred

let download_bytes_transferred = function
  | Get result -> result.bytes_transferred
  | Ranged result -> result.bytes_transferred

let progress ~direction ~phase ~transferred ?total ?part_number () =
  { direction; phase; transferred; total; part_number }

let customer_key_of_encryption = function
  | Some (Encryption.Destination.Sse_c customer_key) -> Some customer_key
  | Some (Sse_s3 | Sse_kms _ | Dsse_kms _) | None -> None

let derived_upload_stage_options ?content_type ?(metadata = Metadata.empty)
    ?storage_class ?(tags = Tag.Set.empty) ?cache_control ?content_encoding
    ?content_disposition ?(preconditions = Object.Preconditions.Write.none)
    ?encryption ?expected_bucket_owner () =
  let customer_key = customer_key_of_encryption encryption in
  let put_options =
    Object.Put.options ?content_type ~metadata ?storage_class ~tags
      ?cache_control ?content_encoding ?content_disposition ~preconditions
      ?encryption ?expected_bucket_owner ()
  in
  let create_options =
    Multipart.Create.options ?content_type ~metadata ?storage_class ~tags
      ?cache_control ?content_encoding ?content_disposition ?encryption
      ?expected_bucket_owner ()
  in
  let upload_part_options =
    Multipart.Upload_part.options ?customer_key ?expected_bucket_owner ()
  in
  let complete_options =
    Multipart.Complete.options_exn ~preconditions ?customer_key
      ?expected_bucket_owner ()
  in
  let list_parts_options =
    Multipart.List_parts.options_exn ?expected_bucket_owner ()
  in
  ( put_options,
    create_options,
    upload_part_options,
    complete_options,
    expected_bucket_owner,
    list_parts_options )

let default_upload_options =
  let ( put_options,
        create_options,
        upload_part_options,
        complete_options,
        abort_expected_bucket_owner,
        list_parts_options ) =
    derived_upload_stage_options ()
  in
  {
    multipart_threshold = default_multipart_threshold;
    part_size = default_part_size;
    concurrency = default_concurrency;
    put_options;
    create_options;
    upload_part_options;
    complete_options;
    abort_expected_bucket_owner;
    list_parts_options;
  }

let default_download_options =
  {
    multipart_threshold = default_multipart_threshold;
    part_size = default_part_size;
    concurrency = default_concurrency;
    overwrite = Replace;
    get_options = Object.Get.default_options;
  }

let validate_common ~multipart_threshold ~concurrency =
  if Int64.compare multipart_threshold 0L < 0 then
    S3_error_context.invalid ~field:"multipart_threshold"
      "multipart_threshold must be >= 0"
  else if concurrency <= 0 then
    S3_error_context.invalid ~field:"concurrency" "concurrency must be positive"
  else Ok ()

let validate_upload_part_size part_size =
  if part_size < min_part_size then
    S3_error_context.invalid ~field:"part_size"
      "part_size must be at least 5 MiB for S3 multipart upload"
  else Ok ()

let validate_download_part_size part_size =
  if part_size <= 0 then
    S3_error_context.invalid ~field:"part_size"
      "download part_size must be positive"
  else Ok ()

let planned_part_count ~content_length ~part_size =
  if Int64.equal content_length 0L then 0L
  else
    let part_size64 = Int64.of_int part_size in
    Int64.succ (Int64.div (Int64.pred content_length) part_size64)

let validate_upload_options (options : upload_options) =
  let* () =
    validate_common ~multipart_threshold:options.multipart_threshold
      ~concurrency:options.concurrency
  in
  let* () = validate_upload_part_size options.part_size in
  if Option.is_some options.create_options.checksum then
    S3_error_context.invalid ~field:"create_options.checksum"
      "multipart file helpers do not compute per-part checksums"
  else if Option.is_some options.upload_part_options.checksum then
    S3_error_context.invalid ~field:"upload_part_options.checksum"
      "multipart file helpers require per-part checksum values from low-level \
       multipart calls"
  else if Option.is_some options.complete_options.checksum then
    S3_error_context.invalid ~field:"complete_options.checksum"
      "multipart file helpers do not compute complete-object checksums"
  else if Option.is_some options.complete_options.checksum_type then
    S3_error_context.invalid ~field:"complete_options.checksum_type"
      "multipart file helpers do not compute complete-object checksums"
  else Ok ()

let validate_multipart_part_count ~content_length ~part_size =
  if Int64.compare content_length 0L < 0 then
    S3_error_context.invalid ~field:"content_length"
      "content_length must be non-negative"
  else if part_size <= 0 then
    S3_error_context.invalid ~field:"part_size" "part_size must be positive"
  else
    let part_count = planned_part_count ~content_length ~part_size in
    if Int64.compare part_count (Int64.of_int max_parts) > 0 then
      S3_error_context.invalid ~field:"part_count"
        "multipart file transfer would exceed 10000 parts"
    else Ok ()

let validate_upload_multipart_selection (options : upload_options) =
  match options.put_options.checksum with
  | Some _ ->
      S3_error_context.invalid ~field:"put_options.checksum"
        "optimized multipart file upload cannot use a single object checksum"
  | None -> Ok ()

module Plan = struct
  type upload_part = {
    part_number : Multipart.Part_number.t;
    offset : int64;
    length : int;
  }

  type download_range = {
    index : int;
    offset : int64;
    length : int;
    range : Range.t;
  }

  let validate_non_negative_content_length content_length =
    if Int64.compare content_length 0L < 0 then
      S3_error_context.invalid ~field:"content_length"
        "content_length must be non-negative"
    else Ok ()

  let validate_part_count ~content_length ~part_size =
    let count = planned_part_count ~content_length ~part_size in
    if Int64.compare count (Int64.of_int max_parts) > 0 then
      S3_error_context.invalid ~field:"part_count"
        "file transfer would exceed 10000 parts"
    else Ok ()

  let build_part_seq ~content_length ~part_size ~make =
    let part_size64 = Int64.of_int part_size in
    let count = planned_part_count ~content_length ~part_size |> Int64.to_int in
    let rec next index offset () =
      if index > count then Seq.Nil
      else
        let remaining = Int64.sub content_length offset in
        let length =
          if Int64.compare remaining part_size64 > 0 then part_size
          else Int64.to_int remaining
        in
        let part = make ~index ~offset ~length in
        Seq.Cons
          (part, next (index + 1) (Int64.add offset (Int64.of_int length)))
    in
    next 1 0L

  let upload_part_seq ~content_length ~part_size =
    let* () = validate_non_negative_content_length content_length in
    if Int64.equal content_length 0L then
      S3_error_context.invalid ~field:"content_length"
        "multipart upload planning requires a non-empty file"
    else
      let* () = validate_upload_part_size part_size in
      let* () = validate_part_count ~content_length ~part_size in
      Ok
        (build_part_seq ~content_length ~part_size
           ~make:(fun ~index ~offset ~length ->
             let part_number = Multipart.Part_number.of_int_exn index in
             { part_number; offset; length }))

  let upload_parts ~content_length ~part_size =
    let* parts = upload_part_seq ~content_length ~part_size in
    Ok (List.of_seq parts)

  let download_range_seq ~content_length ~part_size =
    let* () = validate_non_negative_content_length content_length in
    let* () = validate_download_part_size part_size in
    let* () = validate_part_count ~content_length ~part_size in
    Ok
      (build_part_seq ~content_length ~part_size
         ~make:(fun ~index ~offset ~length ->
           let finish = Int64.add offset (Int64.of_int (length - 1)) in
           let range = Range.bytes_exn ~start:offset ~finish in
           { index; offset; length; range }))

  let download_ranges ~content_length ~part_size =
    let* ranges = download_range_seq ~content_length ~part_size in
    Ok (List.of_seq ranges)
end

let validate_download_options (options : download_options) =
  let* () =
    validate_common ~multipart_threshold:options.multipart_threshold
      ~concurrency:options.concurrency
  in
  let* () = validate_download_part_size options.part_size in
  match options.get_options.range with
  | Some _ ->
      S3_error_context.invalid ~field:"get_options.range"
        "optimized download_file does not accept a caller-supplied range"
  | None -> Ok ()

let upload_options ?(multipart_threshold = default_multipart_threshold)
    ?(part_size = default_part_size) ?(concurrency = default_concurrency)
    ?content_type ?metadata ?storage_class ?tags ?cache_control
    ?content_encoding ?content_disposition ?preconditions ?encryption
    ?expected_bucket_owner () =
  let ( put_options,
        create_options,
        upload_part_options,
        complete_options,
        abort_expected_bucket_owner,
        list_parts_options ) =
    derived_upload_stage_options ?content_type ?metadata ?storage_class ?tags
      ?cache_control ?content_encoding ?content_disposition ?preconditions
      ?encryption ?expected_bucket_owner ()
  in
  let options =
    {
      multipart_threshold;
      part_size;
      concurrency;
      put_options;
      create_options;
      upload_part_options;
      complete_options;
      abort_expected_bucket_owner;
      list_parts_options;
    }
  in
  let* () = validate_upload_options options in
  Ok options

let upload_options_exn ?multipart_threshold ?part_size ?concurrency
    ?content_type ?metadata ?storage_class ?tags ?cache_control
    ?content_encoding ?content_disposition ?preconditions ?encryption
    ?expected_bucket_owner () =
  Awskit.Error.Producer.get_ok_exn
    (upload_options ?multipart_threshold ?part_size ?concurrency ?content_type
       ?metadata ?storage_class ?tags ?cache_control ?content_encoding
       ?content_disposition ?preconditions ?encryption ?expected_bucket_owner ())

let download_options ?(multipart_threshold = default_multipart_threshold)
    ?(part_size = default_part_size) ?(concurrency = default_concurrency)
    ?(overwrite = Replace) ?(get_options = Object.Get.default_options) () =
  let options =
    { multipart_threshold; part_size; concurrency; overwrite; get_options }
  in
  let* () = validate_download_options options in
  Ok options

let download_options_exn ?multipart_threshold ?part_size ?concurrency ?overwrite
    ?get_options () =
  Awskit.Error.Producer.get_ok_exn
    (download_options ?multipart_threshold ?part_size ?concurrency ?get_options
       ?overwrite ())

let upload_multipart_threshold (options : upload_options) =
  options.multipart_threshold

let upload_part_size (options : upload_options) = options.part_size
let upload_concurrency (options : upload_options) = options.concurrency
let upload_put_options (options : upload_options) = options.put_options
let upload_create_options (options : upload_options) = options.create_options
let upload_part_options (options : upload_options) = options.upload_part_options

let upload_complete_options (options : upload_options) =
  options.complete_options

let upload_abort_expected_bucket_owner (options : upload_options) =
  options.abort_expected_bucket_owner

let upload_list_parts_options (options : upload_options) =
  options.list_parts_options

let download_multipart_threshold (options : download_options) =
  options.multipart_threshold

let download_part_size (options : download_options) = options.part_size
let download_concurrency (options : download_options) = options.concurrency
let download_overwrite (options : download_options) = options.overwrite
let download_get_options (options : download_options) = options.get_options
