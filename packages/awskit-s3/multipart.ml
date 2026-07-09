module Object = Object

let ( let* ) = S3_result.( let* )

module Upload_id = struct
  type t = string

  let of_string value =
    if value = "" then
      S3_error_context.invalid ~field:"upload_id" "upload id must be non-empty"
    else if S3_string.has_ctl_or_del value then
      S3_error_context.invalid ~field:"upload_id"
        "upload id contains control characters"
    else Ok value

  let of_string_exn value = S3_result.result_exn (of_string value)
  let to_string value = value
  let pp fmt value = Format.pp_print_string fmt value
  let equal = String.equal
end

module Part_number = struct
  type t = int

  let of_int value =
    if value <= 0 then
      S3_error_context.invalid ~field:"part_number"
        "part number must be positive"
    else if value > 10_000 then
      S3_error_context.invalid ~field:"part_number"
        "part number must be <= 10000"
    else Ok value

  let of_int_exn value = S3_result.result_exn (of_int value)
  let to_int value = value
  let pp fmt value = Format.pp_print_int fmt value
  let equal = Int.equal
end

module Part_number_marker = struct
  type t = int

  let of_int value =
    if value < 0 then
      S3_error_context.invalid ~field:"part_number_marker"
        "part number marker must be non-negative"
    else if value > 10_000 then
      S3_error_context.invalid ~field:"part_number_marker"
        "part number marker must be <= 10000"
    else Ok value

  let of_int_exn value = S3_result.result_exn (of_int value)
  let to_int value = value
  let pp fmt value = Format.pp_print_int fmt value
  let equal = Int.equal
end

module Upload = struct
  type created
  type caller_owned

  type +'ownership t = {
    bucket : Bucket_name.t;
    key : Object_key.t;
    upload_id : Upload_id.t;
  }

  let created ~bucket ~key ~upload_id = { bucket; key; upload_id }
  let resume ~bucket ~key ~upload_id = { bucket; key; upload_id }

  let of_strings ~bucket ~key ~upload_id =
    let* bucket = Bucket_name.of_string bucket in
    let* key = Object_key.of_string key in
    Ok (resume ~bucket ~key ~upload_id)

  let of_strings_exn ~bucket ~key ~upload_id =
    S3_result.result_exn (of_strings ~bucket ~key ~upload_id)

  let bucket upload = upload.bucket
  let key upload = upload.key
  let upload_id upload = upload.upload_id

  let as_caller_owned upload =
    { bucket = upload.bucket; key = upload.key; upload_id = upload.upload_id }
end

module Part = struct
  type t = {
    part_number : Part_number.t;
    etag : Object.Etag.t;
    checksum : Object.Checksum.value option;
    size : int64 option;
  }

  let create ?checksum ?size ~part_number ~etag () =
    match size with
    | Some size when Int64.compare size 0L < 0 ->
        S3_error_context.invalid ~field:"size" "part size must be non-negative"
    | _ -> Ok { part_number; etag; checksum; size }

  let create_exn ?checksum ?size ~part_number ~etag () =
    S3_result.result_exn (create ?checksum ?size ~part_number ~etag ())

  let part_number part = part.part_number
  let etag part = part.etag
  let checksum part = part.checksum
  let size part = part.size
end

module Create = struct
  type options = {
    content_type : Content_type.t option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.Set.t;
    checksum_algorithm : Object.Checksum.Algorithm.t option;
    checksum_type : Object.Checksum.Type.t option;
    encryption : Encryption.Destination.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type result = {
    upload : Upload.created Upload.t;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      content_type = None;
      metadata = Metadata.empty;
      storage_class = None;
      tags = Tag.Set.empty;
      checksum_algorithm = None;
      checksum_type = None;
      encryption = None;
      expected_bucket_owner = None;
    }

  let options ?content_type ?(metadata = Metadata.empty) ?storage_class
      ?(tags = Tag.Set.empty) ?checksum_algorithm ?checksum_type ?encryption
      ?expected_bucket_owner () =
    Ok
      {
        content_type;
        metadata;
        storage_class;
        tags;
        checksum_algorithm;
        checksum_type;
        encryption;
        expected_bucket_owner;
      }

  let options_exn ?content_type ?metadata ?storage_class ?tags
      ?checksum_algorithm ?checksum_type ?encryption ?expected_bucket_owner () =
    S3_result.result_exn
      (options ?content_type ?metadata ?storage_class ?tags ?checksum_algorithm
         ?checksum_type ?encryption ?expected_bucket_owner ())
end

module Upload_part = struct
  type options = {
    checksum : Object.Checksum.value option;
    customer_key : Encryption.Customer_key.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type result = {
    part : Part.t;
    checksum : Object.Checksum.response;
    response : Awskit.Response.t;
  }

  let default_options =
    { checksum = None; customer_key = None; expected_bucket_owner = None }

  let options ?checksum ?customer_key ?expected_bucket_owner () =
    Ok { checksum; customer_key; expected_bucket_owner }

  let options_exn ?checksum ?customer_key ?expected_bucket_owner () =
    S3_result.result_exn
      (options ?checksum ?customer_key ?expected_bucket_owner ())
end

module Complete = struct
  type options = {
    expected_bucket_owner : Account_id.t option;
    checksum : Object.Checksum.value option;
    checksum_type : Object.Checksum.Type.t option;
    customer_key : Encryption.Customer_key.t option;
    multipart_object_size : int64 option;
  }

  type result = {
    etag : Object.Etag.t option;
    version_id : Object.Version_id.t option;
    checksum : Object.Checksum.response;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      expected_bucket_owner = None;
      checksum = None;
      checksum_type = None;
      customer_key = None;
      multipart_object_size = None;
    }

  let options ?expected_bucket_owner ?checksum ?checksum_type ?customer_key
      ?multipart_object_size () =
    let* () =
      match multipart_object_size with
      | Some size when Int64.compare size 0L < 0 ->
          S3_error_context.invalid ~field:"multipart_object_size"
            "multipart object size must be non-negative"
      | _ -> Ok ()
    in
    Ok
      {
        expected_bucket_owner;
        checksum;
        checksum_type;
        customer_key;
        multipart_object_size;
      }

  let options_exn ?expected_bucket_owner ?checksum ?checksum_type ?customer_key
      ?multipart_object_size () =
    S3_result.result_exn
      (options ?expected_bucket_owner ?checksum ?checksum_type ?customer_key
         ?multipart_object_size ())
end

module Abort = struct
  type options = { expected_bucket_owner : Account_id.t option }
  type result = { response : Awskit.Response.t }

  let default_options = { expected_bucket_owner = None }
  let options ?expected_bucket_owner () = Ok { expected_bucket_owner }

  let options_exn ?expected_bucket_owner () =
    S3_result.result_exn (options ?expected_bucket_owner ())
end

module List_parts = struct
  type options = {
    max_parts : int option;
    part_number_marker : Part_number_marker.t option;
    expected_bucket_owner : Account_id.t option;
  }

  type part_info = {
    part_number : Part_number.t;
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
    checksum : Object.Checksum.response;
  }

  type page = {
    parts : part_info list;
    is_truncated : bool;
    next_part_number_marker : Part_number_marker.t option;
    checksum_type : Object.Checksum.Type.observed option;
    response : Awskit.Response.t;
  }

  let default_options =
    {
      max_parts = None;
      part_number_marker = None;
      expected_bucket_owner = None;
    }

  let validate_max_parts = function
    | None -> Ok ()
    | Some value when value > 0 && value <= 1000 -> Ok ()
    | Some _ ->
        S3_error_context.invalid ~field:"max_parts"
          "max_parts must be between 1 and 1000"

  let options ?max_parts ?part_number_marker ?expected_bucket_owner () =
    let* () = validate_max_parts max_parts in
    Ok { max_parts; part_number_marker; expected_bucket_owner }

  let options_exn ?max_parts ?part_number_marker ?expected_bucket_owner () =
    S3_result.result_exn
      (options ?max_parts ?part_number_marker ?expected_bucket_owner ())
end
