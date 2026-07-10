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
  module Checksum = struct
    type t = {
      algorithm : Object.Checksum.Algorithm.t;
      checksum_type : Object.Checksum.Type.t option;
    }

    let supported algorithm = function
      | None -> true
      | Some Object.Checksum.Type.Composite ->
          algorithm <> Object.Checksum.Algorithm.Crc64nvme
      | Some Object.Checksum.Type.Full_object -> (
          match algorithm with
          | Object.Checksum.Algorithm.Crc32 | Object.Checksum.Algorithm.Crc32c
          | Object.Checksum.Algorithm.Crc64nvme ->
              true
          | Object.Checksum.Algorithm.Sha1 | Object.Checksum.Algorithm.Sha256
          | Object.Checksum.Algorithm.Sha512 | Object.Checksum.Algorithm.Md5
          | Object.Checksum.Algorithm.Xxhash64
          | Object.Checksum.Algorithm.Xxhash3
          | Object.Checksum.Algorithm.Xxhash128 ->
              false)

    let create ~algorithm ?checksum_type () =
      if supported algorithm checksum_type then Ok { algorithm; checksum_type }
      else
        S3_error_context.invalid ~field:"checksum_type"
          "checksum type %s is not supported with algorithm %s"
          (Option.fold ~none:"service default"
             ~some:Object.Checksum.Type.to_string checksum_type)
          (Object.Checksum.Algorithm.to_string algorithm)

    let create_exn ~algorithm ?checksum_type () =
      S3_result.result_exn (create ~algorithm ?checksum_type ())
  end

  type options = {
    content_type : Content_type.t option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.Set.t;
    checksum : Checksum.t option;
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
      checksum = None;
      encryption = None;
      expected_bucket_owner = None;
    }

  let options ?content_type ?(metadata = Metadata.empty) ?storage_class
      ?(tags = Tag.Set.empty) ?checksum ?encryption ?expected_bucket_owner () =
    {
      content_type;
      metadata;
      storage_class;
      tags;
      checksum;
      encryption;
      expected_bucket_owner;
    }
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
    { checksum; customer_key; expected_bucket_owner }
end

module Complete = struct
  module Parts = struct
    type t = { values : Part.t list; multipart_object_size : int64 option }

    let min_nonfinal_part_size = 5_242_880L

    let validate_object_size = function
      | Some size when Int64.compare size 0L < 0 ->
          S3_error_context.invalid ~field:"multipart_object_size"
            "multipart object size must be non-negative"
      | _ -> Ok ()

    let validate_expected_size multipart_object_size total all_sizes_known =
      match multipart_object_size with
      | Some expected when all_sizes_known && not (Int64.equal expected total)
        ->
          S3_error_context.invalid ~field:"multipart_object_size"
            "multipart object size does not match completed part sizes"
      | _ -> Ok ()

    let validate_checksum_algorithm expected part =
      match (expected, Part.checksum part) with
      | None, None -> Ok None
      | None, Some checksum -> Ok (Some checksum.algorithm)
      | Some algorithm, None -> Ok (Some algorithm)
      | Some algorithm, Some checksum when algorithm = checksum.algorithm ->
          Ok (Some algorithm)
      | Some _, Some _ ->
          S3_error_context.invalid ~field:"checksum_algorithm"
            "completed part checksums must use one algorithm"

    let of_list ?multipart_object_size values =
      let* () = validate_object_size multipart_object_size in
      let checksummed_parts =
        List.exists (fun part -> Option.is_some (Part.checksum part)) values
      in
      let rec loop previous checksum_algorithm total all_sizes_known = function
        | [] ->
            let* () =
              validate_expected_size multipart_object_size total all_sizes_known
            in
            Ok { values; multipart_object_size }
        | part :: rest ->
            let part_number = Part.part_number part |> Part_number.to_int in
            let* () =
              if
                match previous with
                | Some previous -> part_number <= previous
                | None -> false
              then
                S3_error_context.invalid ~field:"part_number"
                  "parts must be sorted by part_number"
              else Ok ()
            in
            let* () =
              if
                checksummed_parts
                &&
                match previous with
                | None -> part_number <> 1
                | Some previous -> part_number <> previous + 1
              then
                S3_error_context.invalid ~field:"part_number"
                  "parts with checksums must use consecutive part numbers \
                   starting at 1"
              else Ok ()
            in
            let* checksum_algorithm =
              validate_checksum_algorithm checksum_algorithm part
            in
            let* () =
              match (rest, Part.size part) with
              | _ :: _, Some size
                when Int64.compare size min_nonfinal_part_size < 0 ->
                  S3_error_context.invalid ~field:"parts"
                    "non-final multipart parts must be at least 5 MiB"
              | _ -> Ok ()
            in
            let total, all_sizes_known =
              match Part.size part with
              | None -> (total, false)
              | Some size -> (Int64.add total size, all_sizes_known)
            in
            loop (Some part_number) checksum_algorithm total all_sizes_known
              rest
      in
      match values with
      | [] ->
          S3_error_context.invalid ~field:"parts"
            "complete requires at least one part"
      | _ -> loop None None 0L true values

    let of_list_exn ?multipart_object_size values =
      S3_result.result_exn (of_list ?multipart_object_size values)

    let to_list parts = parts.values
    let multipart_object_size parts = parts.multipart_object_size
  end

  type options = {
    expected_bucket_owner : Account_id.t option;
    checksum : Object.Checksum.value option;
    checksum_type : Object.Checksum.Type.t option;
    customer_key : Encryption.Customer_key.t option;
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
    }

  let options ?expected_bucket_owner ?checksum ?checksum_type ?customer_key () =
    let* () =
      match checksum with
      | Some (checksum : Object.Checksum.value)
        when not (Create.Checksum.supported checksum.algorithm checksum_type) ->
          S3_error_context.invalid ~field:"checksum_type"
            "checksum type %s is not supported with algorithm %s"
            (Option.fold ~none:"service default"
               ~some:Object.Checksum.Type.to_string checksum_type)
            (Object.Checksum.Algorithm.to_string checksum.algorithm)
      | _ -> Ok ()
    in
    Ok { expected_bucket_owner; checksum; checksum_type; customer_key }

  let options_exn ?expected_bucket_owner ?checksum ?checksum_type ?customer_key
      () =
    S3_result.result_exn
      (options ?expected_bucket_owner ?checksum ?checksum_type ?customer_key ())
end

module Abort = struct
  type result = { response : Awskit.Response.t }
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
    | Some value when value >= 0 && value <= 1000 -> Ok ()
    | Some _ ->
        S3_error_context.invalid ~field:"max_parts"
          "max_parts must be between 0 and 1000"

  let options ?max_parts ?part_number_marker ?expected_bucket_owner () =
    let* () = validate_max_parts max_parts in
    Ok { max_parts; part_number_marker; expected_bucket_owner }

  let options_exn ?max_parts ?part_number_marker ?expected_bucket_owner () =
    S3_result.result_exn
      (options ?max_parts ?part_number_marker ?expected_bucket_owner ())
end
