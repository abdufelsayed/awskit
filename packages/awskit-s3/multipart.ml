open Common
module Object = Object

module Upload_id = struct
  type t = string

  let of_string value =
    if value = "" then invalid ~field:"upload_id" "upload id must be non-empty"
    else if has_ctl_or_del value then
      invalid ~field:"upload_id" "upload id contains control characters"
    else Ok value

  let of_string_exn value = result_exn (of_string value)
  let to_string value = value
end

module Upload = struct
  type t = { bucket : string; key : string; upload_id : Upload_id.t }

  let create ~bucket ~key ~upload_id =
    match validate_bucket_key bucket key with
    | Error _ as error -> error
    | Ok () -> Ok { bucket; key; upload_id }

  let create_exn ~bucket ~key ~upload_id =
    result_exn (create ~bucket ~key ~upload_id)
end

module Part = struct
  type t = { part_number : int; etag : Object.Etag.t }

  let create ~part_number ~etag =
    if part_number <= 0 then
      invalid ~field:"part_number" "part number must be positive"
    else if part_number > 10_000 then
      invalid ~field:"part_number" "part number must be <= 10000"
    else Ok { part_number; etag }

  let create_exn ~part_number ~etag = result_exn (create ~part_number ~etag)
end

module Create = struct
  type options = {
    content_type : string option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.t list;
    checksum : Object.Checksum.request option;
    server_side_encryption : Object.Encryption.request option;
  }

  type result = { upload : Upload.t; request : Awskit.Response.t }

  let default_options =
    {
      content_type = None;
      metadata = [];
      storage_class = None;
      tags = [];
      checksum = None;
      server_side_encryption = None;
    }
end

module Upload_part = struct
  type options = { checksum : Object.Checksum.request option }

  type result = {
    part : Part.t;
    checksum : Object.Checksum.response option;
    request : Awskit.Response.t;
  }

  let default_options = { checksum = None }
end

module Complete = struct
  type result = {
    etag : Object.Etag.t option;
    version_id : Object.Version_id.t option;
    checksum : Object.Checksum.response option;
    request : Awskit.Response.t;
  }
end

module List_parts = struct
  type options = { max_parts : int option; part_number_marker : int option }

  type part_info = {
    part_number : int;
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
    checksum : Object.Checksum.response option;
  }

  type page = {
    parts : part_info list;
    is_truncated : bool;
    next_part_number_marker : int option;
    request : Awskit.Response.t;
  }

  let default_options = { max_parts = None; part_number_marker = None }
end

module Managed = struct
  let min_part_size = 5 * 1024 * 1024
  let default_part_size = 8 * 1024 * 1024
  let max_parts = 10_000

  type options = {
    part_size : int;
    create_options : Create.options;
    upload_part_options : Upload_part.options;
  }

  type result = {
    upload : Upload.t;
    parts : Part.t list;
    complete : Complete.result;
  }

  let default_options =
    {
      part_size = default_part_size;
      create_options = Create.default_options;
      upload_part_options = Upload_part.default_options;
    }

  let validate_options (options : options) =
    if options.part_size < min_part_size then
      invalid ~field:"part_size" "part size must be at least 5 MiB"
    else Ok ()
end
