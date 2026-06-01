open Common

let min_part_size = 5 * 1024 * 1024
let default_part_size = 8 * 1024 * 1024
let max_parts = 10_000

type options = {
  part_size : int;
  create_options : Multipart.Create.options;
  upload_part_options : Multipart.Upload_part.options;
}

type result = {
  upload : Multipart.Upload.t;
  parts : Multipart.Part.t list;
  complete : Multipart.Complete.result;
}

let default_options =
  {
    part_size = default_part_size;
    create_options = Multipart.Create.default_options;
    upload_part_options = Multipart.Upload_part.default_options;
  }

let validate_options options =
  if options.part_size < min_part_size then
    invalid ~field:"part_size"
      "part_size must be at least 5 MiB for S3 multipart upload"
  else Ok ()
