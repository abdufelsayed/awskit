let validate_header_value ~field value =
  if String.equal value "" then
    S3_error_context.invalid ~field "%s must be non-empty" field
  else if S3_string.has_ctl_or_del value then
    S3_error_context.invalid ~field "%s contains control characters" field
  else Ok ()

(* S3 documents a 5 GiB multipart-part ceiling. Awskit models the service and
   SDK single-request byte ceiling with the same exact limit. Keep both limits
   in one runtime-neutral module so transfer planning and primitive request
   validation cannot drift. *)
let max_part_size = Int64.mul 5L (Int64.mul 1024L (Int64.mul 1024L 1024L))
let max_single_request_size = max_part_size

let validate_upload_part_content_length content_length =
  if Int64.compare content_length max_part_size > 0 then
    S3_error_context.invalid ~field:"content_length"
      "multipart upload parts must be at most 5 GiB (5368709120 bytes)"
  else Ok ()

let validate_put_object_content_length content_length =
  if Int64.compare content_length max_single_request_size > 0 then
    S3_error_context.invalid ~field:"content_length"
      "PutObject bodies must be at most 5 GiB (5368709120 bytes)"
  else Ok ()

let validate_metadata metadata =
  Metadata.of_list (Metadata.to_list metadata) |> Result.map ignore

let validate_tag tag =
  Tag.create ~key:(Tag.key tag) ~value:(Tag.value tag) |> Result.map ignore

let validate_tags tags =
  let ( let* ) = S3_result.( let* ) in
  let tags = Tag.Set.to_list tags in
  let rec loop = function
    | [] -> Tag.Set.of_list tags
    | tag :: rest ->
        let* () = validate_tag tag in
        loop rest
  in
  loop tags |> Result.map ignore

let validate_bucket bucket = Bucket_name.of_string bucket |> Result.map ignore
let validate_key key = Object_key.of_string key |> Result.map ignore

let validate_bucket_key bucket key =
  let ( let* ) = S3_result.( let* ) in
  let* () = validate_bucket bucket in
  validate_key key
