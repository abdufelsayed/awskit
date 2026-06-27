let validate_header_value ~field value =
  if String.equal value "" then
    S3_error_context.invalid ~field "%s must be non-empty" field
  else if S3_string.has_ctl_or_del value then
    S3_error_context.invalid ~field "%s contains control characters" field
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
