module Bucket_name = Awskit_s3.Bucket_name
module Metadata = Awskit_s3.Metadata
module Object_key = Awskit_s3.Object_key
module Tag = Awskit_s3.Tag

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let invalid ?field fmt =
  Fmt.kstr
    (fun message -> Error (Awskit.Error.Producer.validation ?field message))
    fmt

let decode fmt = Fmt.kstr Awskit.Error.Producer.decode fmt

let is_prefix ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.equal (String.sub value 0 prefix_len) prefix

let is_suffix ~suffix value =
  let suffix_len = String.length suffix in
  let len = String.length value in
  len >= suffix_len && String.sub value (len - suffix_len) suffix_len = suffix

let has_ctl_or_del value =
  String.exists
    (fun c ->
      let code = Char.code c in
      code < 0x20 || code = 0x7F)
    value

let int_of_string_opt value =
  try Some (int_of_string value) with Failure _ -> None

let ptime_to_header value = Ptime.to_rfc3339 value

let validate_header_value ~field value =
  if String.equal value "" then invalid ~field "%s must be non-empty" field
  else if has_ctl_or_del value then
    invalid ~field "%s contains control characters" field
  else Ok ()

module Metadata_headers = struct
  let prefix = "x-amz-meta-"
end

let validate_metadata metadata =
  Metadata.of_list (Metadata.to_list metadata) |> Result.map ignore

let validate_tag tag =
  let* () = validate_header_value ~field:"tag key" (Tag.key tag) in
  if has_ctl_or_del (Tag.value tag) then
    invalid ~field:"tag value" "tag value contains control characters"
  else Ok ()

let validate_tags tags =
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
  let* () = validate_bucket bucket in
  validate_key key
