open Awskit_s3

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let invalid ?field fmt =
  Fmt.kstr
    (fun message -> Error (Awskit.Error.Internal.validation ?field message))
    fmt

let decode fmt = Fmt.kstr Awskit.Error.Internal.decode fmt

let is_prefix ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len && String.sub value 0 prefix_len = prefix

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
  if value = "" then invalid ~field "%s must be non-empty" field
  else if has_ctl_or_del value then
    invalid ~field "%s contains control characters" field
  else Ok ()

module Metadata_headers = struct
  let prefix = "x-amz-meta-"
end

let validate_metadata metadata =
  let validate_key key =
    if key = "" then invalid ~field:"metadata" "metadata key must be non-empty"
    else if has_ctl_or_del key then
      invalid ~field:"metadata" "metadata key contains control characters"
    else if
      is_prefix ~prefix:Metadata_headers.prefix (String.lowercase_ascii key)
    then invalid ~field:"metadata" "metadata keys must not include x-amz-meta-"
    else Ok ()
  in
  let rec loop = function
    | [] -> Ok ()
    | (key, value) :: rest ->
        let* () = validate_key key in
        let* () = validate_header_value ~field:("metadata " ^ key) value in
        loop rest
  in
  loop metadata

let validate_tag (tag : Tag.t) =
  let* () = validate_header_value ~field:"tag key" tag.key in
  if has_ctl_or_del tag.value then
    invalid ~field:"tag value" "tag value contains control characters"
  else Ok ()

let validate_tags tags =
  let rec loop = function
    | [] -> Ok ()
    | tag :: rest ->
        let* () = validate_tag tag in
        loop rest
  in
  loop tags

let validate_bucket bucket =
  let len = String.length bucket in
  let is_lower = function 'a' .. 'z' -> true | _ -> false in
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let is_alnum c = is_lower c || is_digit c in
  let is_bucket_char = function '.' | '-' -> true | c -> is_alnum c in
  let bad_dot_dash =
    let rec loop i =
      i + 1 < len
      && ((bucket.[i] = '.' && bucket.[i + 1] = '.')
         || (bucket.[i] = '.' && bucket.[i + 1] = '-')
         || (bucket.[i] = '-' && bucket.[i + 1] = '.')
         || loop (i + 1))
    in
    loop 0
  in
  let looks_like_ipv4 =
    match String.split_on_char '.' bucket with
    | [ a; b; c; d ] ->
        List.for_all
          (fun part ->
            match int_of_string_opt part with
            | Some n -> n >= 0 && n <= 255 && string_of_int n = part
            | None -> false)
          [ a; b; c; d ]
    | _ -> false
  in
  if len < 3 || len > 63 then
    invalid ~field:"bucket" "bucket must be 3-63 characters"
  else if not (is_alnum bucket.[0] && is_alnum bucket.[len - 1]) then
    invalid ~field:"bucket" "bucket must start and end with a letter or digit"
  else if not (String.for_all is_bucket_char bucket) then
    invalid ~field:"bucket" "bucket contains invalid characters"
  else if has_ctl_or_del bucket then
    invalid ~field:"bucket" "bucket contains control characters"
  else if bad_dot_dash then
    invalid ~field:"bucket" "bucket contains invalid dot/dash sequencing"
  else if looks_like_ipv4 then
    invalid ~field:"bucket" "bucket must not be formatted as an IPv4 address"
  else if is_prefix ~prefix:"xn--" bucket then
    invalid ~field:"bucket" "bucket must not start with xn--"
  else if is_prefix ~prefix:"sthree-" bucket then
    invalid ~field:"bucket" "bucket must not start with sthree-"
  else if is_prefix ~prefix:"amzn-s3-demo-" bucket then
    invalid ~field:"bucket" "bucket must not start with amzn-s3-demo-"
  else if is_suffix ~suffix:"-s3alias" bucket then
    invalid ~field:"bucket" "bucket must not end with -s3alias"
  else if is_suffix ~suffix:"--ol-s3" bucket then
    invalid ~field:"bucket" "bucket must not end with --ol-s3"
  else if is_suffix ~suffix:".mrap" bucket then
    invalid ~field:"bucket" "bucket must not end with .mrap"
  else if is_suffix ~suffix:"--x-s3" bucket then
    invalid ~field:"bucket" "bucket must not end with --x-s3"
  else if is_suffix ~suffix:"--table-s3" bucket then
    invalid ~field:"bucket" "bucket must not end with --table-s3"
  else Ok ()

let validate_key key =
  if key = "" then invalid ~field:"key" "key must be non-empty"
  else if has_ctl_or_del key then
    invalid ~field:"key" "key contains control characters"
  else Ok ()

let validate_bucket_key bucket key =
  let* () = validate_bucket bucket in
  validate_key key
