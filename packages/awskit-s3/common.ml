module Credentials = Awskit.Credentials
module Endpoint = Awskit.Endpoint
module Region = Awskit.Region

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as e -> e

let result_exn = Awskit.Error.Internal.get_ok_exn

let invalid ?field fmt =
  Fmt.kstr
    (fun message -> Error (Awskit.Error.Internal.validation ?field message))
    fmt

let decode fmt = Fmt.kstr Awskit.Error.Internal.decode fmt

let decode_with_context ~what message =
  Awskit.Error.Internal.decode message
  |> Awskit.Error.Internal.with_context (Fmt.str "decoding %s" what)

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

let int64_of_string_opt value =
  try Some (Int64.of_string value) with Failure _ -> None

let non_negative_int_of_string_opt value =
  match int_of_string_opt value with
  | Some value when value >= 0 -> Some value
  | _ -> None

let non_negative_int64_of_string_opt value =
  match int64_of_string_opt value with
  | Some value when Int64.compare value 0L >= 0 -> Some value
  | _ -> None

let option_map_result f = function
  | None -> Ok None
  | Some value -> Result.map Option.some (f value)

let ptime_of_string value =
  match Ptime.of_rfc3339 ~strict:false value with
  | Ok (time, _, _) -> Some time
  | Error _ -> None

let ptime_to_header value = Ptime.to_rfc3339 value

let s3_uri ?key bucket =
  match key with
  | None -> Fmt.str "s3://%s" bucket
  | Some key -> Fmt.str "s3://%s/%s" bucket key

let with_s3_operation ~operation ?bucket ?key error =
  let resource = Option.map (fun bucket -> s3_uri ?key bucket) bucket in
  let already_present =
    List.exists
      (function
        | Awskit.Error.Operation
            { service = Some "S3"; name; resource = existing } ->
            String.equal name operation && existing = resource
        | _ -> false)
      (Awskit.Error.context error)
  in
  if already_present then error
  else
    Awskit.Error.Internal.with_operation ~service:"S3" ~name:operation ?resource
      () error

let return_s3_error return_error ~operation ?bucket ?key error =
  return_error (with_s3_operation ~operation ?bucket ?key error)

module Xml = struct
  let el name children = `El ((("", name), []), children)
  let text name value = el name [ `Data value ]
  let to_string node = Ezxmlm.to_string [ node ]

  let root body =
    try
      let _, nodes = Ezxmlm.from_string body in
      match
        List.find_map
          (function
            | `El (((_, name), _), children) -> Some (name, children)
            | _ -> None)
          nodes
      with
      | Some root -> Ok root
      | None ->
          Error (decode_with_context ~what:"XML document" "empty XML document")
    with exn ->
      Error (decode_with_context ~what:"XML document" (Printexc.to_string exn))

  let children name nodes =
    List.filter_map
      (function
        | `El (((_, child_name), _), children) when child_name = name ->
            Some children
        | _ -> None)
      nodes

  let rec text_content nodes =
    nodes
    |> List.map (function
      | `Data data -> data
      | `El ((_, _), children) -> text_content children)
    |> String.concat ""

  let child name nodes =
    match children name nodes with [] -> None | x :: _ -> Some x

  let child_text name nodes = Option.map text_content (child name nodes)
  let child_texts name nodes = List.map text_content (children name nodes)

  let decode_field_error ~path fmt =
    Fmt.kstr
      (fun message ->
        Error
          (decode_with_context ~what:path
             (Fmt.str "invalid XML field: %s" message)))
      fmt

  let required_child_text ~path name nodes =
    match child_text name nodes with
    | Some value -> Ok value
    | None -> decode_field_error ~path "missing required <%s>" name

  let optional_child_parse ~path name parse nodes =
    match child_text name nodes with
    | None -> Ok None
    | Some value -> (
        match parse value with
        | Some parsed -> Ok (Some parsed)
        | None ->
            decode_field_error ~path "<%s> has invalid value %S" name value)

  let optional_child_result ~path name parse nodes =
    match child_text name nodes with
    | None -> Ok None
    | Some value -> (
        match parse value with
        | Ok parsed -> Ok (Some parsed)
        | Error error ->
            decode_field_error ~path "<%s> has invalid value %S: %s" name value
              (Awskit.Error.to_string_hum error))

  let children_result name nodes ~f =
    let rec loop index acc = function
      | [] -> Ok (List.rev acc)
      | child :: rest -> (
          match f index child with
          | Error _ as error -> error
          | Ok value -> loop (index + 1) (value :: acc) rest)
    in
    loop 0 [] (children name nodes)

  let decode_root body ~name =
    match root body with
    | Error _ as error -> error
    | Ok (actual, children) when String.equal actual name -> Ok children
    | Ok (actual, _) ->
        Error
          (decode_with_context ~what:"XML document"
             (Fmt.str "expected %s root element, got %s" name actual))

  let service_code body =
    match root body with
    | Error _ -> None
    | Ok (_, nodes) -> child_text "Code" nodes

  let service_message body =
    match root body with
    | Error _ -> None
    | Ok (_, nodes) -> child_text "Message" nodes
end

module Error = struct
  type t = Awskit.Error.t

  let pp = Awskit.Error.pp
  let equal = Awskit.Error.equal
  let to_string_hum = Awskit.Error.to_string_hum
  let service_code = Awskit.Error.service_code

  let code_is expected error =
    match service_code error with
    | None -> false
    | Some code -> String.lowercase_ascii code = String.lowercase_ascii expected

  let is_not_found = Awskit.Error.is_not_found
  let is_no_such_bucket error = code_is "NoSuchBucket" error
  let is_no_such_key error = code_is "NoSuchKey" error

  let is_precondition_failed error =
    Awskit.Error.service_status error = Some 412
    || code_is "PreconditionFailed" error

  let is_conditional_request_conflict error =
    code_is "ConditionalRequestConflict" error

  let is_conditional_failure error =
    is_precondition_failed error || is_conditional_request_conflict error
end

module Metadata = Metadata

module Metadata_headers = struct
  let prefix = "x-amz-meta-"

  let of_headers headers =
    List.filter_map
      (fun (key, value) ->
        let lower = String.lowercase_ascii key in
        if is_prefix ~prefix lower then
          Some
            ( String.sub key (String.length prefix)
                (String.length key - String.length prefix),
              value )
        else None)
      headers

  let to_headers metadata = List.map (fun (k, v) -> (prefix ^ k, v)) metadata
end

module Storage_class = Storage_class
module Tag = Tag
module Range = Range

let validate_header_value ~field value =
  if value = "" then invalid ~field "%s must be non-empty" field
  else if has_ctl_or_del value then
    invalid ~field "%s contains control characters" field
  else Ok ()

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
            part <> ""
            &&
            match int_of_string_opt part with
            | Some n -> n >= 0 && n <= 255
            | None -> false)
          [ a; b; c; d ]
    | _ -> false
  in
  if len < 3 || len > 63 then
    invalid ~field:"bucket" "bucket must be 3-63 characters"
  else if not (String.for_all is_bucket_char bucket) then
    invalid ~field:"bucket"
      "bucket must contain only lowercase letters, digits, dots, and hyphens"
  else if not (is_alnum bucket.[0] && is_alnum bucket.[len - 1]) then
    invalid ~field:"bucket"
      "bucket must start and end with a lowercase letter or digit"
  else if has_ctl_or_del bucket then
    invalid ~field:"bucket" "bucket contains control characters"
  else if bad_dot_dash then
    invalid ~field:"bucket"
      "bucket must not contain adjacent dots or dot-hyphen pairs"
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
