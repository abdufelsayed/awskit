(** Internal pure helpers shared by resource modules. Not part of public API. *)

open Base

let src = Logs.Src.create "awskit-s3" ~doc:"S3"

module Log = (val Logs.src_log src : Logs.LOG)

module Let_syntax = struct
  let ( let* ) result f = Result.bind result ~f
  let ( let+ ) result f = Result.map result ~f
end

let resource_path bucket path query =
  let base = Fmt.str "/%s%s" bucket path in
  if String.is_empty query then base else base ^ "?" ^ query

let query_of_params params = Awskit.Signing.canonical_query_params params
let raw_object_key_path key = "/" ^ key

let encode_object_key_path key =
  "/" ^ Awskit.Signing.uri_encode ~encode_slash:false key

let encoded_copy_source ~bucket ~key =
  Awskit.Signing.uri_encode ~encode_slash:false (Fmt.str "/%s/%s" bucket key)

let opt_header key value headers =
  match value with Some value -> (key, value) :: headers | None -> headers

(* ── Metadata header helpers ────────────────────────────────────── *)

module Metadata_headers = struct
  let prefix = "x-amz-meta-"

  let of_response_headers headers =
    List.filter_map headers ~f:(fun (key, value) ->
        let lower_key = String.lowercase key in
        if String.is_prefix lower_key ~prefix then
          Some (String.drop_prefix key (String.length prefix), value)
        else None)

  let to_headers (metadata : Metadata.t) =
    List.map metadata ~f:(fun (key, value) -> (prefix ^ key, value))
end

module Validation = struct
  let invalid fmt = Fmt.kstr (fun msg -> Error (`Invalid_request msg)) fmt

  let has_ctl_or_del s =
    String.exists s ~f:(fun c ->
        let code = Char.to_int c in
        code < 0x20 || code = 0x7F)

  let ensure_present ~field value =
    if String.is_empty value then invalid "%s must be non-empty" field
    else Ok ()

  let ensure_no_ctl ~field value =
    if has_ctl_or_del value then
      invalid "%s must not contain control characters" field
    else Ok ()

  let ensure_no_surrounding_whitespace ~field value =
    if String.equal value (String.strip value) then Ok ()
    else invalid "%s must not have leading/trailing whitespace" field

  let validate_bucket bucket =
    let open Let_syntax in
    let* () = ensure_present ~field:"bucket" bucket in
    let* () = ensure_no_ctl ~field:"bucket" bucket in
    let* () = ensure_no_surrounding_whitespace ~field:"bucket" bucket in
    let len = String.length bucket in
    let is_lowercase_letter = function 'a' .. 'z' -> true | _ -> false in
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let is_alnum c = is_lowercase_letter c || is_digit c in
    let is_bucket_char = function '.' | '-' -> true | c -> is_alnum c in
    let looks_like_ipv4 =
      let parts = String.split bucket ~on:'.' in
      List.length parts = 4
      && List.for_all parts ~f:(fun part ->
          (not (String.is_empty part))
          && String.for_all part ~f:is_digit
          &&
          match Int.of_string_opt part with
          | Some n -> n >= 0 && n <= 255
          | None -> false)
    in
    let has_bad_dot_dash_pair =
      String.is_substring bucket ~substring:".."
      || String.is_substring bucket ~substring:".-"
      || String.is_substring bucket ~substring:"-."
    in
    if len < 3 || len > 63 then invalid "bucket must be 3-63 characters"
    else if not (String.for_all bucket ~f:is_bucket_char) then
      invalid
        "bucket must contain only lowercase letters, digits, dots, and hyphens"
    else if not (is_alnum bucket.[0] && is_alnum bucket.[len - 1]) then
      invalid "bucket must start and end with a lowercase letter or digit"
    else if has_bad_dot_dash_pair then
      invalid "bucket must not contain adjacent dots or dot-hyphen pairs"
    else if looks_like_ipv4 then
      invalid "bucket must not be formatted as an IPv4 address"
    else if String.is_prefix bucket ~prefix:"xn--" then
      invalid "bucket must not start with xn--"
    else if String.is_prefix bucket ~prefix:"sthree-" then
      invalid "bucket must not start with sthree-"
    else if String.is_prefix bucket ~prefix:"amzn-s3-demo-" then
      invalid "bucket must not start with amzn-s3-demo-"
    else if String.is_suffix bucket ~suffix:"-s3alias" then
      invalid "bucket must not end with -s3alias"
    else if String.is_suffix bucket ~suffix:"--ol-s3" then
      invalid "bucket must not end with --ol-s3"
    else if String.is_suffix bucket ~suffix:".mrap" then
      invalid "bucket must not end with .mrap"
    else if String.is_suffix bucket ~suffix:"--x-s3" then
      invalid "bucket must not end with --x-s3"
    else if String.is_suffix bucket ~suffix:"--table-s3" then
      invalid "bucket must not end with --table-s3"
    else Ok ()

  let validate_key key =
    let open Let_syntax in
    let* () = ensure_present ~field:"key" key in
    let* () = ensure_no_ctl ~field:"key" key in
    Ok ()

  let validate_header_value ~field value =
    let open Let_syntax in
    let* () = ensure_present ~field value in
    let* () = ensure_no_ctl ~field value in
    Ok ()

  let is_http_token_char = function
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~'
    | '0' .. '9'
    | 'A' .. 'Z'
    | 'a' .. 'z' ->
        true
    | _ -> false

  let validate_metadata_key key =
    let open Let_syntax in
    let* () = ensure_present ~field:"metadata key" key in
    let* () = ensure_no_ctl ~field:"metadata key" key in
    if String.is_prefix (String.lowercase key) ~prefix:Metadata_headers.prefix
    then invalid "metadata keys must not include the x-amz-meta- prefix"
    else if String.for_all key ~f:is_http_token_char then Ok ()
    else
      invalid
        "metadata keys must be valid HTTP token characters (no spaces or \
         separators)"

  let combine a b =
    match (a, b) with
    | Ok (), Ok () -> Ok ()
    | (Error _ as error), _ -> error
    | _, (Error _ as error) -> error

  let validate_metadata (metadata : Metadata.t) =
    List.fold metadata ~init:(Ok ()) ~f:(fun acc (key, value) ->
        combine acc
          (let open Let_syntax in
           let* () = validate_metadata_key key in
           validate_header_value
             ~field:(Fmt.str "metadata value for %s" key)
             value))

  let validate_tag (tag : Tag.t) =
    let open Let_syntax in
    let* () = ensure_present ~field:"tag key" tag.key in
    let* () = ensure_no_ctl ~field:"tag key" tag.key in
    let* () = ensure_no_ctl ~field:"tag value" tag.value in
    Ok ()

  let validate_tags tags =
    List.fold tags ~init:(Ok ()) ~f:(fun acc tag ->
        combine acc (validate_tag tag))

  let validate_range_header_value name = function
    | None -> Ok ()
    | Some value -> validate_header_value ~field:name value

  let validate_max_keys max_keys =
    if max_keys <= 0 then invalid "max_keys must be positive"
    else if max_keys > 1000 then invalid "max_keys must be <= 1000"
    else Ok ()

  let validate_delete_batch_keys keys =
    if List.length keys > 1000 then
      invalid "delete_batch supports at most 1000 keys"
    else
      List.fold keys ~init:(Ok ()) ~f:(fun acc key ->
          match acc with Error _ as error -> error | Ok () -> validate_key key)

  let validate_part_number part_number =
    if part_number <= 0 then invalid "part_number must be positive"
    else if part_number > 10_000 then invalid "part_number must be <= 10000"
    else Ok ()

  let validate_upload_id upload_id =
    let open Let_syntax in
    let* () = ensure_present ~field:"upload_id" upload_id in
    let* () = ensure_no_ctl ~field:"upload_id" upload_id in
    Ok ()

  let validate_completed_parts parts =
    let open Let_syntax in
    let rec loop previous = function
      | [] -> Ok ()
      | (part : Multipart.Part.t) :: rest -> (
          let* () = validate_part_number part.part_number in
          let* () = validate_header_value ~field:"etag" part.etag in
          match previous with
          | Some prev when part.part_number <= prev ->
              invalid
                "multipart parts must be strictly increasing by part_number"
          | _ -> loop (Some part.part_number) rest)
    in
    if List.is_empty parts then invalid "complete requires at least one part"
    else loop None parts
end

(* ── XML helpers ────────────────────────────────────────────────── *)

module Xml = struct
  let root_of_string s =
    try
      let _, nodes = Ezxmlm.from_string s in
      match List.hd nodes with
      | Some node -> Ok node
      | None -> Error (`Invalid_xml "empty XML document")
    with exn -> Error (`Invalid_xml (Exn.to_string exn))

  let decode s ~name of_xml =
    match root_of_string s with
    | Error _ as error -> error
    | Ok xml -> (
        try Ok (of_xml xml)
        with exn ->
          Error (`Invalid_response (Fmt.str "%s: %s" name (Exn.to_string exn))))
end

(* ── XML fragment helpers ───────────────────────────────────────── *)

let set_xml_name name = function
  | `El (((ns, _), attrs), elems) -> `El (((ns, name), attrs), elems)
  | fragment -> fragment
