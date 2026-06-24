open Common

let etag_condition_header = function
  | Object.Etag_condition.Any -> "*"
  | Etag etag -> Object.Etag.to_string etag

let add_opt_header name value headers =
  match value with None -> headers | Some value -> (name, value) :: headers

let add_opt_account_id_header name value headers =
  add_opt_header name (Option.map Account_id.to_string value) headers

let add_opt_content_type_header name value headers =
  add_opt_header name (Option.map Content_type.to_string value) headers

let add_time_header name value headers =
  match value with
  | None -> headers
  | Some value -> (name, ptime_to_header value) :: headers

let write_precondition_headers (p : Object.Preconditions.Write.t) =
  []
  |> add_opt_header "if-match" (Option.map etag_condition_header p.if_match)
  |> add_opt_header "if-none-match"
       (Option.map etag_condition_header p.if_none_match)

let read_precondition_headers (p : Object.Preconditions.Read.t) =
  []
  |> add_opt_header "if-match" (Option.map etag_condition_header p.if_match)
  |> add_opt_header "if-none-match"
       (Option.map etag_condition_header p.if_none_match)
  |> add_time_header "if-modified-since" p.if_modified_since
  |> add_time_header "if-unmodified-since" p.if_unmodified_since

let delete_precondition_headers (p : Object.Preconditions.Delete.t) =
  [] |> add_opt_header "if-match" (Option.map etag_condition_header p.if_match)

let copy_source_precondition_headers (p : Object.Preconditions.Copy_source.t) =
  []
  |> add_opt_header "x-amz-copy-source-if-match"
       (Option.map etag_condition_header p.if_match)
  |> add_opt_header "x-amz-copy-source-if-none-match"
       (Option.map etag_condition_header p.if_none_match)
  |> add_time_header "x-amz-copy-source-if-modified-since" p.if_modified_since
  |> add_time_header "x-amz-copy-source-if-unmodified-since"
       p.if_unmodified_since

let validate_common_headers ?content_type ?cache_control ?content_encoding
    ?content_disposition () =
  let validate_opt field = function
    | None -> Ok ()
    | Some value -> validate_header_value ~field value
  in
  let* () = validate_opt "content-type" content_type in
  let* () = validate_opt "cache-control" cache_control in
  let* () = validate_opt "content-encoding" content_encoding in
  validate_opt "content-disposition" content_disposition

let tags_header tags =
  match Tag.Set.to_list tags with
  | [] -> None
  | tags ->
      Some
        (tags
        |> List.map (fun tag ->
            Awskit.Signing.uri_encode (Tag.key tag)
            ^ "="
            ^ Awskit.Signing.uri_encode (Tag.value tag))
        |> String.concat "&")

let checksum_header_name = function
  | Object.Checksum.Algorithm.Crc32 -> Some "x-amz-checksum-crc32"
  | Crc32c -> Some "x-amz-checksum-crc32c"
  | Crc64nvme -> Some "x-amz-checksum-crc64nvme"
  | Sha1 -> Some "x-amz-checksum-sha1"
  | Sha256 -> Some "x-amz-checksum-sha256"
  | Sha512 -> Some "x-amz-checksum-sha512"
  | Md5 -> Some "x-amz-checksum-md5"
  | Xxhash64 -> Some "x-amz-checksum-xxhash64"
  | Xxhash3 -> Some "x-amz-checksum-xxhash3"
  | Xxhash128 -> Some "x-amz-checksum-xxhash128"
  | Unknown _ -> None

let validate_checksum_algorithm = function
  | Object.Checksum.Algorithm.Unknown value ->
      invalid ~field:"checksum_algorithm"
        "unknown checksum algorithm %S cannot be sent" value
  | _ -> Ok ()

let validate_checksum_type = function
  | Object.Checksum.Type.Unknown value ->
      invalid ~field:"checksum_type" "unknown checksum type %S cannot be sent"
        value
  | _ -> Ok ()

let validate_checksum_value (checksum : Object.Checksum.value) =
  validate_checksum_algorithm checksum.algorithm

let validate_storage_class = function
  | Storage_class.Unknown value ->
      invalid ~field:"storage_class" "unknown storage class %S cannot be sent"
        value
  | _ -> Ok ()

let checksum_value_headers = function
  | None -> []
  | Some (checksum : Object.Checksum.value) -> (
      match checksum_header_name checksum.algorithm with
      | None -> []
      | Some name -> [ (name, checksum.value) ])

let checksum_algorithm_header = function
  | None -> []
  | Some algorithm ->
      [
        ( "x-amz-checksum-algorithm",
          Object.Checksum.Algorithm.to_string algorithm );
      ]

let checksum_type_header = function
  | None -> []
  | Some checksum_type ->
      [ ("x-amz-checksum-type", Object.Checksum.Type.to_string checksum_type) ]

let checksum_mode_header = function
  | None -> []
  | Some mode ->
      [ ("x-amz-checksum-mode", Object.Checksum.Mode.to_string mode) ]

let multipart_object_size_header = function
  | None -> []
  | Some size -> [ ("x-amz-mp-object-size", Int64.to_string size) ]

let encryption_request_headers = function
  | None -> []
  | Some `AES256 -> [ ("x-amz-server-side-encryption", "AES256") ]
  | Some (`Aws_kms kms) ->
      let kms : Object.Encryption.kms = kms in
      ("x-amz-server-side-encryption", "aws:kms")
      :: add_opt_header "x-amz-server-side-encryption-aws-kms-key-id" kms.key_id
           []
      |> fun headers ->
      add_opt_header "x-amz-server-side-encryption-bucket-key-enabled"
        (Option.map string_of_bool kms.bucket_key_enabled)
        headers
