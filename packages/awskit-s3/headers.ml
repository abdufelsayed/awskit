open Common

open struct
  module Object = Object
end

let etag_condition_header = function
  | Object.Etag_condition.Any -> "*"
  | Etag etag -> Object.Etag.to_string etag

let add_opt_header name value headers =
  match value with None -> headers | Some value -> (name, value) :: headers

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
  []
  |> add_opt_header "if-match" (Option.map etag_condition_header p.if_match)
  |> add_time_header "x-amz-if-match-last-modified-time"
       p.if_match_last_modified_time
  |> add_opt_header "x-amz-if-match-size"
       (Option.map Int64.to_string p.if_match_size)

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
  match tags with
  | [] -> None
  | tags ->
      Some
        (tags
        |> List.map (fun (tag : Tag.t) ->
            Uri.pct_encode tag.key ^ "=" ^ Uri.pct_encode tag.value)
        |> String.concat "&")

let checksum_algorithm = function
  | `CRC32 -> "CRC32"
  | `CRC32C -> "CRC32C"
  | `CRC64NVME -> "CRC64NVME"
  | `SHA1 -> "SHA1"
  | `SHA256 -> "SHA256"

let checksum_header_name = function
  | `CRC32 -> "x-amz-checksum-crc32"
  | `CRC32C -> "x-amz-checksum-crc32c"
  | `CRC64NVME -> "x-amz-checksum-crc64nvme"
  | `SHA1 -> "x-amz-checksum-sha1"
  | `SHA256 -> "x-amz-checksum-sha256"

let checksum_request_headers = function
  | None -> []
  | Some (request : Object.Checksum.request) ->
      ("x-amz-checksum-algorithm", checksum_algorithm request.algorithm)
      ::
      (match request.value with
      | None -> []
      | Some value -> [ (checksum_header_name request.algorithm, value) ])

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
