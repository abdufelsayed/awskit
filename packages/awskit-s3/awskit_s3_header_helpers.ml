module type DOMAIN = sig
  module Account_id : sig
    type t

    val to_string : t -> string
  end

  module Content_type : sig
    type t

    val to_string : t -> string
  end

  module Tag : sig
    type t

    val key : t -> string
    val value : t -> string

    module Set : sig
      type tag = t
      type t

      val to_list : t -> tag list
    end
  end

  module Storage_class : sig
    type t =
      | Standard
      | Reduced_redundancy
      | Standard_ia
      | Onezone_ia
      | Intelligent_tiering
      | Glacier
      | Glacier_ir
      | Deep_archive
      | Outposts
      | Snow
      | Express_onezone
      | Fsx_openzfs
      | Fsx_ontap
      | Other of string

    val to_string : t -> string
  end

  module Encryption : sig
    module Kms : sig
      type t

      val key_id : t -> string option
      val bucket_key_enabled : t -> bool option
    end

    module Customer_key : sig
      type t

      val algorithm : t -> string
      val key_base64 : t -> string
      val key_md5_base64 : t -> string
    end

    module Destination : sig
      type t =
        | Sse_s3
        | Sse_kms of Kms.t
        | Dsse_kms of Kms.t
        | Sse_c of Customer_key.t

      val validate_request : t -> (unit, Awskit.Error.t) result
    end

    module Source : sig
      type t = Sse_c of Customer_key.t
    end
  end

  module Object : sig
    module Etag : sig
      type t

      val to_string : t -> string
    end

    module Etag_condition : sig
      type t = Any | Etag of Etag.t
    end

    module Preconditions : sig
      module Write : sig
        type t = {
          if_match : Etag_condition.t option;
          if_none_match : Etag_condition.t option;
        }
      end

      module Read : sig
        type t = {
          if_match : Etag_condition.t option;
          if_none_match : Etag_condition.t option;
          if_modified_since : Ptime.t option;
          if_unmodified_since : Ptime.t option;
        }
      end

      module Delete : sig
        type t = { if_match : Etag_condition.t option }
      end

      module Copy_source : sig
        type t = {
          if_match : Etag_condition.t option;
          if_none_match : Etag_condition.t option;
          if_modified_since : Ptime.t option;
          if_unmodified_since : Ptime.t option;
        }
      end
    end

    module Checksum : sig
      module Algorithm : sig
        type t =
          | Crc32
          | Crc32c
          | Crc64nvme
          | Sha1
          | Sha256
          | Sha512
          | Md5
          | Xxhash64
          | Xxhash3
          | Xxhash128
          | Unknown of string

        val to_string : t -> string
      end

      module Type : sig
        type t = Composite | Full_object | Unknown of string

        val to_string : t -> string
      end

      module Mode : sig
        type t

        val to_string : t -> string
      end

      type value = private { algorithm : Algorithm.t; value : string }
    end
  end
end

module type CONFIG = sig
  val ptime_to_header : Ptime.t -> string

  val validate_header_value :
    field:string -> string -> (unit, Awskit.Error.t) result
end

module Make (Domain : DOMAIN) (Config : CONFIG) = struct
  let ( let* ) result f =
    match result with Ok value -> f value | Error _ as error -> error

  let invalid ?field fmt =
    Printf.ksprintf
      (fun message -> Error (Awskit.Error.Producer.validation ?field message))
      fmt

  let etag_condition_header = function
    | Domain.Object.Etag_condition.Any -> "*"
    | Etag etag -> Domain.Object.Etag.to_string etag

  let add_opt_header name value headers =
    match value with None -> headers | Some value -> (name, value) :: headers

  let add_opt_account_id_header name value headers =
    add_opt_header name (Option.map Domain.Account_id.to_string value) headers

  let add_opt_content_type_header name value headers =
    add_opt_header name (Option.map Domain.Content_type.to_string value) headers

  let add_time_header name value headers =
    match value with
    | None -> headers
    | Some value -> (name, Config.ptime_to_header value) :: headers

  let write_precondition_headers (p : Domain.Object.Preconditions.Write.t) =
    []
    |> add_opt_header "if-match" (Option.map etag_condition_header p.if_match)
    |> add_opt_header "if-none-match"
         (Option.map etag_condition_header p.if_none_match)

  let read_precondition_headers (p : Domain.Object.Preconditions.Read.t) =
    []
    |> add_opt_header "if-match" (Option.map etag_condition_header p.if_match)
    |> add_opt_header "if-none-match"
         (Option.map etag_condition_header p.if_none_match)
    |> add_time_header "if-modified-since" p.if_modified_since
    |> add_time_header "if-unmodified-since" p.if_unmodified_since

  let delete_precondition_headers (p : Domain.Object.Preconditions.Delete.t) =
    []
    |> add_opt_header "if-match" (Option.map etag_condition_header p.if_match)

  let copy_source_precondition_headers
      (p : Domain.Object.Preconditions.Copy_source.t) =
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
      | Some value -> Config.validate_header_value ~field value
    in
    let* () = validate_opt "content-type" content_type in
    let* () = validate_opt "cache-control" cache_control in
    let* () = validate_opt "content-encoding" content_encoding in
    validate_opt "content-disposition" content_disposition

  let tags_header tags =
    match Domain.Tag.Set.to_list tags with
    | [] -> None
    | tags ->
        Some
          (tags
          |> List.map (fun tag ->
              Awskit.Signing.uri_encode (Domain.Tag.key tag)
              ^ "="
              ^ Awskit.Signing.uri_encode (Domain.Tag.value tag))
          |> String.concat "&")

  let checksum_header_name = function
    | Domain.Object.Checksum.Algorithm.Crc32 -> Some "x-amz-checksum-crc32"
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
    | Domain.Object.Checksum.Algorithm.Unknown value ->
        invalid ~field:"checksum_algorithm"
          "unknown checksum algorithm %S cannot be sent" value
    | _ -> Ok ()

  let validate_checksum_type = function
    | Domain.Object.Checksum.Type.Unknown value ->
        invalid ~field:"checksum_type" "unknown checksum type %S cannot be sent"
          value
    | _ -> Ok ()

  let validate_checksum_value (checksum : Domain.Object.Checksum.value) =
    let* () = validate_checksum_algorithm checksum.algorithm in
    Config.validate_header_value ~field:"checksum_value" checksum.value

  let validate_storage_class storage_class =
    Config.validate_header_value ~field:"storage_class"
      (Domain.Storage_class.to_string storage_class)

  let validate_destination_encryption = function
    | None -> Ok ()
    | Some encryption ->
        Domain.Encryption.Destination.validate_request encryption

  let validate_source_encryption = function
    | None | Some (Domain.Encryption.Source.Sse_c _) -> Ok ()

  let checksum_value_headers = function
    | None -> []
    | Some (checksum : Domain.Object.Checksum.value) -> (
        match checksum_header_name checksum.algorithm with
        | None -> []
        | Some name -> [ (name, checksum.value) ])

  let checksum_algorithm_header = function
    | None -> []
    | Some algorithm ->
        [
          ( "x-amz-checksum-algorithm",
            Domain.Object.Checksum.Algorithm.to_string algorithm );
        ]

  let checksum_type_header = function
    | None -> []
    | Some checksum_type ->
        [
          ( "x-amz-checksum-type",
            Domain.Object.Checksum.Type.to_string checksum_type );
        ]

  let checksum_mode_header = function
    | None -> []
    | Some mode ->
        [ ("x-amz-checksum-mode", Domain.Object.Checksum.Mode.to_string mode) ]

  let multipart_object_size_header = function
    | None -> []
    | Some size -> [ ("x-amz-mp-object-size", Int64.to_string size) ]

  let kms_headers kms headers =
    headers
    |> add_opt_header "x-amz-server-side-encryption-aws-kms-key-id"
         (Domain.Encryption.Kms.key_id kms)
    |> add_opt_header "x-amz-server-side-encryption-bucket-key-enabled"
         (Option.map string_of_bool
            (Domain.Encryption.Kms.bucket_key_enabled kms))

  let customer_key_headers_with_prefix prefix key =
    [
      (prefix ^ "algorithm", Domain.Encryption.Customer_key.algorithm key);
      (prefix ^ "key", Domain.Encryption.Customer_key.key_base64 key);
      (prefix ^ "key-MD5", Domain.Encryption.Customer_key.key_md5_base64 key);
    ]

  let customer_key_headers = function
    | None -> []
    | Some key ->
        customer_key_headers_with_prefix
          "x-amz-server-side-encryption-customer-" key

  let destination_encryption_headers = function
    | None -> []
    | Some Domain.Encryption.Destination.Sse_s3 ->
        [ ("x-amz-server-side-encryption", "AES256") ]
    | Some (Sse_kms kms) ->
        kms_headers kms [ ("x-amz-server-side-encryption", "aws:kms") ]
    | Some (Dsse_kms kms) ->
        ("x-amz-server-side-encryption", "aws:kms:dsse")
        :: add_opt_header "x-amz-server-side-encryption-aws-kms-key-id"
             (Domain.Encryption.Kms.key_id kms)
             []
    | Some (Sse_c key) -> customer_key_headers (Some key)

  let source_encryption_headers = function
    | None -> []
    | Some (Domain.Encryption.Source.Sse_c key) ->
        customer_key_headers (Some key)

  let copy_source_encryption_headers = function
    | None -> []
    | Some (Domain.Encryption.Source.Sse_c key) ->
        customer_key_headers_with_prefix
          "x-amz-copy-source-server-side-encryption-customer-" key
end
