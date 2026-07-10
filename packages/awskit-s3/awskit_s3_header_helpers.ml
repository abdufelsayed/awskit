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
    type t

    val to_string : t -> string
  end

  module Encryption : sig
    module Customer_key : sig
      type t

      val reveal_headers : t -> (string * string) list
      val reveal_copy_source_headers : t -> (string * string) list
    end

    module Destination : sig
      type t = private
        | Sse_s3
        | Sse_kms of {
            key_id : string option;
            bucket_key_enabled : bool option;
          }
        | Dsse_kms of { key_id : string option }
        | Sse_c of Customer_key.t
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

        val to_string : t -> string
      end

      module Type : sig
        type t = Composite | Full_object

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
end

module Make (Domain : DOMAIN) (Config : CONFIG) = struct
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
    | Domain.Object.Checksum.Algorithm.Crc32 -> "x-amz-checksum-crc32"
    | Crc32c -> "x-amz-checksum-crc32c"
    | Crc64nvme -> "x-amz-checksum-crc64nvme"
    | Sha1 -> "x-amz-checksum-sha1"
    | Sha256 -> "x-amz-checksum-sha256"
    | Sha512 -> "x-amz-checksum-sha512"
    | Md5 -> "x-amz-checksum-md5"
    | Xxhash64 -> "x-amz-checksum-xxhash64"
    | Xxhash3 -> "x-amz-checksum-xxhash3"
    | Xxhash128 -> "x-amz-checksum-xxhash128"

  let checksum_value_headers = function
    | None -> []
    | Some (checksum : Domain.Object.Checksum.value) ->
        [ (checksum_header_name checksum.algorithm, checksum.value) ]

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

  let kms_headers ~key_id ~bucket_key_enabled headers =
    headers
    |> add_opt_header "x-amz-server-side-encryption-aws-kms-key-id" key_id
    |> add_opt_header "x-amz-server-side-encryption-bucket-key-enabled"
         (Option.map string_of_bool bucket_key_enabled)

  let customer_key_headers = function
    | None -> []
    | Some key -> Domain.Encryption.Customer_key.reveal_headers key

  let destination_encryption_headers = function
    | None -> []
    | Some Domain.Encryption.Destination.Sse_s3 ->
        [ ("x-amz-server-side-encryption", "AES256") ]
    | Some (Sse_kms { key_id; bucket_key_enabled }) ->
        kms_headers ~key_id ~bucket_key_enabled
          [ ("x-amz-server-side-encryption", "aws:kms") ]
    | Some (Dsse_kms { key_id }) ->
        ("x-amz-server-side-encryption", "aws:kms:dsse")
        :: add_opt_header "x-amz-server-side-encryption-aws-kms-key-id" key_id
             []
    | Some (Sse_c key) -> customer_key_headers (Some key)

  let source_encryption_headers = function
    | None -> []
    | Some (Domain.Encryption.Source.Sse_c key) ->
        customer_key_headers (Some key)

  let copy_source_encryption_headers = function
    | None -> []
    | Some (Domain.Encryption.Source.Sse_c key) ->
        Domain.Encryption.Customer_key.reveal_copy_source_headers key
end
