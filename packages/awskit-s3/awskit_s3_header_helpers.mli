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
      | Unknown of string
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

      type value = { algorithm : Algorithm.t; value : string }
    end

    module Encryption : sig
      type kms = { key_id : string option; bucket_key_enabled : bool option }
      type request = [ `AES256 | `Aws_kms of kms ]
    end
  end
end

module type CONFIG = sig
  val ptime_to_header : Ptime.t -> string

  val validate_header_value :
    field:string -> string -> (unit, Awskit.Error.t) result
end

module Make (Domain : DOMAIN) (Config : CONFIG) : sig
  val add_opt_header :
    string -> string option -> (string * string) list -> (string * string) list

  val add_opt_account_id_header :
    string ->
    Domain.Account_id.t option ->
    (string * string) list ->
    (string * string) list

  val add_opt_content_type_header :
    string ->
    Domain.Content_type.t option ->
    (string * string) list ->
    (string * string) list

  val write_precondition_headers :
    Domain.Object.Preconditions.Write.t -> (string * string) list

  val read_precondition_headers :
    Domain.Object.Preconditions.Read.t -> (string * string) list

  val delete_precondition_headers :
    Domain.Object.Preconditions.Delete.t -> (string * string) list

  val copy_source_precondition_headers :
    Domain.Object.Preconditions.Copy_source.t -> (string * string) list

  val validate_common_headers :
    ?content_type:string ->
    ?cache_control:string ->
    ?content_encoding:string ->
    ?content_disposition:string ->
    unit ->
    (unit, Awskit.Error.t) result

  val tags_header : Domain.Tag.Set.t -> string option
  val checksum_header_name : Domain.Object.Checksum.Algorithm.t -> string option

  val validate_checksum_algorithm :
    Domain.Object.Checksum.Algorithm.t -> (unit, Awskit.Error.t) result

  val validate_checksum_type :
    Domain.Object.Checksum.Type.t -> (unit, Awskit.Error.t) result

  val validate_checksum_value :
    Domain.Object.Checksum.value -> (unit, Awskit.Error.t) result

  val validate_storage_class :
    Domain.Storage_class.t -> (unit, Awskit.Error.t) result

  val checksum_value_headers :
    Domain.Object.Checksum.value option -> (string * string) list

  val checksum_algorithm_header :
    Domain.Object.Checksum.Algorithm.t option -> (string * string) list

  val checksum_type_header :
    Domain.Object.Checksum.Type.t option -> (string * string) list

  val checksum_mode_header :
    Domain.Object.Checksum.Mode.t option -> (string * string) list

  val multipart_object_size_header : int64 option -> (string * string) list

  val encryption_request_headers :
    Domain.Object.Encryption.request option -> (string * string) list
end
