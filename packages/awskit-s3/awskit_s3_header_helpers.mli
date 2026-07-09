(** Functorized internal helpers for S3 request header construction.

    This interface is shared by the main S3 library and simulator so both
    surfaces validate and render headers with the same domain rules. *)

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

      val algorithm : t -> string
      val key_base64 : t -> string
      val key_md5_base64 : t -> string
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

  val tags_header : Domain.Tag.Set.t -> string option
  val checksum_header_name : Domain.Object.Checksum.Algorithm.t -> string

  val checksum_value_headers :
    Domain.Object.Checksum.value option -> (string * string) list

  val checksum_algorithm_header :
    Domain.Object.Checksum.Algorithm.t option -> (string * string) list

  val checksum_type_header :
    Domain.Object.Checksum.Type.t option -> (string * string) list

  val checksum_mode_header :
    Domain.Object.Checksum.Mode.t option -> (string * string) list

  val multipart_object_size_header : int64 option -> (string * string) list

  val destination_encryption_headers :
    Domain.Encryption.Destination.t option -> (string * string) list

  val source_encryption_headers :
    Domain.Encryption.Source.t option -> (string * string) list

  val copy_source_encryption_headers :
    Domain.Encryption.Source.t option -> (string * string) list

  val customer_key_headers :
    Domain.Encryption.Customer_key.t option -> (string * string) list
end
