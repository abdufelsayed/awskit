open Common

module type OBJECT_DATA = sig
  module Etag : sig
    type t

    val of_string : string -> (t, Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
  end

  module Version_id : sig
    type t

    val of_string : string -> (t, Error.t) result
    val of_string_exn : string -> t
    val to_string : t -> string
    val pp : Format.formatter -> t -> unit
    val equal : t -> t -> bool
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
      val of_string : string -> t
    end

    module Type : sig
      type t = Composite | Full_object | Unknown of string

      val to_string : t -> string
      val of_string : string -> t
    end

    module Mode : sig
      type t = Enabled

      val to_string : t -> string
    end

    type value = { algorithm : Algorithm.t; value : string }
    type response = { values : value list; checksum_type : Type.t option }

    type summary = {
      algorithms : Algorithm.t list;
      checksum_type : Type.t option;
    }
  end

  module Encryption : sig
    type kms = { key_id : string option; bucket_key_enabled : bool option }
    type request = [ `AES256 | `Aws_kms of kms ]
    type response = [ `AES256 | `Aws_kms of kms | `Unknown of string ]
  end

  module Etag_condition : sig
    type t = Any | Etag of Etag.t

    val any : t
    val etag : Etag.t -> t
  end

  module Preconditions : sig
    module Write : sig
      type t = {
        if_match : Etag_condition.t option;
        if_none_match : Etag_condition.t option;
      }

      val none : t
      val if_absent : t
      val if_etag : Etag.t -> t
    end

    module Read : sig
      type t = {
        if_match : Etag_condition.t option;
        if_none_match : Etag_condition.t option;
        if_modified_since : Ptime.t option;
        if_unmodified_since : Ptime.t option;
      }

      val none : t
    end

    module Delete : sig
      type t = { if_match : Etag_condition.t option }

      val none : t
      val if_etag : Etag.t -> t
    end

    module Copy_source : sig
      type t = {
        if_match : Etag_condition.t option;
        if_none_match : Etag_condition.t option;
        if_modified_since : Ptime.t option;
        if_unmodified_since : Ptime.t option;
      }

      val none : t
    end
  end

  module Tagging : sig
    type result = { tags : Tag.t list; response : Awskit.Response.t }
  end
end
