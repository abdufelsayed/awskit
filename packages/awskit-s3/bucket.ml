open Base

(** S3 Bucket — types, XML wire formats, and config submodules.

    Provides types for bucket operations and submodules for bucket-level
    configuration: policy, versioning, encryption, and tagging. The actual
    operations are in the [Make] functor result. *)

let all_results results =
  List.fold_right results ~init:(Ok []) ~f:(fun result acc ->
      match (result, acc) with
      | Ok value, Ok values -> Ok (value :: values)
      | (Error _ as error), _ -> error
      | _, (Error _ as error) -> error)

module Action = struct
  type t = Create | Delete | Head | List | Get_location [@@deriving show, eq]

  let to_string = function
    | Create -> "s3:CreateBucket"
    | Delete -> "s3:DeleteBucket"
    | Head -> "s3:HeadBucket"
    | List -> "s3:ListAllMyBuckets"
    | Get_location -> "s3:GetBucketLocation"
end

(** Bucket metadata (from LIST operation). *)
module Info = struct
  type t = { name : string; creation_date : string } [@@deriving show, eq]
end

(* ── XML wire types ─────────────────────────────────────────────── *)

module Xml = struct
  type bucket_info = {
    name : string; [@key "Name"]
    creation_date : string; [@key "CreationDate"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type buckets = { bucket : bucket_info list [@key "Bucket"] }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type list_result = { buckets : buckets [@key "Buckets"] }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type location = { location : string option [@key "LocationConstraint"] }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type create_bucket_config = {
    location_constraint : string; [@key "LocationConstraint"]
  }
  [@@deriving to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type versioning_config = { status : string option [@key "Status"] }
  [@@deriving
    of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
    to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type encryption_default = {
    sse_algorithm : string; [@key "SSEAlgorithm"]
    kms_master_key_id : string option; [@key "KMSMasterKeyID"]
  }
  [@@deriving
    of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
    to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type encryption_rule = {
    apply : encryption_default; [@key "ApplyServerSideEncryptionByDefault"]
  }
  [@@deriving
    of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
    to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type encryption_config = { rules : encryption_rule list [@key "Rule"] }
  [@@deriving
    of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
    to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
end

(* ── Submodule types ────────────────────────────────────────────── *)

(** Bucket policy operations (get, put, delete). *)
module Policy_ = struct
  module Action = struct
    type t = Get | Put | Delete [@@deriving show, eq]

    let to_string = function
      | Get -> "s3:GetBucketPolicy"
      | Put -> "s3:PutBucketPolicy"
      | Delete -> "s3:DeleteBucketPolicy"
  end
end

module Policy = Policy_

(** Bucket versioning configuration. *)
module Versioning = struct
  module Action = struct
    type t = Get | Put [@@deriving show, eq]

    let to_string = function
      | Get -> "s3:GetBucketVersioning"
      | Put -> "s3:PutBucketVersioning"
  end

  (** Versioning status. *)
  module Status = struct
    type t = Enabled | Suspended [@@deriving show, eq]

    let to_string = function Enabled -> "Enabled" | Suspended -> "Suspended"

    let of_string = function
      | "Enabled" -> Some Enabled
      | "Suspended" -> Some Suspended
      | _ -> None
  end
end

(** Bucket tagging operations. *)
module Tagging = struct
  module Action = struct
    type t = Get | Put | Delete [@@deriving show, eq]

    let to_string = function
      | Get -> "s3:GetBucketTagging"
      | Put -> "s3:PutBucketTagging"
      | Delete -> "s3:DeleteBucketTagging"
  end
end

(** Server-side encryption configuration. *)
module Encryption = struct
  module Action = struct
    type t = Get | Put | Delete [@@deriving show, eq]

    let to_string = function
      | Get -> "s3:GetEncryptionConfiguration"
      | Put -> "s3:PutEncryptionConfiguration"
      | Delete -> "s3:DeleteEncryptionConfiguration"
  end

  (** Encryption algorithm. *)
  module Algorithm = struct
    type t = Aes256 | Aws_kms [@@deriving show, eq]

    let to_string = function Aes256 -> "AES256" | Aws_kms -> "aws:kms"

    let of_string = function
      | "AES256" -> Some Aes256
      | "aws:kms" -> Some Aws_kms
      | _ -> None
  end

  (** A single encryption rule. *)
  module Rule = struct
    type t = { sse_algorithm : Algorithm.t; kms_master_key_id : string option }
    [@@deriving show, eq]
  end

  (** Encryption configuration (one or more rules). *)
  module Config = struct
    type t = { rules : Rule.t list } [@@deriving show, eq]
  end
end
