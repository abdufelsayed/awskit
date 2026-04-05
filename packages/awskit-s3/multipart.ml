open Base

(** S3 Multipart Upload — types and XML wire formats.

    Multipart uploads let you upload large objects in parts. This module defines
    the types used across multipart operations. The actual operations are in the
    [Make] functor result.

    Typical multipart upload flow:
    + [Multipart.create] — initiate the upload
    + [Multipart.upload_part] — upload each part
    + [Multipart.complete] — finalize the upload

    If something goes wrong, use [Multipart.abort] to cancel. *)

module Action = struct
  type t = Create | Upload_part | Complete | Abort | List_parts
  [@@deriving show, eq]

  let to_string = function
    | Create -> "s3:PutObject"
    | Upload_part -> "s3:PutObject"
    | Complete -> "s3:PutObject"
    | Abort -> "s3:AbortMultipartUpload"
    | List_parts -> "s3:ListMultipartUploadParts"
end

(** An in-progress multipart upload. *)
module Upload = struct
  type t = { upload_id : string; key : string } [@@deriving show, eq]
  (** [upload_id] is returned by S3 when the upload is initiated. [key] is the
      object key. *)
end

(** A completed upload part. *)
module Part = struct
  type t = { part_number : int; etag : string } [@@deriving show, eq]
  (** [part_number] is the 1-based part index. [etag] is returned by S3 after
      each part upload. *)
end

(** Detailed info about a uploaded part (from [list_parts]). *)
module Part_info = struct
  type t = {
    part_number : int;
    etag : string;
    size : int;
    last_modified : string;
  }
  [@@deriving show, eq]
end

(* ── XML wire types ─────────────────────────────────────────────── *)

module Xml = struct
  type initiate_result = {
    bucket : string; [@key "Bucket"]
    key : string; [@key "Key"]
    upload_id : string; [@key "UploadId"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type part = {
    part_number : int; [@key "PartNumber"]
    etag : string; [@key "ETag"]
  }
  [@@deriving
    to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
    of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type complete_request = { parts : part list [@key "Part"] }
  [@@deriving to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type complete_result = {
    location : string; [@key "Location"]
    bucket : string; [@key "Bucket"]
    key : string; [@key "Key"]
    etag : string; [@key "ETag"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type list_part_info = {
    part_number : int; [@key "PartNumber"]
    etag : string; [@key "ETag"]
    size : int; [@key "Size"]
    last_modified : string; [@key "LastModified"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type list_parts_result = {
    parts : list_part_info list; [@key "Part"]
    is_truncated : bool; [@key "IsTruncated"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
end
