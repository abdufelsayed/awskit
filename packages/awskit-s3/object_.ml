open Base

(** S3 Object — types, XML wire formats, and pure response parsers.

    This module defines types for object operations and provides pure functions
    for parsing S3 responses. The actual operations are in the [Make] functor
    result. *)

(** Actions for object operations (used in policy evaluation). *)
module Action = struct
  type t = Get | Put | Delete | Head | List | Copy [@@deriving show, eq]

  let to_string = function
    | Get -> "s3:GetObject"
    | Put -> "s3:PutObject"
    | Delete -> "s3:DeleteObject"
    | Head -> "s3:HeadObject"
    | List -> "s3:ListBucket"
    | Copy -> "s3:GetObject"
end

(** Result of a successful PUT operation. *)
module Put_result = struct
  type t = { etag : string } [@@deriving show, eq]
  (** Contains the ETag of the uploaded object. *)
end

(** ETag condition values for S3 conditional requests. *)
module Etag_condition = struct
  type t = Any | Etag of string [@@deriving show, eq]

  let to_header = function Any -> "*" | Etag etag -> etag
end

(** Operation-specific preconditions for object requests. *)
module Preconditions = struct
  module Write = struct
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
    }
    [@@deriving show, eq]

    let none = { if_match = None; if_none_match = None }
    let if_absent = { none with if_none_match = Some Etag_condition.Any }
    let if_etag etag = { none with if_match = Some (Etag_condition.Etag etag) }
  end

  module Read = struct
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
      if_modified_since : string option;
      if_unmodified_since : string option;
    }
    [@@deriving show, eq]

    let none =
      {
        if_match = None;
        if_none_match = None;
        if_modified_since = None;
        if_unmodified_since = None;
      }
  end

  module Delete = struct
    type t = {
      if_match : Etag_condition.t option;
      if_match_last_modified_time : string option;
      if_match_size : int option;
    }
    [@@deriving show, eq]

    let none =
      {
        if_match = None;
        if_match_last_modified_time = None;
        if_match_size = None;
      }

    let if_etag etag = { none with if_match = Some (Etag_condition.Etag etag) }
  end

  module Copy_source = struct
    type t = {
      if_match : Etag_condition.t option;
      if_none_match : Etag_condition.t option;
      if_modified_since : string option;
      if_unmodified_since : string option;
    }
    [@@deriving show, eq]

    let none =
      {
        if_match = None;
        if_none_match = None;
        if_modified_since = None;
        if_unmodified_since = None;
      }
  end
end

(** Result of a successful GET operation. *)
module Get_result = struct
  type t = {
    body : string;  (** Object content *)
    etag : string;  (** ETag (MD5 hash) *)
    content_type : string option;  (** MIME type *)
    content_length : int;  (** Size in bytes *)
    last_modified : string;  (** Last modification time *)
    metadata : Metadata.t;  (** Custom metadata *)
  }
  [@@deriving show, eq]
end

(** Object metadata (from HEAD operation). *)
module Info = struct
  type t = {
    etag : string;  (** ETag (MD5 hash) *)
    size : int;  (** Size in bytes *)
    last_modified : string;  (** Last modification time *)
    content_type : string option;  (** MIME type *)
    storage_class : Storage_class.t option;
        (** Storage class, if not Standard *)
    metadata : Metadata.t;  (** Custom metadata *)
  }
  [@@deriving show, eq]
end

(** Result of a COPY operation. *)
module Copy_result = struct
  type t = { etag : string; last_modified : string } [@@deriving show, eq]
end

(** Result of a batch DELETE operation. *)
module Delete_result = struct
  (** Successfully deleted objects. *)
  module Deleted = struct
    type t = { key : string } [@@deriving show, eq]
  end

  (** Failed deletions. *)
  module Error = struct
    type t = { key : string; code : string; message : string }
    [@@deriving show, eq]
  end

  type t = { deleted : Deleted.t list; errors : Error.t list }
  [@@deriving show, eq]
end

(** Result of a LIST operation. *)
module List_result = struct
  type t = {
    keys : string list;  (** Object keys in this page *)
    is_truncated : bool;  (** True if more results available *)
    next_continuation_token : string option;  (** Token for next page *)
  }
  [@@deriving show, eq]
end

(* ── XML wire types ─────────────────────────────────────────────── *)

module Xml = struct
  type list_content = {
    key : string; [@key "Key"]
    size : int; [@key "Size"]
    etag : string; [@key "ETag"]
    last_modified : string; [@key "LastModified"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type list_result = {
    name : string; [@key "Name"]
    prefix : string option; [@key "Prefix"]
    key_count : int; [@key "KeyCount"]
    max_keys : int; [@key "MaxKeys"]
    is_truncated : bool; [@key "IsTruncated"]
    contents : list_content list; [@key "Contents"]
    next_continuation_token : string option; [@key "NextContinuationToken"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type copy_result = {
    etag : string; [@key "ETag"]
    last_modified : string; [@key "LastModified"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type delete_object = {
    key : string; [@key "Key"]
    version_id : string option; [@key "VersionId"]
    etag : string option; [@key "ETag"]
    last_modified_time : string option; [@key "LastModifiedTime"]
    size : int option; [@key "Size"]
  }
  [@@deriving
    of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm),
    to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type delete_error = {
    key : string; [@key "Key"]
    code : string; [@key "Code"]
    message : string; [@key "Message"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type delete_request = {
    quiet : bool; [@key "Quiet"]
    objects : delete_object list; [@key "Object"]
  }
  [@@deriving to_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]

  type delete_result = {
    deleted : delete_object list; [@key "Deleted"]
    errors : delete_error list; [@key "Error"]
  }
  [@@deriving of_protocol ~driver:(module Protocol_conv_xmlm.Xmlm)]
end

(* ── Pure response parsers ──────────────────────────────────────── *)

let get_result_of_response (resp : Awskit.Response.t) =
  let ( let* ) result f = Result.bind result ~f in
  let* etag = Awskit.Response.header_exn resp "etag" in
  let* content_length = Awskit.Response.header_int_exn resp "content-length" in
  let* last_modified = Awskit.Response.header_exn resp "last-modified" in
  let metadata = Internal.Metadata_headers.of_response_headers resp.headers in
  Ok
    {
      Get_result.body = resp.body;
      etag;
      content_type = Awskit.Response.header resp "content-type";
      content_length;
      last_modified;
      metadata;
    }

let storage_class_of_response (resp : Awskit.Response.t) =
  match Awskit.Response.header resp "x-amz-storage-class" with
  | None -> Ok None
  | Some storage_class -> (
      match Storage_class.of_string storage_class with
      | Some storage_class -> Ok (Some storage_class)
      | None -> Error (`Invalid_header ("x-amz-storage-class", storage_class)))

let info_of_response (resp : Awskit.Response.t) =
  let ( let* ) result f = Result.bind result ~f in
  let* etag = Awskit.Response.header_exn resp "etag" in
  let* size = Awskit.Response.header_int_exn resp "content-length" in
  let* last_modified = Awskit.Response.header_exn resp "last-modified" in
  let* storage_class = storage_class_of_response resp in
  let metadata = Internal.Metadata_headers.of_response_headers resp.headers in
  Ok
    {
      Info.etag;
      size;
      last_modified;
      content_type = Awskit.Response.header resp "content-type";
      storage_class;
      metadata;
    }

(* ── Tagging submodule types ────────────────────────────────────── *)

module Tagging = struct
  module Action = struct
    type t = Get | Put | Delete [@@deriving show, eq]

    let to_string = function
      | Get -> "s3:GetObjectTagging"
      | Put -> "s3:PutObjectTagging"
      | Delete -> "s3:DeleteObjectTagging"
  end
end
