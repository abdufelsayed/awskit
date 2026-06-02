(** S3 multipart upload data types and operation data. *)

module Upload_id : sig
  type t

  val of_string : string -> (t, Awskit.Error.t) result
  val of_string_exn : string -> t
  val to_string : t -> string
end

module Upload : sig
  type t = private { bucket : string; key : string; upload_id : Upload_id.t }

  val create :
    bucket:string ->
    key:string ->
    upload_id:Upload_id.t ->
    (t, Awskit.Error.t) result

  val create_exn : bucket:string -> key:string -> upload_id:Upload_id.t -> t
end

module Part : sig
  type t = private {
    part_number : int;
    etag : Object.Etag.t;
    checksum : Object.Checksum.value option;
  }

  val create :
    ?checksum:Object.Checksum.value ->
    part_number:int ->
    etag:Object.Etag.t ->
    unit ->
    (t, Awskit.Error.t) result

  val create_exn :
    ?checksum:Object.Checksum.value ->
    part_number:int ->
    etag:Object.Etag.t ->
    unit ->
    t
end

module Create : sig
  type options = {
    content_type : string option;
    metadata : Metadata.t;
    storage_class : Storage_class.t option;
    tags : Tag.t list;
    checksum_algorithm : Object.Checksum.Algorithm.t option;
    checksum_type : Object.Checksum.Type.t option;
    server_side_encryption : Object.Encryption.request option;
    expected_bucket_owner : string option;
  }

  type result = { upload : Upload.t; response : Awskit.Response.t }

  val default_options : options
end

module Upload_part : sig
  type options = {
    checksum : Object.Checksum.value option;
    expected_bucket_owner : string option;
  }

  type result = {
    part : Part.t;
    checksum : Object.Checksum.response;
    response : Awskit.Response.t;
  }

  val default_options : options
end

module Complete : sig
  type options = {
    expected_bucket_owner : string option;
    checksum : Object.Checksum.value option;
    checksum_type : Object.Checksum.Type.t option;
    multipart_object_size : int64 option;
  }

  type result = {
    etag : Object.Etag.t option;
    version_id : Object.Version_id.t option;
    checksum : Object.Checksum.response;
    response : Awskit.Response.t;
  }

  val default_options : options
end

module Abort : sig
  type options = { expected_bucket_owner : string option }
  type result = Awskit.Response.t

  val default_options : options
end

module List_parts : sig
  type options = {
    max_parts : int option;
    part_number_marker : int option;
    expected_bucket_owner : string option;
  }

  type part_info = {
    part_number : int;
    etag : Object.Etag.t option;
    size : int64 option;
    last_modified : Ptime.t option;
    checksum : Object.Checksum.response;
  }

  type page = {
    parts : part_info list;
    is_truncated : bool;
    next_part_number_marker : int option;
    checksum_type : Object.Checksum.Type.t option;
    response : Awskit.Response.t;
  }

  val default_options : options
end
