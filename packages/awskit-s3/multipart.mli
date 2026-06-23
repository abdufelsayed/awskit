(** S3 multipart upload data types, options, and results. *)

module Upload_id : sig
  type t
  (** Opaque multipart upload id returned by S3. *)

  val of_string : string -> (t, Awskit.Error.t) result
  (** Validate and wrap a multipart upload id. *)

  val of_string_exn : string -> t
  (** Like {!val:of_string}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_string : t -> string
  (** Return the upload id string. *)
end

module Upload : sig
  type t = private { bucket : string; key : string; upload_id : Upload_id.t }
  (** Identifies one multipart upload for a bucket/key pair. *)

  val create :
    bucket:string ->
    key:string ->
    upload_id:Upload_id.t ->
    (t, Awskit.Error.t) result
  (** Create an upload handle after validating bucket, key, and upload id. *)

  val create_exn : bucket:string -> key:string -> upload_id:Upload_id.t -> t
  (** Like {!val:create}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)
end

module Part : sig
  type t = private {
    part_number : int;
    etag : Object.Etag.t;
    checksum : Object.Checksum.value option;
  }
  (** Completed multipart part reference used by [CompleteMultipartUpload]. *)

  val create :
    ?checksum:Object.Checksum.value ->
    part_number:int ->
    etag:Object.Etag.t ->
    unit ->
    (t, Awskit.Error.t) result
  (** Create a completed part reference. S3 part numbers must be in the valid
      multipart part-number range. *)

  val create_exn :
    ?checksum:Object.Checksum.value ->
    part_number:int ->
    etag:Object.Etag.t ->
    unit ->
    t
end

module Create : sig
  type options = {
    content_type : Content_type.t option;  (** Final object's [Content-Type]. *)
    metadata : Metadata.t;  (** User metadata for the final object. *)
    storage_class : Storage_class.t option;
        (** Storage class for the final object. *)
    tags : Tag.Set.t;  (** Tags for the final object. *)
    checksum_algorithm : Object.Checksum.Algorithm.t option;
        (** Checksum algorithm requested for the multipart upload. *)
    checksum_type : Object.Checksum.Type.t option;
        (** Whether S3 should treat checksums as full-object or composite. *)
    server_side_encryption : Object.Encryption.request option;
        (** Server-side encryption for the final object. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [CreateMultipartUpload] request options. *)

  type result = { upload : Upload.t; response : Awskit.Response.t }
  (** [CreateMultipartUpload] result metadata, including the upload handle used
      by subsequent part requests. *)

  val default_options : options
end

module Upload_part : sig
  type options = {
    checksum : Object.Checksum.value option;
        (** Explicit checksum for this part body. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [UploadPart] request options. *)

  type result = {
    part : Part.t;
        (** Completed part reference for [CompleteMultipartUpload]. *)
    checksum : Object.Checksum.response;
        (** Checksum response headers for the uploaded part. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [UploadPart] result metadata. *)

  val default_options : options
end

module Complete : sig
  type options = {
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
    checksum : Object.Checksum.value option;
        (** Optional full-object checksum supplied at completion time. *)
    checksum_type : Object.Checksum.Type.t option;
        (** Checksum aggregation mode for S3 to apply. *)
    multipart_object_size : int64 option;
        (** Expected final object size sent as [x-amz-mp-object-size]. *)
  }
  (** [CompleteMultipartUpload] request options. *)

  type result = {
    etag : Object.Etag.t option;  (** Final object ETag. *)
    version_id : Object.Version_id.t option;
        (** Final object version id for versioned buckets. *)
    checksum : Object.Checksum.response;
        (** Final checksum response headers. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** [CompleteMultipartUpload] result metadata. *)

  val default_options : options
end

module Abort : sig
  type options = { expected_bucket_owner : Account_id.t option }
  (** [AbortMultipartUpload] request options. *)

  type result = Awskit.Response.t
  (** [AbortMultipartUpload] returns only raw response metadata. *)

  val default_options : options
end

module List_parts : sig
  type options = {
    max_parts : int option;
        (** Maximum number of parts S3 should return in one page. *)
    part_number_marker : int option;
        (** Pagination marker, usually from [next_part_number_marker]. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [ListParts] request options. *)

  type part_info = {
    part_number : int;  (** Uploaded part number. *)
    etag : Object.Etag.t option;  (** Part ETag. *)
    size : int64 option;  (** Part size in bytes. *)
    last_modified : Ptime.t option;  (** Part upload timestamp. *)
    checksum : Object.Checksum.response;  (** Part checksum metadata. *)
  }
  (** Uploaded part summary returned by [ListParts]. *)

  type page = {
    parts : part_info list;  (** Uploaded parts in this page. *)
    is_truncated : bool;  (** Whether more part pages are available. *)
    next_part_number_marker : int option;
        (** Marker to use for the next page. *)
    checksum_type : Object.Checksum.Type.t option;
        (** Checksum aggregation mode reported by S3. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** One [ListParts] page. *)

  val default_options : options
end
