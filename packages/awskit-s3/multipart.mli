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

  val pp : Format.formatter -> t -> unit
  (** Pretty-print the upload id. *)

  val equal : t -> t -> bool
  (** Compare two upload ids. *)
end

module Part_number : sig
  type t
  (** Opaque S3 multipart part number. S3 accepts part numbers from 1 to 10,000
      inclusive. *)

  val of_int : int -> (t, Awskit.Error.t) result
  (** Validate and wrap a multipart part number. *)

  val of_int_exn : int -> t
  (** Like {!val:of_int}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_int : t -> int
  (** Return the raw part number. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print the part number. *)

  val equal : t -> t -> bool
  (** Compare two part numbers. *)
end

module Part_number_marker : sig
  type t
  (** Opaque S3 [ListParts] part-number marker. Markers accept [0] as the
      position before part 1 and otherwise follow S3's 10,000-part limit. *)

  val of_int : int -> (t, Awskit.Error.t) result
  (** Validate and wrap a part-number marker. *)

  val of_int_exn : int -> t
  (** Like {!val:of_int}, but raises [Awskit.Error.Awskit_error] carrying the
      structured validation error on validation failure. *)

  val to_int : t -> int
  (** Return the raw marker. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print the marker. *)

  val equal : t -> t -> bool
  (** Compare two markers. *)
end

module Upload : sig
  type created
  (** Upload handle returned by [CreateMultipartUpload]. High-level helpers may
      abort these handles automatically when they created the upload and
      post-create work fails before completion. *)

  type caller_owned
  (** Upload handle supplied by the caller, typically from persisted multipart
      state. High-level helpers do not abort caller-owned uploads automatically.
  *)

  type +'ownership t
  (** Identifies one multipart upload for a bucket/key pair. The ownership
      phantom tracks whether Awskit created the upload or the caller resumed it.
  *)

  val created :
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    upload_id:Upload_id.t ->
    created t
  (** Create an Awskit-owned handle after a successful [CreateMultipartUpload]
      response. This is primarily for runtime and simulator implementations. *)

  val resume :
    bucket:Bucket_name.t ->
    key:Object_key.t ->
    upload_id:Upload_id.t ->
    caller_owned t
  (** Rebuild a caller-owned upload handle from persisted multipart state. *)

  val of_strings :
    bucket:string ->
    key:string ->
    upload_id:Upload_id.t ->
    (caller_owned t, Awskit.Error.t) result
  (** Validate raw bucket/key strings and create a caller-owned upload handle.
      Raw string construction is only an edge convenience; standard operations
      use typed bucket and key values. *)

  val of_strings_exn :
    bucket:string -> key:string -> upload_id:Upload_id.t -> caller_owned t
  (** Like {!val:of_strings}, but raises on validation failure. *)

  val bucket : _ t -> Bucket_name.t
  (** Return the upload bucket. *)

  val key : _ t -> Object_key.t
  (** Return the upload key. *)

  val upload_id : _ t -> Upload_id.t
  (** Return the S3 upload id. *)

  val as_caller_owned : _ t -> caller_owned t
  (** Forget Awskit cleanup ownership. Use when persisting or returning a handle
      to callers that own future cleanup. *)
end

module Part : sig
  type t = private {
    part_number : Part_number.t;
    etag : Object.Etag.t;
    checksum : Object.Checksum.value option;
    size : int64 option;
  }
  (** Completed multipart part reference used by [CompleteMultipartUpload]. *)

  val create :
    ?checksum:Object.Checksum.value ->
    ?size:int64 ->
    part_number:Part_number.t ->
    etag:Object.Etag.t ->
    unit ->
    (t, Awskit.Error.t) result
  (** Create a completed part reference. [size] is optional because callers may
      complete parts discovered outside Awskit. Awskit uses known sizes for
      client-side [CompleteMultipartUpload] validation when enough information
      is available; S3 remains the authority for final object-size validation.
  *)

  val create_exn :
    ?checksum:Object.Checksum.value ->
    ?size:int64 ->
    part_number:Part_number.t ->
    etag:Object.Etag.t ->
    unit ->
    t

  val part_number : t -> Part_number.t
  (** Return the completed part number. *)

  val etag : t -> Object.Etag.t
  (** Return the completed part ETag. *)

  val checksum : t -> Object.Checksum.value option
  (** Return the completed part checksum, when known. *)

  val size : t -> int64 option
  (** Return the completed part size, when known. *)
end

module Create : sig
  module Checksum : sig
    type t = private {
      algorithm : Object.Checksum.Algorithm.t;
      checksum_type : Object.Checksum.Type.t option;
    }
    (** Valid checksum policy for [CreateMultipartUpload]. An omitted
        [checksum_type] lets S3 select the mode for [algorithm]. Explicit
        full-object mode accepts CRC64/NVME, CRC32, and CRC32C; explicit
        composite mode accepts every supported algorithm except CRC64/NVME. *)

    val create :
      algorithm:Object.Checksum.Algorithm.t ->
      ?checksum_type:Object.Checksum.Type.t ->
      unit ->
      (t, Awskit.Error.t) result
    (** Build and validate an initiation checksum policy. *)

    val create_exn :
      algorithm:Object.Checksum.Algorithm.t ->
      ?checksum_type:Object.Checksum.Type.t ->
      unit ->
      t
    (** Like {!val:create}, but raises on validation failure. *)
  end

  type options = {
    content_type : Content_type.t option;  (** Final object's [Content-Type]. *)
    metadata : Metadata.t;  (** User metadata for the final object. *)
    storage_class : Storage_class.t option;
        (** Storage class for the final object. *)
    tags : Tag.Set.t;  (** Tags for the final object. *)
    cache_control : Header_value.t option;
        (** Optional [Cache-Control] metadata for the final object. *)
    content_encoding : Header_value.t option;
        (** Optional [Content-Encoding] metadata for the final object. *)
    content_disposition : Header_value.t option;
        (** Optional [Content-Disposition] metadata for the final object. *)
    checksum : Checksum.t option;
        (** Valid checksum policy requested for the multipart upload. *)
    encryption : Encryption.Destination.t option;
        (** Encryption for the final object. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [CreateMultipartUpload] request options. *)

  type result = {
    upload : Upload.created Upload.t;
    response : Awskit.Response.t;
  }
  (** [CreateMultipartUpload] result metadata, including the upload handle used
      by subsequent part requests. *)

  val default_options : options

  val options :
    ?content_type:Content_type.t ->
    ?metadata:Metadata.t ->
    ?storage_class:Storage_class.t ->
    ?tags:Tag.Set.t ->
    ?cache_control:Header_value.t ->
    ?content_encoding:Header_value.t ->
    ?content_disposition:Header_value.t ->
    ?checksum:Checksum.t ->
    ?encryption:Encryption.Destination.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Build [CreateMultipartUpload] options from independently valid fields. *)
end

module Upload_part : sig
  type options = {
    checksum : Object.Checksum.value option;
        (** Explicit checksum for this part body. *)
    customer_key : Encryption.Customer_key.t option;
        (** SSE-C customer key headers for this uploaded part. *)
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

  val options :
    ?checksum:Object.Checksum.value ->
    ?customer_key:Encryption.Customer_key.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Build [UploadPart] options. *)
end

module Complete : sig
  module Parts : sig
    type t
    (** Valid, ordered collection for [CompleteMultipartUpload]. *)

    val of_list :
      ?multipart_object_size:int64 -> Part.t list -> (t, Awskit.Error.t) result
    (** Validate a completion list. The list must be non-empty and strictly
        ordered. When checksums are present, part numbers start at one, remain
        consecutive, and checksum algorithms agree. Known non-final part sizes
        must be at least 5 MiB. [multipart_object_size], when supplied, is
        non-negative and must equal the sum when every part size is known. *)

    val of_list_exn : ?multipart_object_size:int64 -> Part.t list -> t
    (** Like {!val:of_list}, but raises on validation failure. *)

    val to_list : t -> Part.t list
    (** Return the validated parts in completion order. *)

    val multipart_object_size : t -> int64 option
    (** Return the expected assembled object size, when supplied. *)
  end

  type options = private {
    preconditions : Object.Preconditions.Write.t;
        (** Conditional headers checked when committing the final object. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
    checksum : Object.Checksum.value option;
        (** Optional object-level checksum supplied at completion time. *)
    checksum_type : Object.Checksum.Type.t option;
        (** Checksum aggregation mode for S3 to apply. *)
    customer_key : Encryption.Customer_key.t option;
        (** SSE-C customer key headers for completing an SSE-C multipart upload.
        *)
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

  val options :
    ?preconditions:Object.Preconditions.Write.t ->
    ?expected_bucket_owner:Account_id.t ->
    ?checksum:Object.Checksum.value ->
    ?checksum_type:Object.Checksum.Type.t ->
    ?customer_key:Encryption.Customer_key.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [CompleteMultipartUpload] header options. When [checksum] and
      [checksum_type] are both supplied, their multipart capability pair is
      validated. A type without a value remains a valid assertion about the
      initiation policy. *)

  val options_exn :
    ?preconditions:Object.Preconditions.Write.t ->
    ?expected_bucket_owner:Account_id.t ->
    ?checksum:Object.Checksum.value ->
    ?checksum_type:Object.Checksum.Type.t ->
    ?customer_key:Encryption.Customer_key.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end

module List_parts : sig
  type options = private {
    max_parts : int option;
        (** Maximum number of parts S3 should return in one page. Values are
            between 0 and 1,000 inclusive. *)
    part_number_marker : Part_number_marker.t option;
        (** Pagination marker, usually from [next_part_number_marker]. Omit to
            start listing from the beginning. *)
    expected_bucket_owner : Account_id.t option;
        (** [x-amz-expected-bucket-owner]. *)
  }
  (** [ListParts] request options. *)

  type part_info = {
    part_number : Part_number.t;  (** Uploaded part number. *)
    etag : Object.Etag.t option;  (** Part ETag. *)
    size : int64 option;  (** Part size in bytes. *)
    last_modified : Ptime.t option;  (** Part upload timestamp. *)
    checksum : Object.Checksum.response;  (** Part checksum metadata. *)
  }
  (** Uploaded part summary returned by [ListParts]. *)

  type page = {
    parts : part_info list;  (** Uploaded parts in this page. *)
    is_truncated : bool;  (** Whether more part pages are available. *)
    next_part_number_marker : Part_number_marker.t option;
        (** Marker to use for the next page. *)
    checksum_type : Object.Checksum.Type.observed option;
        (** Checksum aggregation mode reported by S3. *)
    response : Awskit.Response.t;  (** Raw response metadata. *)
  }
  (** One [ListParts] page. *)

  val default_options : options

  val options :
    ?max_parts:int ->
    ?part_number_marker:Part_number_marker.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    (options, Awskit.Error.t) Stdlib.result
  (** Build [ListParts] options. [max_parts], when present, must be between 0
      and 1,000 inclusive. *)

  val options_exn :
    ?max_parts:int ->
    ?part_number_marker:Part_number_marker.t ->
    ?expected_bucket_owner:Account_id.t ->
    unit ->
    options
  (** Like {!val:options}, but raises on validation failure. *)
end
