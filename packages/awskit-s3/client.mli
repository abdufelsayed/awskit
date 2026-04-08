(** S3 client functor. Adapter libraries: [Awskit_s3_eio], [Awskit_s3_lwt],
    [Awskit_s3_lwt_unix].

    {[
    module S3 = Awskit_s3.Make (My_runtime)
    ]} *)

module Make (R : Awskit.Runtime.S) : sig
  (** {1 Object operations} *)

  module Object : sig
    include module type of Object_

    module Tagging : sig
      include module type of Object_.Tagging

      val get :
        R.connection ->
        bucket:string ->
        key:string ->
        (Tag.t list, Error.t) result R.t
      (** Error if the object doesn't exist. *)

      val put :
        R.connection ->
        bucket:string ->
        key:string ->
        Tag.t list ->
        (unit, Error.t) result R.t
      (** Replaces all existing tags. *)

      val delete :
        R.connection ->
        bucket:string ->
        key:string ->
        (unit, Error.t) result R.t
      (** Remove all tags from an object. *)
    end

    val put :
      R.connection ->
      bucket:string ->
      key:string ->
      ?if_none_match:bool ->
      ?if_match:string ->
      ?content_type:string ->
      ?metadata:Metadata.t ->
      ?storage_class:Storage_class.t ->
      ?tags:Tag.t list ->
      ?cache_control:string ->
      ?content_encoding:string ->
      ?content_disposition:string ->
      string ->
      (Put_result.t, Error.t) result R.t
    (** Upload an object. Returns the ETag.

        [if_none_match:true] makes this a conditional write — fails with
        [`Precondition_failed] if the key already exists (put-if-not-exists).
        [if_match] only writes if the current ETag matches. [metadata] is stored
        as [x-amz-meta-*] headers. *)

    val get :
      R.connection ->
      bucket:string ->
      key:string ->
      ?range:Range.t ->
      ?if_match:string ->
      ?if_none_match:string ->
      ?if_modified_since:string ->
      ?if_unmodified_since:string ->
      unit ->
      (Get_result.t, Error.t) result R.t
    (** Download an object. [range] for partial reads, [if_*] for conditional
        gets per S3 semantics. *)

    val head :
      R.connection ->
      bucket:string ->
      key:string ->
      ?if_match:string ->
      ?if_none_match:string ->
      ?if_modified_since:string ->
      ?if_unmodified_since:string ->
      unit ->
      (Info.t option, Error.t) result R.t
    (** Object metadata without the body. [Ok (Some info)] if exists, [Ok None]
        if not — no error for missing objects. *)

    val delete :
      R.connection -> bucket:string -> key:string -> (unit, Error.t) result R.t
    (** Succeeds even if the key doesn't exist (S3 semantics). *)

    val delete_batch :
      R.connection ->
      bucket:string ->
      keys:string list ->
      (Delete_result.t, Error.t) result R.t
    (** Up to 1000 keys per request. Returns both deleted keys and per-key
        errors. *)

    val copy :
      R.connection ->
      src_bucket:string ->
      src_key:string ->
      dst_bucket:string ->
      dst_key:string ->
      ?if_match:string ->
      ?if_none_match:string ->
      ?metadata_directive:[ `Copy | `Replace ] ->
      ?metadata:Metadata.t ->
      unit ->
      (Copy_result.t, Error.t) result R.t
    (** Server-side copy. [metadata_directive]: [`Copy] keeps source metadata
        (default), [`Replace] uses the new [metadata] parameter. *)

    val list :
      R.connection ->
      bucket:string ->
      prefix:string ->
      ?delimiter:string ->
      ?max_keys:int ->
      ?start_after:string ->
      ?continuation_token:string ->
      unit ->
      (List_result.t, Error.t) result R.t
    (** S3 list v2. If [is_truncated] is [true], pass [next_continuation_token]
        to the next call. [delimiter] groups keys (e.g., ["/"] for
        directory-like listing). [max_keys] defaults to 1000. *)
  end

  (** {1 Bucket operations} *)

  module Bucket : sig
    type bucket_policy = Policy.t
    (** Alias for the bucket policy document type ([Policy.t]).

        Needed because the [Policy] submodule below shadows the outer [Policy]
        module name. *)

    include module type of Bucket

    val create :
      R.connection ->
      bucket:string ->
      ?region:string ->
      unit ->
      (unit, Error.t) result R.t
    (** Create a new S3 bucket.

        @param region
          Region for the bucket. Defaults to the connection's region. For
          [us-east-1], no region constraint is sent. *)

    val delete : R.connection -> bucket:string -> (unit, Error.t) result R.t
    (** Delete an empty bucket.

        Fails with [`Bucket_not_empty] if the bucket contains objects. *)

    val head : R.connection -> bucket:string -> (bool, Error.t) result R.t
    (** Check whether a bucket exists.

        Returns [Ok true] or [Ok false] — no error for missing buckets. *)

    val list : R.connection -> (Info.t list, Error.t) result R.t
    (** List all buckets in your account. *)

    val get_location :
      R.connection -> bucket:string -> (string, Error.t) result R.t
    (** Get the region a bucket is in. *)

    (** Bucket policy operations (JSON-based access policies). *)
    module Policy : sig
      include module type of Bucket.Policy_

      val get :
        R.connection -> bucket:string -> (bucket_policy, Error.t) result R.t
      (** Get the bucket policy document. *)

      val put :
        R.connection ->
        bucket:string ->
        bucket_policy ->
        (unit, Error.t) result R.t
      (** Set the bucket policy document. *)

      val delete : R.connection -> bucket:string -> (unit, Error.t) result R.t
      (** Delete the bucket policy. *)
    end

    (** Bucket versioning operations. *)
    module Versioning : sig
      include module type of Bucket.Versioning

      val get :
        R.connection -> bucket:string -> (Status.t option, Error.t) result R.t
      (** Get versioning status. Returns [None] if never enabled. *)

      val put :
        R.connection -> bucket:string -> Status.t -> (unit, Error.t) result R.t
      (** Enable or suspend versioning. *)
    end

    (** Bucket tagging operations. *)
    module Tagging : sig
      include module type of Bucket.Tagging

      val get :
        R.connection -> bucket:string -> (Tag.t list, Error.t) result R.t
      (** Get all tags on a bucket. *)

      val put :
        R.connection ->
        bucket:string ->
        Tag.t list ->
        (unit, Error.t) result R.t
      (** Set tags on a bucket (replaces all existing tags). *)

      val delete : R.connection -> bucket:string -> (unit, Error.t) result R.t
      (** Remove all tags from a bucket. *)
    end

    (** Bucket encryption operations.

        Manage default server-side encryption for a bucket. *)
    module Encryption : sig
      (** Actions for encryption operations (used in policy evaluation). *)
      module Action : sig
        type t = Bucket.Encryption.Action.t = Get | Put | Delete
        [@@deriving show, eq]

        val to_string : t -> string
      end

      (** Encryption algorithm. *)
      module Algorithm : sig
        type t = Bucket.Encryption.Algorithm.t = Aes256 | Aws_kms
        [@@deriving show, eq]

        val to_string : t -> string
        val of_string : string -> t option
      end

      (** A single encryption rule. *)
      module Rule : sig
        type t = Bucket.Encryption.Rule.t = {
          sse_algorithm : Algorithm.t;
          kms_master_key_id : string option;
        }
        [@@deriving show, eq]
      end

      (** Encryption configuration (one or more rules). *)
      module Config : sig
        type t = Bucket.Encryption.Config.t = { rules : Rule.t list }
        [@@deriving show, eq]
      end

      val get : R.connection -> bucket:string -> (Config.t, Error.t) result R.t
      (** Get the default encryption configuration. *)

      val put :
        R.connection -> bucket:string -> Config.t -> (unit, Error.t) result R.t
      (** Set the default encryption configuration. *)

      val delete : R.connection -> bucket:string -> (unit, Error.t) result R.t
      (** Remove the default encryption configuration. *)
    end
  end

  (** {1 Multipart upload operations}

      For large objects (over 5 MB), multipart uploads let you upload in parts
      and assemble them on S3. *)

  module Multipart : sig
    include module type of Multipart

    val create :
      R.connection ->
      bucket:string ->
      key:string ->
      ?content_type:string ->
      ?metadata:Metadata.t ->
      ?storage_class:Storage_class.t ->
      unit ->
      (Upload.t, Error.t) result R.t
    (** Initiate a multipart upload.

        Returns an {!Upload.t} containing the upload ID needed for subsequent
        operations.

        @param content_type MIME type for the final object
        @param metadata Custom metadata for the final object
        @param storage_class Storage tier for the final object *)

    val upload_part :
      R.connection ->
      bucket:string ->
      key:string ->
      upload_id:string ->
      part_number:int ->
      string ->
      (Part.t, Error.t) result R.t
    (** Upload a single part.

        Each part must be ≥ 5 MB except the last one. Returns a {!Part.t} with
        the ETag — collect these for {!val:complete}.

        @param part_number 1-based part index *)

    val complete :
      R.connection ->
      bucket:string ->
      key:string ->
      upload_id:string ->
      Part.t list ->
      (Object_.Put_result.t, Error.t) result R.t
    (** Finalize the multipart upload.

        Pass the list of {!Part.t} values returned by {!val:upload_part}. S3
        assembles the parts into the final object. *)

    val abort :
      R.connection ->
      bucket:string ->
      key:string ->
      upload_id:string ->
      (unit, Error.t) result R.t
    (** Abort and clean up an incomplete multipart upload.

        Always abort uploads you don't complete — incomplete parts are still
        billed. *)

    val list_parts :
      R.connection ->
      bucket:string ->
      key:string ->
      upload_id:string ->
      (Part_info.t list, Error.t) result R.t
    (** List the parts uploaded so far for an in-progress upload. *)
  end

  (** {1 Presigned URLs}

      Generate temporary URLs that grant access to S3 objects without requiring
      AWS credentials on the client side. *)

  module Presigned : sig
    val get_object :
      R.connection ->
      bucket:string ->
      key:string ->
      ?expires_in:int ->
      unit ->
      (string, [> `Invalid_request of string ]) result
    (** Generate a presigned GET URL for downloading an object.

        @param expires_in URL lifetime in seconds (default: 3600, max: 604800)
    *)

    val put_object :
      R.connection ->
      bucket:string ->
      key:string ->
      ?expires_in:int ->
      ?content_type:string ->
      unit ->
      (string, [> `Invalid_request of string ]) result
    (** Generate a presigned PUT URL for uploading an object.

        @param expires_in URL lifetime in seconds (default: 3600, max: 604800)
        @param content_type Required [Content-Type] for the upload *)
  end
end
