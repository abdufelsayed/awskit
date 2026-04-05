(** S3 client parameterized by {!Awskit.Runtime.S}. No IO dependencies.

    Object operations: get, put, delete, copy, list, head. Bucket operations:
    create, delete, list, head. Bucket configuration: policy, versioning,
    encryption, tagging. Multipart uploads: create, upload part, complete,
    abort. Presigned URLs: get object, put object. In-memory simulation for
    testing.

    {[
    module S3 = Awskit_s3.Make (Awskit_eio.Runtime)
    ]}

    Pre-built adapters: [aws-s3-eio], [aws-s3-lwt]. *)

module Error = Error
(** S3-specific error types. *)

module Storage_class = Storage_class
(** Storage classes (Standard, Glacier, etc.). *)

module Tag = Tag
(** Object tags. *)

module Range = Range
(** Byte range for partial reads. *)

module Metadata = Metadata
(** Custom object metadata as [x-amz-meta-*] headers. *)

module Policy = Policy
(** Bucket policy types and evaluation. *)

module Object = Object_
(** Object operations and types. *)

module Bucket = Bucket
(** Bucket operations and types. *)

module Multipart = Multipart
(** Multipart upload operations. *)

module Presigned = Presigned
(** Presigned URL generation. *)

module Sim = Sim
(** In-memory S3 simulation for testing. *)

module Make = Client.Make
(** Functor to create an S3 client with a specific runtime. *)
