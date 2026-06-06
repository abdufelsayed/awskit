(** AWS S3 SDK surface.

    [Awskit_s3] is the public facade for AWS S3 bucket/object storage: object
    operations, bucket operations and configuration, multipart upload, presigned
    URLs, endpoint resolution, and runtime-backed clients. *)

include Awskit_s3_intf.Sigs
(** @inline *)
