(** AWS S3 client API.

    [Awskit_s3] is the main entrypoint for AWS S3 bucket and object storage:
    object operations, bucket operations and configuration, multipart upload,
    presigned request artifacts, endpoint resolution, and runtime-backed
    clients. *)

include Awskit_s3_intf.Sigs
(** @inline *)
