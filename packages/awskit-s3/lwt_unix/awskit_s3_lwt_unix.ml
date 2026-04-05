include Awskit_s3.Make (Awskit_lwt_unix.Runtime)

(* Re-export pure types for convenience *)
module Error = Awskit_s3.Error
module Storage_class = Awskit_s3.Storage_class
module Tag = Awskit_s3.Tag
module Range = Awskit_s3.Range
module Metadata = Awskit_s3.Metadata
module Policy = Awskit_s3.Policy
module Sim = Awskit_s3.Sim
