include Awskit_s3_internal.Core
module Metadata = Awskit_s3_internal.Metadata
module Storage_class = Awskit_s3_internal.Storage_class
module Tag = Awskit_s3_internal.Tag
module Range = Awskit_s3_internal.Range
module Endpoint_resolver = Awskit_s3_internal.Endpoint_resolver
module Object = Awskit_s3_internal.Object
module Bucket = Awskit_s3_internal.Bucket
module Multipart = Awskit_s3_internal.Multipart
module Transfer = Awskit_s3_internal.Transfer
module Policy = Awskit_s3_internal.Policy
module Presigned = Awskit_s3_internal.Presigned
module Put_object = Object.Put
module Get_object = Object.Get
module Head_object = Object.Head
module Delete_object = Object.Delete
module Delete_objects = Object.Delete_many
module Copy_object = Object.Copy
module List_objects_v2 = Object.List
module List_object_versions = Object.Versions
module Create_bucket = Bucket.Create
module Delete_bucket = Bucket.Delete
module Head_bucket = Bucket.Head
module List_buckets = Bucket.List_buckets
module Get_bucket_location = Bucket.Get_location
module Create_multipart_upload = Multipart.Create
module Upload_part = Multipart.Upload_part
module Complete_multipart_upload = Multipart.Complete
module Abort_multipart_upload = Multipart.Abort
module List_parts = Multipart.List_parts

module type OBJECT_DATA = module type of Object
module type BUCKET_DATA = module type of Bucket
module type MULTIPART_DATA = module type of Multipart
module type POLICY = module type of Policy
module type PRESIGNED_DATA = module type of Presigned

module Make = Awskit_s3_internal.Make.Make
