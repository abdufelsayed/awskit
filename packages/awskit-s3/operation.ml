open Base

type t =
  | Abort_multipart_upload
  | Complete_multipart_upload
  | Copy_object
  | Create_bucket
  | Create_multipart_upload
  | Delete_bucket
  | Delete_bucket_cors
  | Delete_bucket_encryption
  | Delete_bucket_ownership_controls
  | Delete_bucket_policy
  | Delete_bucket_tagging
  | Delete_object
  | Delete_object_tagging
  | Delete_objects
  | Delete_public_access_block
  | Get_bucket_cors
  | Get_bucket_encryption
  | Get_bucket_location
  | Get_bucket_ownership_controls
  | Get_bucket_policy
  | Get_bucket_tagging
  | Get_bucket_versioning
  | Get_object
  | Get_object_tagging
  | Get_public_access_block
  | Head_bucket
  | Head_object
  | List_buckets
  | List_object_versions
  | List_objects_v2
  | List_parts
  | Put_bucket_cors
  | Put_bucket_encryption
  | Put_bucket_ownership_controls
  | Put_bucket_policy
  | Put_bucket_tagging
  | Put_bucket_versioning
  | Put_object
  | Put_object_tagging
  | Put_public_access_block
  | Upload_part

let to_string = function
  | Abort_multipart_upload -> "AbortMultipartUpload"
  | Complete_multipart_upload -> "CompleteMultipartUpload"
  | Copy_object -> "CopyObject"
  | Create_bucket -> "CreateBucket"
  | Create_multipart_upload -> "CreateMultipartUpload"
  | Delete_bucket -> "DeleteBucket"
  | Delete_bucket_cors -> "DeleteBucketCors"
  | Delete_bucket_encryption -> "DeleteBucketEncryption"
  | Delete_bucket_ownership_controls -> "DeleteBucketOwnershipControls"
  | Delete_bucket_policy -> "DeleteBucketPolicy"
  | Delete_bucket_tagging -> "DeleteBucketTagging"
  | Delete_object -> "DeleteObject"
  | Delete_object_tagging -> "DeleteObjectTagging"
  | Delete_objects -> "DeleteObjects"
  | Delete_public_access_block -> "DeletePublicAccessBlock"
  | Get_bucket_cors -> "GetBucketCors"
  | Get_bucket_encryption -> "GetBucketEncryption"
  | Get_bucket_location -> "GetBucketLocation"
  | Get_bucket_ownership_controls -> "GetBucketOwnershipControls"
  | Get_bucket_policy -> "GetBucketPolicy"
  | Get_bucket_tagging -> "GetBucketTagging"
  | Get_bucket_versioning -> "GetBucketVersioning"
  | Get_object -> "GetObject"
  | Get_object_tagging -> "GetObjectTagging"
  | Get_public_access_block -> "GetPublicAccessBlock"
  | Head_bucket -> "HeadBucket"
  | Head_object -> "HeadObject"
  | List_buckets -> "ListBuckets"
  | List_object_versions -> "ListObjectVersions"
  | List_objects_v2 -> "ListObjectsV2"
  | List_parts -> "ListParts"
  | Put_bucket_cors -> "PutBucketCors"
  | Put_bucket_encryption -> "PutBucketEncryption"
  | Put_bucket_ownership_controls -> "PutBucketOwnershipControls"
  | Put_bucket_policy -> "PutBucketPolicy"
  | Put_bucket_tagging -> "PutBucketTagging"
  | Put_bucket_versioning -> "PutBucketVersioning"
  | Put_object -> "PutObject"
  | Put_object_tagging -> "PutObjectTagging"
  | Put_public_access_block -> "PutPublicAccessBlock"
  | Upload_part -> "UploadPart"

let equal = Poly.equal

let all =
  [
    Abort_multipart_upload;
    Complete_multipart_upload;
    Copy_object;
    Create_bucket;
    Create_multipart_upload;
    Delete_bucket;
    Delete_bucket_cors;
    Delete_bucket_encryption;
    Delete_bucket_ownership_controls;
    Delete_bucket_policy;
    Delete_bucket_tagging;
    Delete_object;
    Delete_object_tagging;
    Delete_objects;
    Delete_public_access_block;
    Get_bucket_cors;
    Get_bucket_encryption;
    Get_bucket_location;
    Get_bucket_ownership_controls;
    Get_bucket_policy;
    Get_bucket_tagging;
    Get_bucket_versioning;
    Get_object;
    Get_object_tagging;
    Get_public_access_block;
    Head_bucket;
    Head_object;
    List_buckets;
    List_object_versions;
    List_objects_v2;
    List_parts;
    Put_bucket_cors;
    Put_bucket_encryption;
    Put_bucket_ownership_controls;
    Put_bucket_policy;
    Put_bucket_tagging;
    Put_bucket_versioning;
    Put_object;
    Put_object_tagging;
    Put_public_access_block;
    Upload_part;
  ]
