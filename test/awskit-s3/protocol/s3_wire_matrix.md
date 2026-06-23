# S3 Object And Bucket Wire Matrix

Status: Plan 05 protocol manifest.

This matrix records the supported S3 object and bucket wire behavior that
Awskit intends to lock with focused request, decode, simulator, or contract
tests. It is a manifest for implementation agents, not a new public feature
list. Rows describe existing or Plan-05-supported behavior only.

## Scope

Supported object scope for this plan: PutObject, GetObject, HeadObject,
DeleteObject, DeleteObjects, CopyObject, object tagging, metadata, tags, single
Range reads, conditional headers, version ids, checksums already modeled by the
public surface, and expected-owner guards.

Supported bucket scope for this plan: CreateBucket, DeleteBucket, HeadBucket,
GetBucketLocation, ListBuckets, and existing bucket tagging/configuration APIs
only as wire-shape correction targets for the APIs already present.

Out of scope: object lock, legal hold, lifecycle, replication, access points,
directory buckets, inventory, analytics, broad ACL/policy edge support, every
server-side-encryption variant, every checksum variant, every provider variant,
Requester Pays, MFA delete, S3 on Outposts, Object Lambda, and full
S3-compatible-provider semantics.

## Object Operations

| Operation | API surface/current file | Method | Path/query | Request headers/body | Success/decode | Error/region behavior | Evidence/tests to add |
| --- | --- | --- | --- | --- | --- | --- | --- |
| PutObject | `Object.put`; `packages/awskit-s3/object_request.ml`; `packages/awskit-s3/object.mli` | `PUT` | `/{key}` in bucket host; no query | Body is caller `Body.t` with known `Content-Length`; metadata as `x-amz-meta-*`; optional `Content-Type`, cache/content headers, storage class, `x-amz-tagging`, write preconditions `If-Match`/`If-None-Match`, checksum value header, modeled SSE request header, `x-amz-expected-bucket-owner` | Decode 200/OK response headers: `ETag`, `x-amz-version-id`, checksum headers, raw response metadata; discard response body | Validation fails before transport for invalid bucket/key, invalid metadata/tags/checksum/common headers, or unknown content length | Recording-runtime assertions for method/path/body/content-length, content type, metadata/tags header encoding, write condition headers, expected-owner header, and response decode |
| GetObject | `Object.get`/`Object.find`; `object_request.ml`; `object.mli` | `GET` | `/{key}`; optional `versionId` | No request body; optional one `Range` header only; read preconditions `If-*`; `x-amz-checksum-mode: ENABLED`; `x-amz-expected-bucket-owner` | Decode 200 metadata headers into `Object.Get.result`, including typed `content_range` when S3 returns `Content-Range`; consume response body through caller reader; `find` maps service `NoSuchKey` to `Ok None` only | S3 does not support multiple ranges in one `GET`; missing bucket and consumer-owned errors stay errors; malformed `Content-Range` is a decode error | Add explicit single-range assertion and no multi-range API implication; assert typed `Content-Range` decode; assert `find` keeps missing-bucket/consumer errors; assert `versionId` query |
| HeadObject | `Object.head`/`find_metadata`/`exists`; `object_request.ml`; `object.mli` | `HEAD`, never `GET` | `/{key}`; optional `versionId` | No request body; read preconditions `If-*`; optional checksum mode and expected-owner headers. Current public `Head.options` has no range field | Decode metadata headers like `Get.info`; no object body; discard any runtime body defensively | `find_metadata` maps service `NoSuchKey` and bare HEAD 404 to `Ok None`; missing bucket remains an error. Conditional failures surface as service errors | Add recording assertion that `Object.head`, `find_metadata`, and `exists` send `HEAD`; assert 404 object vs bucket behavior |
| DeleteObject | `Object.delete`; `object_request.ml`; `object.mli` | `DELETE` | `/{key}`; optional `versionId` | No request body; supported delete condition is `If-Match`; expected-owner header. Directory-bucket-only last-modified/size conditions stay out of scope | Decode 204/2xx headers: delete marker, version id, response metadata; discard response body | Owner mismatch is 403 service error; object-lock governance bypass and MFA delete are out of scope | Assert method/query, `If-Match`, expected-owner, and no directory-bucket-only headers |
| DeleteObjects | `Object.delete_objects`; `object_request.ml`; `object_delete_xml.ml`; `object.mli` | `POST` | Bucket root `/?delete` | XML `<Delete>` body with up to 1,000 `<Object>` members and no `<Quiet>` element unless a public quiet option is added; each member has typed key, optional `VersionId`, optional bare conditional `ETag`; `Content-MD5` is required for general-purpose buckets and emitted; `Content-Type: application/xml`; optional expected-owner header | Decode HTTP 200 XML `<DeleteResult>` into per-member `Deleted` and `Error` lists, including `VersionId`, `DeleteMarker`, and `DeleteMarkerVersionId` on deleted members; decode HTTP 200 XML `<Error>` as a service error; malformed returned keys/version ids are decode errors | Empty object list and more than 1,000 objects fail before transport. Requester Pays, MFA delete, governance bypass, directory-bucket checksum alternatives, last-modified/size conditions are out of scope | Assert method is `POST`, query is exactly `delete`, body XML and MD5 header are present, count bound prevents transport, mixed `Deleted`/`Error` members decode, and embedded `<Error>` is a service error |
| CopyObject | `Object.copy`; `object_request.ml`; `object.mli` | `PUT` | Destination `/{destination_key}`; no query | No request body; required `x-amz-copy-source`; source value is URL-encoded bucket/key and optional `?versionId=` exactly once; source preconditions as `x-amz-copy-source-if-*`; optional metadata directive, replacement metadata, storage class, checksum algorithm, modeled SSE request header, destination/source expected-owner headers | Decode 200 XML `CopyObjectResult` plus `x-amz-version-id` and `x-amz-copy-source-version-id`; detect and surface embedded `<Error>` payloads even when HTTP status is 200 | Access point copy source, multipart copy, object-lock headers, tagging directive, ACLs, SSE-C, Local Zone/Outposts rules are out of scope | Add exact once-encoding fixtures for source bucket/key/versionId, including spaces, slashes, percent signs, and `?`; assert embedded copy errors are service errors |
| GetObjectTagging | `Object.Tagging.get`; `object_tagging_request.ml`; `object.mli` | `GET` | `/{key}?tagging` | No body; optional expected-owner header | Decode 200 XML tag set into `Tag.Set.t` and response metadata | Incomplete tag XML is decode error; unsupported tag edge families stay absent | Assert method/query/header and decode malformed tag XML |
| PutObjectTagging | `Object.Tagging.put`; `object_tagging_request.ml`; `object.mli` | `PUT` | `/{key}?tagging` | XML tag set body; `Content-MD5`; `Content-Type: application/xml`; optional expected-owner header | 200/2xx success returns response metadata; discard body | Invalid tags fail before transport | Assert XML body, MD5, content type, query, expected-owner |
| DeleteObjectTagging | `Object.Tagging.delete`; `object_tagging_request.ml`; `object.mli` | `DELETE` | `/{key}?tagging` | No body; optional expected-owner header | 204/2xx success returns response metadata; discard body | Service errors propagate with operation context | Assert method/query/header |

## Bucket Fundamentals

| Operation | API surface/current file | Method | Path/query | Request headers/body | Success/decode | Error/region behavior | Evidence/tests to add |
| --- | --- | --- | --- | --- | --- | --- | --- |
| CreateBucket | `Bucket.create`; `bucket_request.ml`; `bucket.mli` | `PUT` | Bucket root `/`; no query | For `us-east-1`, empty body; for other modeled regions, XML `CreateBucketConfiguration` with `LocationConstraint`; `Content-Type: application/xml` only when body is present | Decode success response metadata into `Bucket.Create.result` | Global endpoint 307 redirect behavior is AWS-defined but endpoint/redirect handling remains transport policy; directory-bucket create, ACLs, object lock, ownership headers, tags are out of scope | Assert method/path/body for `us-east-1` and non-default region; do not add ACL/object-lock rows |
| DeleteBucket | `Bucket.delete`; `bucket_request.ml`; `bucket.mli` | `DELETE` | Bucket root `/`; no query | No body; optional `x-amz-expected-bucket-owner` | 204/2xx success returns response metadata; discard body | Owner mismatch is 403 service error; non-empty bucket and not-found errors propagate | Assert method/path/header |
| HeadBucket | `Bucket.head`/`exists`; `bucket_request.ml`; `bucket.mli` | `HEAD`, never `GET` | Bucket root `/`; no query | No body; optional expected-owner header | On 200, decode `x-amz-bucket-region` when present and preserve raw response metadata | Preserve `x-amz-bucket-region` on success and on service-error/redirect responses when runtime headers are available. Test targets: 301 wrong region, 307 temporary redirect, 400 region hint, 403 forbidden, 404 not found. HEAD errors may have no body and generic status only; `exists` maps not-found to false but must not hide forbidden or region errors | Add recording assertion for `HEAD`; add fixture/runtime tests for 301, 307, 400 region hint, 403, 404 with header preservation |
| GetBucketLocation | `Bucket.get_location`; `bucket_request.ml`; `bucket.mli` | `GET` | Bucket root `/?location` | No body; optional expected-owner header | Decode XML location constraint; `None` represents default-location encoding | 403 expected-owner mismatch and other service errors propagate | Assert method/query/header and default-region decode |
| ListBuckets | `Bucket.list`; `bucket_request.ml`; `bucket.mli` | `GET` | Service root `/`; no query for current surface | No operation body or expected-owner header | Decode `ListAllMyBucketsResult` bucket names through `Bucket_name.of_string`; invalid service bucket names are decode errors | Account-scoped pagination/filtering variants are out of scope until public surface exists | Assert service-root host/path, decode valid names, reject invalid names |

## Existing Bucket Configuration Rows

These rows exist to correct current wire shapes for APIs already present. They
do not expand Awskit's supported S3 feature claims. They are not a mandate to
turn Plan 05 into broad bucket-configuration work; use them when an already
public bucket configuration path is touched, when an existing focused test is
being corrected, or when Plan 10's fixture/evidence infrastructure needs a
source-backed manifest row.

| Operation | API surface/current file | Method | Path/query | Request headers/body | Success/decode | Error/region behavior | Evidence/tests to add |
| --- | --- | --- | --- | --- | --- | --- | --- |
| GetBucketPolicy | `Bucket.Policy.get`; `bucket_request.ml`; `bucket.mli` | `GET` | `/?policy` | No body; optional expected-owner header | Decode policy JSON with `Policy.of_json` | Broad policy semantic validation is out of scope | Assert method/query/header and body size cap/decode context |
| PutBucketPolicy | `Bucket.Policy.put`; `bucket_request.ml`; `bucket.mli` | `PUT` | `/?policy` | Policy JSON body; `Content-MD5`; `Content-Type: application/json`; optional expected-owner header | 2xx response metadata | Policy edge semantics and public-access rules are out of scope | Assert method/query/body/MD5/content type |
| DeleteBucketPolicy | `Bucket.Policy.delete`; `bucket_request.ml`; `bucket.mli` | `DELETE` | `/?policy` | No body; optional expected-owner header | 2xx response metadata | Service errors propagate | Assert method/query/header |
| GetBucketVersioning | `Bucket.Versioning.get`; `bucket_request.ml`; `bucket.mli` | `GET` | `/?versioning` | No body; optional expected-owner header | Decode optional `Enabled`/`Suspended` status | Versioning feature exists only as current config API; directory-bucket versioning is out of scope | Assert method/query/header/decode |
| PutBucketVersioning | `Bucket.Versioning.put`; `bucket_request.ml`; `bucket.mli` | `PUT` | `/?versioning` | XML body; `Content-MD5`; `Content-Type: application/xml`; optional expected-owner header | 2xx response metadata | MFA delete is out of scope | Assert XML/MD5/content type |
| GetBucketTagging | `Bucket.Tagging.get`; `bucket_request.ml`; `bucket.mli` | `GET` | `/?tagging` | No body; optional expected-owner header | Decode XML tag set | Tag validation/decode errors are structured | Assert method/query/header/decode |
| PutBucketTagging | `Bucket.Tagging.put`; `bucket_request.ml`; `bucket.mli` | `PUT` | `/?tagging` | XML tag set body; `Content-MD5`; `Content-Type: application/xml`; optional expected-owner header | 2xx response metadata | Invalid tags fail before transport | Assert XML/MD5/content type |
| DeleteBucketTagging | `Bucket.Tagging.delete`; `bucket_request.ml`; `bucket.mli` | `DELETE` | `/?tagging` | No body; optional expected-owner header | 2xx response metadata | Service errors propagate | Assert method/query/header |
| GetBucketEncryption | `Bucket.Encryption.get`; `bucket_request.ml`; `bucket.mli` | `GET` | `/?encryption` | No body; optional expected-owner header | Decode current encryption XML model | Every SSE/provider variant is out of scope; preserve unknown values where current types do | Assert method/query/header/decode only for modeled config |
| PutBucketEncryption | `Bucket.Encryption.put`; `bucket_request.ml`; `bucket.mli` | `PUT` | `/?encryption` | XML body; `Content-MD5`; `Content-Type: application/xml`; optional expected-owner header | 2xx response metadata | Invalid modeled config fails before transport | Assert XML/MD5/content type |
| DeleteBucketEncryption | `Bucket.Encryption.delete`; `bucket_request.ml`; `bucket.mli` | `DELETE` | `/?encryption` | No body; optional expected-owner header | 2xx response metadata | Service errors propagate | Assert method/query/header |
| GetBucketCors | `Bucket.Cors.get`; `bucket_request.ml`; `bucket.mli` | `GET` | `/?cors` | No body; optional expected-owner header | Decode modeled CORS XML | Broad CORS/provider semantics are out of scope | Assert method/query/header/decode |
| PutBucketCors | `Bucket.Cors.put`; `bucket_request.ml`; `bucket.mli` | `PUT` | `/?cors` | XML body; `Content-MD5`; `Content-Type: application/xml`; optional expected-owner header | 2xx response metadata | Invalid modeled config fails before transport | Assert XML/MD5/content type |
| DeleteBucketCors | `Bucket.Cors.delete`; `bucket_request.ml`; `bucket.mli` | `DELETE` | `/?cors` | No body; optional expected-owner header | 2xx response metadata | Service errors propagate | Assert method/query/header |
| GetPublicAccessBlock | `Bucket.Public_access_block.get`; `bucket_request.ml`; `bucket.mli` | `GET` | `/?publicAccessBlock` | No body; optional expected-owner header | Decode modeled public-access-block XML | Broad ACL/policy edge behavior is out of scope | Assert method/query/header/decode |
| PutPublicAccessBlock | `Bucket.Public_access_block.put`; `bucket_request.ml`; `bucket.mli` | `PUT` | `/?publicAccessBlock` | XML body; `Content-MD5`; `Content-Type: application/xml`; optional expected-owner header | 2xx response metadata | Broad ACL/policy edge behavior is out of scope | Assert XML/MD5/content type |
| DeletePublicAccessBlock | `Bucket.Public_access_block.delete`; `bucket_request.ml`; `bucket.mli` | `DELETE` | `/?publicAccessBlock` | No body; optional expected-owner header | 2xx response metadata | Service errors propagate | Assert method/query/header |
| GetBucketOwnershipControls | `Bucket.Ownership_controls.get`; `bucket_request.ml`; `bucket.mli` | `GET` | `/?ownershipControls` | No body; optional expected-owner header | Decode modeled ownership-controls XML | Broad ACL/ownership edge behavior is out of scope | Assert method/query/header/decode |
| PutBucketOwnershipControls | `Bucket.Ownership_controls.put`; `bucket_request.ml`; `bucket.mli` | `PUT` | `/?ownershipControls` | XML body; `Content-MD5`; `Content-Type: application/xml`; optional expected-owner header | 2xx response metadata | Broad ACL/ownership edge behavior is out of scope | Assert XML/MD5/content type |
| DeleteBucketOwnershipControls | `Bucket.Ownership_controls.delete`; `bucket_request.ml`; `bucket.mli` | `DELETE` | `/?ownershipControls` | No body; optional expected-owner header | 2xx response metadata | Service errors propagate | Assert method/query/header |

## Source Notes

Official AWS S3 API reference pages used for wire facts:

- PutObject: https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html
- GetObject: https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html
- HeadObject: https://docs.aws.amazon.com/AmazonS3/latest/API/API_HeadObject.html
- DeleteObject: https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html
- DeleteObjects: https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObjects.html
- Conditional deletes: https://docs.aws.amazon.com/AmazonS3/latest/userguide/conditional-deletes.html
- CopyObject: https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html
- GetObjectTagging: https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html
- PutObjectTagging: https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObjectTagging.html
- DeleteObjectTagging: https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObjectTagging.html
- CreateBucket: https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html
- DeleteBucket: https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html
- HeadBucket: https://docs.aws.amazon.com/AmazonS3/latest/API/API_HeadBucket.html
- GetBucketLocation: https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLocation.html
- ListBuckets: https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBuckets.html
- Bucket tagging/configuration APIs:
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketVersioning.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketCors.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketOwnershipControls.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketOwnershipControls.html,
  https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketOwnershipControls.html
