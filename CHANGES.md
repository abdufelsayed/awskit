# 0.1.0

Initial public release of Awskit.

Includes core AWS signing, credentials, endpoints, request/response types,
retry support, runtime abstractions, Unix credential helpers, Lwt and Eio
runtime adapters, and focused S3 support for general-purpose buckets.

The S3 packages include bucket configuration primitives, object operations,
multipart upload primitives, presigned URLs, and adapter-level transfer helpers
for streaming file upload and download. The optional S3 simulator package
provides deterministic in-memory S3 for tests.
