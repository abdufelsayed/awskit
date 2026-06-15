# 0.2.0

This release updates S3 streaming and transfer APIs.

Breaking change: object and multipart uploads now use adapter `Body` helpers,
and downloads use scoped reader consumption with `Object.get ~consume`.
Update upload call sites to construct request bodies through the selected
runtime adapter, and consume download streams inside the provided scope.

S3 support now includes native streaming object bodies, multipart upload body
handling, and live SDK examples for common object, bucket, and transfer flows.
The README examples have been updated for the streaming API.

Awskit now provides structured `Awskit.Error` handling for SDK failures, and
the package API documentation has been expanded.

Breaking change: `Awskit.Error` constructors and exception bridge helpers now
live under `Awskit.Error.Internal`. Application code should inspect, classify,
and print returned SDK errors through the top-level `Awskit.Error` consumer API;
Awskit package implementations use `Internal` when constructing errors.

Breaking change: high-level runtime and S3 constructors now accept plain string
`~region` and `~endpoint` arguments, parsing them inside the SDK. Bucket
read/delete-style operations now take the client first and end in `unit`,
matching object operation ergonomics.

# 0.1.0

Initial public release of Awskit.

Includes core AWS signing, credentials, endpoints, request/response types,
retry support, runtime abstractions, Unix credential helpers, Lwt and Eio
runtime adapters, and focused S3 support for general-purpose buckets.

The S3 packages include bucket configuration primitives, object operations,
multipart upload primitives, presigned URLs, and adapter-level transfer helpers
for streaming file upload and download. The optional S3 simulator package
provides deterministic in-memory S3 for tests.
