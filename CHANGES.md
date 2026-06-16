# 0.2.0

This release updates S3 streaming, structured SDK errors, runtime cleanup, and
release packaging.

## Breaking

- Replaced object-specific string and file shortcuts with adapter `Body` and
  `Reader` helpers for uploads and downloads. Use `Object.put` with `Body`
  helpers, `Object.get ~consume` with scoped readers, and transfer helpers for
  larger file workflows. (#5, 53a7642)
- Eio callers now provide their own HTTPS policy when creating runtime and S3
  clients. (#8, c53921f)
- Moved `Awskit.Error` constructors, context tagging, and exception bridge
  helpers under `Awskit.Error.Internal`; application code should use the
  top-level `Awskit.Error` consumer API. (0cfa463)
- High-level runtime and S3 constructors now accept string `~region` and
  `~endpoint` values and return structured validation errors when those values
  are invalid. Bucket no-payload operations now require a trailing `unit`
  argument. (2c19cd0)
- Request builders, XML parsers, runtime internals, and adapter transfer helper
  modules are now private implementation modules. Use the public `Awskit`,
  `Awskit_s3`, and adapter APIs. (fc67929)

## Added

- Added native streaming S3 upload and download APIs with adapter-level `Body`
  and `Reader` modules, plus unified multipart body handling. (#5, 53a7642)
- Added live S3 SDK examples for put/get, listing, presigning, file transfer,
  and object metadata workflows. (#6, 088b1ea)
- Added structured SDK errors with operation, retry, decode, body, and S3
  context, plus SDK exception helpers and option-returning object lookup
  helpers. (#7, ca7be3c)

## Fixed

- Clean up scoped response bodies after consumer exceptions while preserving the
  original exception, and abort fresh multipart uploads when post-create work
  raises or is canceled. (391b216)
- Reject malformed S3 XML response roots and malformed known fields for object
  lists, object versions, multipart parts, and tagging entries while preserving
  tolerance for unknown XML elements. (6213acd)
- Validate CopyObject replacement metadata before request construction, treat
  malformed response headers as decode failures, and accept uppercase SHA-256
  payload hashes by normalizing them before validation. (fa14e23)
- Treat empty `ListObjectVersions` pagination marker elements as absent while
  keeping strict validation for malformed non-empty version IDs. (e83d337)
- Pin high-level ranged downloads to the object observed by `HeadObject` by
  reusing the returned version ID or adding an ETag `If-Match` precondition for
  ranged `GetObject` requests. (b251b54)
- Reject negative S3 XML sizes, key counts, multipart list markers, and enforce
  the valid multipart part number range when decoding list-parts responses.
  (3732f9a)

## Documentation, CI, and Release

- Expanded public API documentation across core Awskit modules, S3 operations,
  adapter entrypoints, simulator APIs, and guide examples. (#3, 0e2021c)
- Updated README examples for the v0.2.0 S3 streaming API. (4a46fb0)
- Added CI coverage for release branch pushes and OCaml 4.14 non-Eio package
  builds. (058c0db)
- Added local release validation for distribution artifacts and MinIO contract
  tests. (7c08cb7)
- Prepared package documentation publishing and release archive documentation
  validation. (bc860a0)
- Clarified how to choose the right package, configure S3 clients, use object
  and transfer workflows, and handle structured errors. (0fe12dc)
- Ensured workflow jobs run for push, schedule, and manual events while still
  skipping draft pull request runs. (8631060)
- Added GitHub Pages publishing for package documentation on main pushes and
  updated package documentation URLs to the Pages site. (d5abfa6)
- Added agent-facing maintenance workflow docs and PR templates for default,
  release, bugfix, documentation, CI, and breaking-change review flows.
  (ba1f090)
- Added agent-facing OCaml development guidance for interface design, runtime
  adapters, error handling, resource cleanup, wire parsing, tests, and package
  boundaries. (d694da9)

# 0.1.0

Initial public release of Awskit.

Includes core AWS signing, credentials, endpoints, request/response types,
retry support, runtime abstractions, Unix credential helpers, Lwt and Eio
runtime adapters, and focused S3 support for general-purpose buckets.

The S3 packages include bucket configuration primitives, object operations,
multipart upload primitives, presigned URLs, and adapter-level transfer helpers
for streaming file upload and download. The optional S3 simulator package
provides deterministic in-memory S3 for tests.
