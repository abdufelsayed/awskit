# 0.2.0

This release updates S3 streaming, structured SDK errors, runtime cleanup, and
release packaging.

## Breaking

- Split the public runtime contract into named capabilities for IO, request and
  response bodies, transport, clock, sleep, random, credentials, endpoint,
  retry, and timeout. Custom runtimes should implement the grouped capability
  modules accepted by `Awskit_s3.Make`. (6cce011)
- Replaced object-specific string and file shortcuts with adapter `Body` and
  `Reader` helpers for uploads and downloads. Use `Object.put` with `Body`
  helpers, `Object.get ~consume` with scoped readers, and transfer helpers for
  larger file workflows. (#5, 53a7642)
- Eio callers now provide their own HTTPS policy when creating runtime and S3
  clients. (#8, c53921f)
- Moved `Awskit.Error` constructors, context tagging, and exception bridge
  helpers under `Awskit.Error.Internal`; application code should use the
  top-level `Awskit.Error` consumer API. (0cfa463)
- High-level runtime constructors now accept string `~region` and `~endpoint`
  values and return structured validation errors when those values are invalid.
  S3 constructors now take `?endpoint_config:Awskit_s3.Endpoint_config.t`
  instead of raw `~endpoint`, `~scheme`, `~addressing_style`, and
  `~endpoint_variant` groups. Use `Endpoint_config.aws`,
  `Endpoint_config.local_plaintext`, `Endpoint_config.s3_compatible`, or
  `Endpoint_config.unsafe_plaintext` to make endpoint policy explicit. Bucket
  no-payload operations now require a trailing `unit` argument. (2c19cd0)
- Standardized S3 object and bucket operation APIs around typed bucket names,
  object keys, metadata, tag sets, owner guards, content/header values,
  operation option builders, and field-addressable object get results. Callers
  should construct domain values with `Bucket_name`, `Object_key`, `Metadata`,
  `Tag.Set`, and operation `options`/`options_exn` builders instead of passing
  raw strings or relying on tuple-style get results. (138ab16)
- Request builders, XML parsers, runtime internals, and adapter transfer helper
  modules are now private implementation modules. Use the public `Awskit`,
  `Awskit_s3`, and adapter APIs. (fc67929)

## Added

- Added `Awskit.Timeout` policies and explicit retry budgets with runtime
  supplied jitter randomness. (6cce011)
- Added native streaming S3 upload and download APIs with adapter-level `Body`
  and `Reader` modules, plus unified multipart body handling. (#5, 53a7642)
- Added live S3 SDK examples for put/get, listing, presigning, file transfer,
  and object metadata workflows. (#6, 088b1ea)
- Added structured SDK errors with operation, retry, decode, body, and S3
  context, plus SDK exception helpers and option-returning object lookup
  helpers. (#7, ca7be3c)

## Fixed

- Generic Lwt runtimes now reject enabled retry policies unless the runtime
  supplies real sleep and random capabilities. S3 retries now spend a
  per-operation retry budget before sleeping. (6cce011)
- Custom and simulator response bodies preserve consumer errors over cleanup
  drain errors, while still reporting drain errors after successful consumers.
  (6cce011)
- Hardened scoped body cleanup and transfer helpers so consumer exceptions,
  canceled multipart uploads, and ranged downloads preserve resource ownership
  and object identity. (391b216, b251b54)
- Hardened S3 wire-format and request validation, rejecting malformed modeled
  XML, invalid numeric fields, bad response headers, and invalid CopyObject
  replacement metadata while preserving supported S3-compatible marker and
  payload-hash forms. (6213acd, fa14e23, e83d337, 3732f9a)

## Documentation, CI, and Release

- Expanded public API documentation across core Awskit modules, S3 operations,
  adapter entrypoints, simulator APIs, and guide examples. (#3, 0e2021c)
- Updated README and guide coverage for the v0.2.0 S3 streaming API, package
  selection, client configuration, object and transfer workflows, and structured
  error handling. (4a46fb0, 0fe12dc)
- Added CI coverage for release branch pushes, non-PR workflow events, and
  OCaml 4.14 non-Eio package builds. (058c0db, 8631060)
- Added local release validation for distribution artifacts, generated package
  documentation, and MinIO contract tests. (7c08cb7, bc860a0)
- Added GitHub Pages publishing for package documentation on main pushes and
  updated package documentation URLs to the Pages site. (d5abfa6)
- Added maintainer workflow docs and PR templates for release, changelog, CI,
  documentation, bugfix, breaking-change, and OCaml development work.
  (ba1f090, d694da9, 17e5fff)

# 0.1.0

Initial public release of Awskit.

Includes core AWS signing, credentials, endpoints, request/response types,
retry support, runtime abstractions, Unix credential helpers, Lwt and Eio
runtime adapters, and focused S3 support for general-purpose buckets.

The S3 packages include bucket configuration primitives, object operations,
multipart upload primitives, presigned URLs, and adapter-level transfer helpers
for streaming file upload and download. The optional S3 simulator package
provides deterministic in-memory S3 for tests.
