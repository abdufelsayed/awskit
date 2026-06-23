# 0.2.0

This release updates S3 streaming, structured SDK errors, runtime cleanup, and
release packaging.

## Breaking

- Split the public runtime contract into named capabilities for IO, request and
  response bodies, transport, clock, sleep, random, credentials, endpoint,
  retry, and timeout. Custom runtimes should implement the grouped capability
  modules accepted by `Awskit_s3.Make`. (6cce011)
- Reworked object body APIs around adapter `Body` and `Reader` modules with
  explicit stream replayability and bounded reads. Use `Object.put_string`,
  `put_bytes`, `get_string`, `get_bytes`, `find_string`, and `find_bytes` for
  bounded in-memory workflows; use `Object.put` with `Body`,
  `Object.get ~consume` with scoped readers, and transfer helpers for larger
  file workflows. (#5, 53a7642; 5815ba4; 73ef68f)
- Reworked multipart upload APIs around typed upload handles, typed part
  numbers, named operation results, ownership-aware transfer lifecycle, and
  resume helpers that complete only from fresh `UploadPart` results.
  (aab863c)
- High-level Lwt Unix and Eio transfer progress callbacks now receive
  `Awskit_s3.Transfer.progress` events instead of raw byte counts. Use
  `progress.transferred` for the previous byte count, and inspect `direction`,
  `phase`, `total`, and `part_number` for structured transfer context.
  (2ee158f)
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
- Moved the S3 signature-sharing module out of the installed public surface.
  Runtime authors should use the public `Awskit_s3.RUNTIME`, `BODY`, `READER`,
  operation module types, `S`, and `Make` surface directly. (191d03e)
- S3 simulator implementation modules are now private. Use the public
  `Awskit_s3_sim` root API for deterministic simulator stores, connections,
  faults, history, inspection, and operation modules. (2aa5523)

## Added

- Added `Awskit.Timeout` policies and explicit retry budgets with runtime
  supplied jitter randomness. (6cce011)
- Added native streaming S3 upload and download APIs with adapter-level `Body`
  and `Reader` modules, first-class bounded string/bytes object helpers, plus
  unified multipart body handling. (#5, 53a7642; 5815ba4; 73ef68f)
- Added live S3 SDK examples for put/get, listing, presigning, file transfer,
  and object metadata workflows. (#6, 088b1ea)
- Added structured SDK errors with operation, retry, decode, body, and S3
  context, plus SDK exception helpers and option-returning object lookup
  helpers. (#7, ca7be3c)
- Added validated `Awskit_s3.Transfer` option builders, option accessors,
  runtime-neutral upload/download planning helpers with property coverage,
  structured transfer progress events, and a download overwrite policy.
  (2ee158f)

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
- Reused the shared transfer planner from Lwt Unix and Eio helpers, propagated
  transfer progress callback exceptions and cancellation consistently after
  owned cleanup, and added pre-transport validation for
  `Error_if_exists` downloads. (2ee158f)
- Hardened S3 wire-format and request validation, rejecting malformed modeled
  XML, invalid numeric fields, bad response headers, and invalid CopyObject
  replacement metadata while preserving supported S3-compatible marker and
  payload-hash forms. (6213acd, fa14e23, e83d337, 3732f9a)
- Preserved Lwt timeout errors over request-body cleanup cancellation races.
  (d804565)

## Documentation, CI, and Release

- Added root support and security policies, simulator-backed no-network S3
  examples, docs content policy checks, and CI/release gates for examples and
  documentation claims. (475bf37)
- Added an installed API snapshot gate, release governance checks, branch and
  ruleset release verification, and PR template prompts for API/support impact
  review. (2aa5523)
- Installed Eio HTTPS example-only dependencies in documentation/example CI and
  local release validation without adding them to published package
  dependencies. (d804565)
- Included paginator property coverage in the `@protocol-pbt` evidence alias so
  pagination PBT runs with the rest of the deterministic protocol checks.
  (3187e10)
- Expanded the `@runtime-conformance` evidence alias to run the existing Lwt,
  Eio, simulator, and recording-runtime law suites directly. (107a815)
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
