# 0.2.0

Awskit 0.2.0 is a breaking SDK hardening release. It moves the public API
toward typed S3 values, scoped streaming bodies, structured diagnostics, and
release evidence that downstream runtime authors can rely on.

## Breaking

- Split the custom-runtime contract into grouped capabilities for IO, request
  and response bodies, transport, clock, sleep, random, credentials, endpoint,
  retry, timeout, and S3 endpoint policy. Custom runtimes should implement the
  grouped `Awskit.Runtime.S` and `Awskit_s3.RUNTIME` modules, including the
  request-body `write_subbytes` hook. (4ec4e2d, 27708c0)
- Runtime and S3 constructors now accept string `~region` and `~endpoint`
  values where validation can fail, and return structured validation errors
  instead of requiring callers to prebuild every core value. Eio callers now
  supply their own HTTPS policy, and S3 clients use
  `?endpoint_config:Awskit_s3.Endpoint_config.t` instead of raw endpoint,
  scheme, addressing-style, and endpoint-variant option groups. (#8, c53921f;
  2c19cd0; 6d09f89)
- Reworked `Awskit.Error` into an opaque consumer API with constructors,
  context tagging, and exception bridge helpers under
  `Awskit.Error.Producer`. Public diagnostics are redacted by default, while
  credential provider failures expose structured source and expiration
  categories. (#7, ca7be3c; 129d1f4; 0cfa463; be75643)
- Reworked S3 object bodies around adapter `Body` and `Reader` modules,
  scoped response readers, explicit request-body replayability, and bounded
  in-memory helpers. Use client `Object.put` and `Object.get ~consume` for
  streaming workflows; use `put_string`, `put_bytes`, `get_string`,
  `get_bytes`, `find_string`, and `find_bytes` for bounded string/bytes
  workflows; use transfer helpers for file workflows. (#5, 53a7642; 754c1ba;
  162d533; bebf0f4)
- Standardized S3 operation APIs around typed bucket names, object keys,
  metadata, tag sets, account IDs, content/header values, ranges, listing
  pages, continuation tokens, operation option builders, and field-addressable
  result records. Callers should construct domain values with the public S3
  domain modules and operation `options`/`options_exn` builders instead of
  passing raw strings or relying on tuple-style results. (138ab16, 9b8d90a,
  c3c111b, ba454b1, 3a61109, 69d795c)
- Bucket no-payload operations and `Object.exists` now take a trailing `unit`
  argument when optional arguments are present, listing collection helpers
  require explicit `max_pages` bounds, `Bucket.Get_location.result` returns a
  concrete region, and raw query signing helpers were removed from
  `Awskit.Signing`. (2c19cd0, c3c111b, ba454b1, 3a61109)
- Reworked multipart upload APIs around typed upload handles, typed part
  numbers, completed-part values, ownership-aware cleanup, caller-owned resume
  handles, and invariant-preserving transfer option values built through the
  public constructors. (aab863c, c801d6f, 69d795c)
- High-level Lwt Unix and Eio transfer callbacks now receive
  `Awskit_s3.Transfer.progress` records instead of raw byte counts, and
  transfer results now use named `Put`, `Multipart`, `Get`, and `Ranged`
  records with byte-count accessors. `Body.of_stream` returns a result, and
  `Reader.to_string` and `Reader.to_bytes` require `max_bytes`. (754c1ba,
  2ee158f, 1388651, c801d6f, 27708c0)
- Presigned requests now return safe opaque artifacts. Use `safe_uri` for logs,
  `reveal_url` only for execution, and `request_headers` for the non-host
  headers an HTTP client should pass explicitly. `HEAD Object` presigning now
  uses `Presigned.Head_object.options`, and `signed_headers` includes the
  canonical `host` header. (6d09f89, bd6f919)
- Reworked object encryption options around `Awskit_s3.Encryption`, with typed
  destination, source, customer-key, and observed encryption domains. Object,
  multipart, and presigned callers should use `encryption`,
  `destination_encryption`, `source_encryption`, or `customer_key` fields
  instead of `server_side_encryption`; SSE-C multipart uploads can now send
  customer-key headers on create, upload-part, and complete requests, and
  unknown encryption strings are preserved only in read-side response metadata.
  (04e5603)
- Removed accidental public implementation surfaces: flat S3 root operation
  aliases, request builders, XML parsers, runtime internals, adapter transfer
  internals, the S3 signature-sharing implementation module, and simulator
  implementation modules. Use the public `Awskit`, `Awskit_s3`, adapter, and
  `Awskit_s3_sim` root APIs. (fc67929, 191d03e, 733c4bc, 5bd490d, c613de7,
  2aa5523)

## Added

- Added `Awskit.Timeout` policies, explicit retry budgets, and runtime-supplied
  retry jitter. (4ec4e2d)
- Added `Awskit_s3.Endpoint_config` builders for AWS endpoints,
  S3-compatible endpoints, local plaintext endpoints, and explicitly unsafe
  plaintext endpoints. (6d09f89)
- Added native S3 streaming upload and download APIs, adapter `Body` and
  `Reader` modules, bounded string/bytes object helpers, option-returning
  object lookup helpers, and reusable transfer option/planning helpers. (#5,
  53a7642; #7, ca7be3c; 754c1ba; 2ee158f; 1388651)
- Added structured SDK errors with operation, retry, decode, body, service,
  timeout, cancellation, and S3 context, plus redacted human and S-expression
  diagnostics. (#7, ca7be3c; 129d1f4)
- Added live S3 examples for put/get, listing, presigning, file transfer, and
  object metadata workflows, plus simulator-backed examples that run without
  network credentials. (#6, 088b1ea; #8, c53921f; 475bf37; 5d4a677)
- Added a public S3 support matrix covering supported object, bucket,
  multipart, presign, transfer, endpoint, simulator, MinIO, and unsupported
  feature boundaries. (cc18e20)

## Fixed

- Runtime cleanup now preserves consumer exceptions and native cancellation
  while still draining or discarding response bodies, aborting owned multipart
  uploads after failures, canceling Lwt request-body producers on transport
  exits, closing Eio request-body switches per attempt, and classifying Unix
  metadata credential timeouts and cancellation consistently. (4ec4e2d,
  391b216, 15d40de, d804565, e995ac5)
- Eio response readers now latch EOF so chunked keep-alive responses do not
  block during cleanup drains after the final chunk has already been read.
  (#10, f02497a)
- Transfer helpers now share the runtime-neutral planner, preserve callback
  exceptions and cancellation through owned cleanup, validate
  `Error_if_exists` downloads before transport, use `PutObject` for empty file
  uploads, stream file plans in bounded batches, avoid stale Eio upload chunks,
  and cancel blocked Eio multipart siblings before cleanup or abort runs. (#11,
  2ee158f; 1388651; c801d6f; 27708c0; 4a723ff)
- Hardened S3 request and response validation for malformed modeled XML,
  numeric fields, booleans, enum values, response headers, explicit checksum
  values, SSE-KMS key IDs, CopyObject replacement metadata, duplicate presigned
  signed headers, invalid S3 Transfer Acceleration combinations, and bracketed
  IPv6 endpoint authorities. (6213acd, fa14e23, e83d337, 3732f9a, bd6f919,
  ba454b1, 3a61109, a23f2ee)
- Preserved supported compatibility behavior while tightening validation:
  empty `ListObjectVersions` marker elements are treated as absent, supported
  S3-compatible payload-hash forms remain accepted, future response-only
  checksum and enum values are preserved when modeled, unknown outbound checksum
  values are rejected before request construction, unmodeled storage classes can
  be sent through `Storage_class.Other`, and bodiless HTTP responses do not
  consume response bodies even when framing headers are present.
  (fa14e23, e83d337, ba454b1, a23f2ee, e8d7032, c86e4de, 15a57ec)
- S3 helpers now reuse the same lower request primitives as the primitive
  operations, so `Object.find` shares `Object.get` request construction and
  context behavior, presigned helpers share endpoint-config signing paths, and
  `ListParts` collection helpers report `max_pages` truncation instead of
  silently returning partial results. (bebf0f4)
- Simulator behavior now matches the scoped reader and validation contracts
  more closely: escaped readers fail after `with_reader`, stream request bodies
  are materialized only when the operation executes, object and multipart
  errors carry S3 operation/resource context, and outbound write validation
  uses the shared header rules. (b5e5e4d, e8d7032)

## Documentation, CI, and Release

- Expanded README, package guides, and odoc coverage for package selection,
  runtime/client construction, S3 streaming, object workflows, transfers,
  presigning, structured errors, simulator usage, and supported S3 feature
  scope. (#3, 0e2021c; 4a46fb0; 0fe12dc; 703937e; 5d4a677; cf9f8bc;
  b7f3158; 97288fa; cc18e20)
- Added `SUPPORT.md`, `SECURITY.md`, a security threat model, maintainer
  workflow docs, release gates, and PR templates for release, changelog, CI,
  documentation, bugfix, breaking-change, and OCaml development work.
  (475bf37, 2aa5523, ba1f090, d694da9, 17e5fff, 429d376)
- Added CI coverage for release branch pushes, non-PR workflow events, and
  OCaml 4.14 non-Eio package builds. (058c0db, 8631060)
- Expanded local and CI release evidence to cover examples, generated package
  documentation, distribution artifacts, package-scoped release gates, MinIO
  contracts, protocol fixtures, fuzz replay, paginator property tests, runtime
  conformance suites, and stricter release validation scripts. (7c08cb7,
  bc860a0, d804565, d523c8c, 3187e10, 107a815, 5009f56, be2e099, f569daa,
  6d99629)
- Added GitHub Pages publishing for generated package documentation on `main`
  pushes and updated package documentation URLs to the Pages site. (d5abfa6)

# 0.1.0

Initial public release of Awskit.

Includes core AWS signing, credentials, endpoints, request/response types,
retry support, runtime abstractions, Unix credential helpers, Lwt and Eio
runtime adapters, and focused S3 support for general-purpose buckets.

The S3 packages include bucket configuration primitives, object operations,
multipart upload primitives, presigned URLs, and adapter-level transfer helpers
for streaming file upload and download. The optional S3 simulator package
provides deterministic in-memory S3 for tests.
