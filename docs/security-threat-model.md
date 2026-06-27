# Security Threat Model

Awskit treats security-sensitive SDK material as part of the public API contract
at runtime, diagnostics, tests, and documentation boundaries.

Related maintainer docs:

- `docs/testing.md` explains the evidence aliases referenced here.
- `docs/release-gates.md` requires support/security scope review before
  production-ready releases.
- `docs/docs-publishing.md` covers generated public documentation.

## Scope

The current scope is the core SDK and supported S3 SDK packages. MinIO is the
named local S3-compatible contract target where executable service-backed
coverage exists. The simulator is an in-process test/runtime boundary and does
not prove live AWS behavior.

## Protected Assets

- Credentials: access key IDs, secret access keys, credential source metadata,
  and credential expiration metadata.
- Session tokens from temporary credentials and metadata services.
- Presigned URLs and signed query parameters, which are bearer artifacts.
- Metadata-provider responses from local container and instance metadata
  services.
- Endpoint configuration, including hosts, schemes, signing regions, TLS policy,
  and any rejected userinfo or path/query/fragment material.
- Object metadata, headers, and operation inputs that may contain user secrets.
- Logs, exception messages, public printers, and sexps.

## Trust Boundaries

- User code constructs credentials, endpoint configuration, operation inputs,
  object metadata, and presign requests.
- Runtime packages resolve credentials, provide clocks, perform HTTP transport,
  and own provider refresh/caching behavior.
- HTTP transport crosses process and network boundaries and must keep secret
  diagnostics out of default output.
- Local metadata services provide temporary credentials through link-local or
  container metadata endpoints.
- The simulator is an in-process test/runtime boundary.
- MinIO is the named local S3-compatible integration target covered by
  service-backed tests; coverage does not imply provider-wide S3-compatible
  support.
- Documentation examples are public artifacts and should avoid printing bearer
  presigned URLs or raw credentials by default.
- CI is the automated evidence boundary for builds, documentation, and tests.

## Protections And Evidence

| Invariant | Current evidence | Expand when |
| --- | --- | --- |
| Public diagnostics redact modeled service bodies and known secret-bearing fields, while raw diagnostics stay behind explicit unsafe APIs. | `test/awskit/test_core_contracts.ml` through `@awskit-core-contracts`. | New diagnostics surfaces, printers, exceptions, or sexps are added. |
| Presign APIs distinguish safe artifacts from bearer URLs and keep signed material out of default diagnostics and examples. | `test/awskit-s3/protocol/test_protocol_pbt.ml`, `test/awskit-s3/protocol/test_protocol_fixtures.ml`, and `test/awskit-s3/fixtures/protocol/presign/**` through `@s3-protocol-laws` and `@s3-protocol-fixtures`. | Presigned operations, signed headers, or artifact printers expand. |
| Credential chains continue after unavailable providers and stop on invalid configured credentials, provider failures, or expired credentials. | `test/awskit/test_core_contracts.ml` and `test/awskit/runtime/test_runtime_contracts.ml` through `@awskit-core-contracts` and `@awskit-runtime-contracts`. | A provider family, refresh path, or runtime credential policy changes. |
| Endpoint policy rejects unsafe endpoint components and requires explicit local/plaintext behavior. | `test/awskit/test_core_contracts.ml`, `test/awskit-s3/protocol/test_protocol_pbt.ml`, and endpoint fuzz replay fixtures through `@awskit-core-contracts`, `@s3-protocol-laws`, and `@s3-protocol-replay`. | Endpoint modes, TLS policy, addressing behavior, or S3-compatible target policy changes. |
| Simulator and MinIO support claims stay scoped to their evidence boundary. | `@s3-simulator`, `@s3-local-service`, `@integration`, and support-matrix package docs. | Backend capability claims or S3-compatible provider support changes. |
| Documentation and generated package docs remain buildable and avoid unsafe examples by default. | `opam exec -- dune build @examples @doc`. | Docs publishing, examples, or public guide snippets change security-sensitive material. |

## Review Standards

- New public printers, sexps, exceptions, and logs are redaction-safe by default.
- Raw credential secrets stay unexposed. Bearer presigned URLs use deliberate
  `reveal_*` APIs, and unsafe diagnostics use deliberate `Unsafe_*` naming.
- Object keys, user metadata, custom source labels, and application diagnostic
  strings may contain application-defined secrets; Awskit public diagnostics do
  not promise to discover every caller-defined secret.
- Runtime packages may expose advanced SDK APIs directly, but they preserve the
  same redaction, credential, endpoint, and presign safety contracts.
- New provider or endpoint support adds executable evidence before public docs
  claim support.
- Documentation examples use dummy credentials or safe artifacts unless the
  point of the example is an explicit handoff of bearer material.
