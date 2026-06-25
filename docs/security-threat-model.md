# Security Threat Model

Awskit treats security-sensitive SDK material as part of the public API
contract at runtime, diagnostics, and test/documentation boundaries. The
current scope is the core SDK and supported S3 SDK packages; MinIO is the named
local S3-compatible contract target where executable contract coverage exists.

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
- HTTP transport crosses process and network boundaries and must not expose
  secret diagnostics by default.
- Local metadata services provide temporary credentials through link-local or
  container metadata endpoints.
- The simulator is an in-process test/runtime boundary and does not prove live
  AWS behavior.
- MinIO is the named local S3-compatible integration target covered by
  service-backed tests; coverage does not imply provider-wide S3-compatible
  support.
- Documentation examples are public artifacts and must avoid printing bearer
  presigned URLs or raw credentials by default.
- CI is the automated evidence boundary for builds, documentation, and tests.

## Protections And Evidence

| Protection | Current evidence | Planned evidence |
| --- | --- | --- |
| Public diagnostics redact modeled service bodies and known secret-bearing fields, while raw diagnostics stay behind explicit unsafe APIs. | `test/awskit/test_error_redaction.ml` | Expanded redaction matrix coverage as new diagnostics surfaces are added. |
| Presign APIs distinguish safe artifacts from bearer URLs and keep signed material out of default diagnostics and examples. | `test/awskit-s3/test_presigned.ml` | Additional presign artifact tests when the artifact surface expands. |
| Credential chains continue only after unavailable providers and stop on invalid configured credentials or provider failures. | `test/awskit/test_core_contract.ml`; `test/awskit/lwt/unix/test_integration.ml` | Additional credential-chain tests for any newly supported provider family. |
| Endpoint policy rejects unsafe endpoint components and requires explicit local/plaintext behavior. | `test/awskit-s3/test_endpoint.ml`; runtime integration tests under `test/awskit-s3/{lwt,eio}/test_integration.ml` | Endpoint-policy tests for new endpoint modes or contract targets. |
| Documentation and generated package docs remain buildable and avoid unsafe examples by default. | `opam exec -- dune build @doc` | Broader docs checks when docs publishing or example validation changes. |

## Review Rules

- New public printers, sexps, exceptions, and logs must be redaction-safe by
  default.
- Raw access to credentials or bearer presigned URLs must use deliberate
  `reveal_*` or `Unsafe_*` naming.
- Do not place secrets in object keys, user metadata, custom source labels, or
  application diagnostic strings and expect Awskit's public diagnostics to
  discover every application-defined secret.
- Runtime packages may expose advanced SDK APIs directly, but they must preserve
  the same redaction, credential, endpoint, and presign safety contracts.
- New provider or endpoint support must add executable evidence before the docs
  claim support.
