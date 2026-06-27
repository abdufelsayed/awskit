# Security Policy

This policy describes how to report vulnerabilities and how Awskit treats
security-sensitive SDK material.

## Supported Versions

Security support follows [SUPPORT.md](SUPPORT.md). The supported line is the
latest released version plus the active release branch until the next release
replaces it.

## Reporting A Vulnerability

Prefer GitHub private vulnerability reporting when it is enabled for this
repository. If it is not enabled, contact the maintainer listed in
`dune-project` and package metadata.

Do not include AWS access keys, session tokens, presigned URLs, Authorization
headers, metadata service tokens, private keys, or object data in public reports.

Reports are most useful when they include:

- the affected package and version or commit;
- the relevant runtime adapter;
- a minimal reproduction that uses dummy credentials and local/simulator data;
- the expected impact;
- whether raw diagnostic APIs such as `Awskit.Error.Unsafe_diagnostics` were
  involved.

## Sensitive Material

Treat the following as sensitive:

- secret access keys;
- session tokens;
- Authorization headers;
- metadata service tokens and responses;
- presigned URLs and signed query parameters;
- private TLS keys;
- object data and user metadata when the application treats them as private.

Presigned URLs are bearer tokens. AWS documents them as time-limited access
grants. Protect them the same way you protect the credentials and permissions
used to create them.

## Current Protections

`Awskit.Credentials.t` is opaque. The secret access key is retained for signing
but is not exposed by accessors. Session tokens are credential material and are
exposed only through the explicit `session_token` accessor because signing and
presigning need them.

Credential providers return structured resolution outcomes:

- `Resolved` supplies credentials;
- `Unavailable` means a provider was not configured and a chain may continue;
- `Invalid` means configured credential material is malformed or unsupported;
- `Failed` means an applicable provider could not complete lookup.

Chains continue only after `Unavailable`. Invalid configured credentials and
provider failures stop the chain.

Public `Awskit.Error` diagnostics redact modeled service bodies and known
secret-bearing fields by default. Raw service headers, bodies, and unredacted
sexps are available only through `Awskit.Error.Unsafe_diagnostics`.

`Awskit_s3.Presigned.result` keeps the raw bearer URL behind
`Awskit_s3.Presigned.reveal_url`. Use `safe_uri`, `method_`,
`signed_headers`, expiry accessors, and `pp` for logs and user-facing output.
Reveal the URL only at the handoff to the component that will execute the
request.

Endpoint policy constructors reject unsafe endpoint components and require
plain HTTP to be local or explicitly unsafe.

## Caller Responsibilities

Do not store long-lived production credentials in source code. Prefer temporary
credentials and standard provider sources where available.

Do not log raw presigned URLs, Authorization headers, security tokens, metadata
service responses, or raw `Unsafe_diagnostics` output.

Do not assume arbitrary object keys, metadata, custom credential source labels,
or application-provided diagnostic strings are secret-redacted. Keep secrets out
of those fields unless the application has its own redaction boundary.

Eio adapters use caller-owned HTTPS transport policy. Applications are
responsible for TLS configuration, CA roots, RNG initialization, plaintext HTTP
decisions, and platform policy. Use HTTP-only Eio transport only with explicit
local/plaintext endpoint configuration.

Custom runtimes built with `Awskit_s3.Make` must preserve the same credential,
endpoint, redaction, retry, timeout, cancellation, and response-body cleanup
contracts as the supplied adapters.

## Out Of Scope

Awskit does not design application IAM policies, prove complete S3 feature
coverage, validate arbitrary S3-compatible providers, or require live AWS
account tests unless [SUPPORT.md](SUPPORT.md) is updated to promise that
coverage.

## Evidence

Relevant executable evidence lives in:

- `test/awskit/test_core_contracts.ml`;
- `test/awskit/runtime/test_runtime_contracts.ml`;
- `test/support/runtime_http_workload.ml`;
- `test/awskit/eio/test_runtime_http_workload_eio.ml`;
- `test/awskit/lwt/test_runtime_http_workload_lwt.ml`;
- `test/awskit-s3/protocol/test_protocol_pbt.ml`;
- `test/awskit-s3/protocol/test_protocol_fixtures.ml`;
- `test/awskit-s3/protocol/test_fuzz_replay.ml`;
- `test/awskit-s3/fixtures/protocol/presign/`;
- `test/awskit-s3/fixtures/protocol/fuzz-replay/`;
- `docs/security-threat-model.md`.
