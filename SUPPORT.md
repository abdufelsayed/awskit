# Support Policy

This policy defines what Awskit treats as supported for the current release
branch. It is intentionally narrower than "all AWS" or "all S3".

## Release Status

Awskit is pre-1.0. Breaking public changes are allowed when they improve code
quality, maintainability, DX, ergonomics, correctness, or production readiness.
The supported line is the latest released version plus the active release
branch until the next release replaces it.

## Platforms

| Area | Status | Notes |
| --- | --- | --- |
| Non-Eio packages | supported | Tested on OCaml 4.14 and the current OCaml 5 compiler in CI. |
| Eio packages | supported | Require OCaml 5.2 or newer as declared in package metadata. |
| Linux | supported | Covered by CI. |
| macOS | supported | Covered by CI. |
| Windows | unsupported | No CI or support policy exists yet. |

## Packages And Runtimes

| Package | Status | Notes |
| --- | --- | --- |
| `awskit` | supported | Runtime-neutral credentials, regions, endpoints, signing, errors, retry, timeout, request/response metadata, and runtime contracts. |
| `awskit-unix` | supported | Unix environment/profile credential and region helpers. No networking. |
| `awskit-lwt` | supported | Generic Lwt runtime over a caller-supplied Cohttp Lwt client. |
| `awskit-lwt-unix` | supported | Ready Lwt Unix runtime using Cohttp Lwt Unix and standard AWS environment/profile/metadata sources. |
| `awskit-eio` | supported | Eio runtime adapter with caller-owned HTTPS, TLS, CA roots, RNG initialization, and platform policy. |
| `awskit-s3` | supported | Runtime-neutral S3 client core for the supported S3 scope below. |
| `awskit-s3-sim` | supported | Deterministic in-memory simulator root API for tests and documentation workflows. Simulator internals are not a supported application API. |
| `awskit-s3-lwt` | supported | S3 adapter over the generic Awskit Lwt runtime. |
| `awskit-s3-lwt-unix` | supported | Ready Lwt Unix S3 client. |
| `awskit-s3-eio` | supported | Eio S3 client with caller-owned HTTPS transport policy. |
| Ready Eio Unix aggregate package | unsupported | Awskit does not currently provide or claim a ready Eio Unix aggregate adapter. |

## Credential Provider Support

| Credential source | Status | Notes |
| --- | --- | --- |
| Explicit static credentials | supported | Useful for tests and controlled configuration. Long-lived credentials in source code are not recommended. |
| Environment variables | supported, scoped | `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, and `AWS_SESSION_TOKEN`. Package-owned tests cover complete and partial environments plus provider-chain stop behavior. |
| Shared credentials/config profiles | supported, scoped | Static profiles from standard AWS files. Package-owned tests cover credentials-file precedence, config profile sections, invalid partial profiles, and unsupported assume-role profiles. |
| ECS/container credentials | supported in Lwt Unix, scoped | Package-owned tests cover relative and safe full endpoints, authorization headers, JSON parsing, refresh-window caching, timeout, and native cancellation. |
| EC2 IMDS credentials | supported in Lwt Unix, scoped | Package-owned tests cover IMDSv2 token/role/credentials flow, disabled metadata, explicit and environment-driven IMDSv1 fallback policy, timeout, and native cancellation. |
| Web identity, process, SSO, STS assume-role profiles | unsupported | Add executable provider evidence before documenting these as supported. |

## S3 Scope

The detailed release-reader feature matrix lives in the generated S3 package
documentation:
[packages/awskit-s3/doc/support_matrix.mld](packages/awskit-s3/doc/support_matrix.mld).

| Area | Status | Notes |
| --- | --- | --- |
| Object put/get/head/delete/copy, buffered helpers, streaming readers, ListObjectsV2, version-aware object APIs, object tagging, metadata, ranges, modeled preconditions/checksums/storage class/encryption, bucket basics, selected bucket configuration subresources, presigned artifacts, multipart upload, and adapter-owned file transfer helpers | supported or supported, scoped | See the package S3 support matrix for package/runtime coverage, simulator evidence, MinIO capability-profile notes, and operation boundaries. |
| Simulator | supported, scoped | Deterministic in-memory application-level evidence for tests and documentation workflows. It is not live AWS coverage or a wire-protocol authority. |
| Local S3-compatible integration target | supported, scoped | MinIO is the current named local S3-compatible integration target only where `@s3-local-service` covers the behavior. Known capability-profile differences are documented in the package S3 support matrix. |
| Access points, Object Lambda, S3 on Outposts, directory buckets, Object Lock/legal hold/retention/governance bypass, MFA delete, inventory, analytics, replication, lifecycle, metrics, notifications, logging, website hosting, requester-pays configuration, broad ACL/IAM/policy semantics, unmodeled SSE/checksum variants, arbitrary S3-compatible providers, and live AWS release-gate coverage | unsupported | No support claim for this release. Live AWS account checks remain optional/manual unless this policy is updated to promise them as a release gate. |

## S3-Compatible Storage

Awskit targets AWS S3 semantics. MinIO is the named local S3-compatible
integration target where `@s3-local-service` states coverage. That coverage does not
imply that arbitrary S3-like providers behave like AWS or are supported by
Awskit.

Plain HTTP is local or unsafe by construction. Use explicit endpoint policy
constructors for local plaintext and custom endpoints. Endpoint, signing
region, addressing style, TLS/HTTP policy, and feature support define any
S3-compatible deployment.

## Testing Evidence

The supported scope is backed by a layered test model:

- deterministic unit/integration tests;
- public `.mli`, docs, and examples review for API changes;
- protocol property tests and golden fixtures;
- simulator contracts;
- runtime HTTP workloads;
- package-owned credential, generic Lwt S3, and runtime-native file-transfer
  contracts;
- local-service integration tests for the stated S3-compatible target;
- odoc builds and compile-tested example executables.

Run the usual local tests and documentation build with:

```sh
opam exec -- dune build @correctness @doc
```

Run local-service integration tests explicitly when Docker is available:

```sh
docker compose up -d
opam exec -- dune build --force @integration
docker compose down -v
```
