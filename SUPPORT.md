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
| Environment variables | supported | `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, and `AWS_SESSION_TOKEN`. |
| Shared credentials/config profiles | supported | Static profiles from standard AWS files. |
| ECS/container credentials | supported in Lwt Unix | Uses the Lwt Unix metadata provider where implemented. |
| EC2 IMDS credentials | supported in Lwt Unix | Uses IMDSv2 when available and supports documented opt-outs/fallback controls. |
| Web identity, process, SSO, STS assume-role profiles | unsupported | Add executable provider evidence before documenting these as supported. |

## S3 Scope

| Area | Status | Runtime coverage | Simulator coverage | MinIO coverage | Notes |
| --- | --- | --- | --- | --- | --- |
| Object put/get/head/delete/copy | supported | Lwt Unix, Eio, custom runtimes via `Awskit_s3.Make` | yes | yes, where contract tests cover it | Includes typed bucket/key values and structured results. |
| Bounded string/bytes object helpers | supported | Lwt Unix, Eio, simulator | yes | yes, through object contracts | `max_bytes` is required for buffered downloads. |
| Streaming bodies and readers | supported | Runtime adapters and custom runtimes | yes | partial | Response readers are scoped to the runtime callback. |
| ListObjectsV2 and pagination helpers | supported | Lwt Unix, Eio, simulator | yes | yes, where contract tests cover it | Collection helpers require explicit `max_pages`. |
| Basic bucket create/delete/head/list/location | supported | Lwt Unix, Eio, simulator | yes | partial | This does not mean S3 Lifecycle configuration. |
| Metadata and object tags | supported | Lwt Unix, Eio, simulator | yes | partial | Validated typed values are used at the API boundary. |
| Range reads | supported | Lwt Unix, Eio, simulator | yes | partial | Large downloads should use streaming consumers. |
| Presigned request artifacts | supported | Standalone helpers and runtime-bound adapters | yes | fixture and unit evidence | Raw bearer URLs are exposed only through explicit reveal APIs. |
| Multipart upload | supported | Lwt Unix, Eio, simulator | yes | partial | Includes typed upload handles and owned cleanup behavior. |
| Local file transfer helpers | supported in Unix-capable adapters | Lwt Unix and Eio | no | partial | Filesystem helpers live in adapter packages, not in `awskit-s3` core. |
| Credentials, signing, endpoints, retry, timeout, cancellation, cleanup | supported | Core and runtime adapters | yes | partial | Runtime conformance and protocol evidence define the shared laws. |
| S3 bucket policy payloads | supported, scoped | Lwt Unix and Eio | partial | untested | Awskit validates and sends policy JSON; application IAM policy design is out of scope. |
| Selected bucket configuration subresources | supported, scoped | Lwt Unix and Eio | partial | untested | Only modeled APIs with tests/docs are supported. This is not full S3 configuration coverage. |
| Object lock and legal hold | unsupported | none | none | none | No support claim. |
| Inventory, analytics, replication, lifecycle | unsupported | none | none | none | No support claim. |
| Access points and directory buckets | unsupported | none | none | none | No support claim. |
| Unmodeled server-side encryption variants | unsupported | none | none | none | Only modeled request/response values are in scope. |
| Unmodeled ACL and policy edge cases | unsupported | none | none | none | No broad IAM/S3 policy coverage claim. |
| Unlisted S3-compatible providers | unsupported | none | none | none | MinIO is the named local contract target only where stated. |
| Live AWS account compatibility | untested by release gate | optional/manual only | no | no | Live AWS is not a release gate unless this policy is updated to promise live AWS coverage. |

## S3-Compatible Storage

Awskit targets AWS S3 semantics. MinIO is the named local S3-compatible
contract target where `@minio-contract` states coverage. That coverage does not
imply that arbitrary S3-like providers behave like AWS or are supported by
Awskit.

Plain HTTP is local or unsafe by construction. Use explicit endpoint policy
constructors for local plaintext and custom endpoints. Endpoint, signing
region, addressing style, TLS/HTTP policy, and feature support define any
S3-compatible deployment.

## Testing Evidence

The supported scope is backed by a layered test model:

- deterministic unit/integration tests;
- compile-only API tests;
- protocol property tests and golden fixtures;
- simulator contracts;
- runtime conformance tests;
- MinIO contracts for the stated local S3-compatible target;
- odoc builds and compile-tested example executables.

Run the usual local evidence with:

```sh
opam exec -- dune build @check-fast @check-protocol @doc
```

Run MinIO contracts explicitly when Docker is available:

```sh
docker compose up -d
opam exec -- dune build --force @minio-contract
docker compose down -v
```
