# S3 0.3 Endpoint Config String Boundary

Status: done
Issue: none
Goal: Make endpoint configuration and bucket-create region string-facing at the public boundary.
Risk: high
Verification: dune build @packages/awskit-s3/all && dune build @test/awskit-s3/runtest

## Context

Depends on: plan 01 for the clean public API direction.

Public callers should not construct `Awskit.Endpoint.t` or `Awskit.Region.t`
just to configure S3 endpoints or create buckets. Constructors parse strings at
the boundary and return structured `Awskit.Error.t` failures.

## Files Likely To Change

- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/endpoint_config.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/endpoint_config.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/endpoint_resolver.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/endpoint_resolver.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/awskit_s3.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/awskit_s3.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/bucket.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/bucket.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/**`
- `/Users/abdllahdev/dev/awskit/examples/**`

## Acceptance Criteria

- `Bucket.Create.options` and `options_exn` accept `?region:string`.
- `Endpoint_config.s3_compatible`, `local_plaintext`, and `unsafe_plaintext`
  accept `endpoint:string` and `signing_region:string`.
- Every endpoint-config constructor that parses strings returns
  `(t, Awskit.Error.t) result`.
- `unsafe_plaintext` no longer returns bare `t`; add `unsafe_plaintext_exn`
  only as a deliberate convenience bridge.
- `Endpoint_config.endpoint` and `Endpoint_config.signing_region` accessors
  remain typed because they return parsed internal facts.
- `Awskit_s3.endpoint_config` and presigned endpoint-config aliases use
  `Endpoint_config.t`, not `Endpoint_resolver.t`, as the normal public surface.
- `Endpoint_resolver` is public only if custom runtime signatures still need it
  and is documented as an advanced/internal-style surface.

## Tasks

- [x] Change `Bucket.Create.options`:
  ```ocaml
  val options :
    ?region:string -> unit -> (options, Awskit.Error.t) result
  ```
- [x] Parse bucket-create region with `Awskit.Region.of_string` inside
  `Bucket.Create.options`; keep the stored field typed.
- [x] Change endpoint config constructors to parse `endpoint:string` with
  `Awskit.Endpoint.of_string` and `signing_region:string` with
  `Awskit.Region.of_string`.
- [x] Change `unsafe_plaintext` to return result and add
  `unsafe_plaintext_exn` if call sites need a raising bridge.
- [x] Update MinIO setup, examples, and tests that currently parse endpoint or
  signing region before calling endpoint config constructors.
- [x] Change root endpoint config aliases and runtime signatures to prefer
  `Endpoint_config.t`.
- [x] Re-run audits for `Endpoint_resolver.t`, `Awskit.Endpoint.t`, and
  `Awskit.Region.t` in public S3 `.mli` files.

## Completion Notes

- `Bucket.Create.options` and `options_exn` now accept `?region:string` and
  parse it at the boundary.
- `Endpoint_config.s3_compatible`, `local_plaintext`, and
  `unsafe_plaintext` now accept endpoint/signing-region strings. The unsafe
  constructor returns a result, with `unsafe_plaintext_exn` as the explicit
  raising bridge.
- Root S3 endpoint config aliases and request contexts now expose
  `Endpoint_config.t` for the normal public surface.
- Verification passed:
  - `dune build @packages/awskit-s3/all`
  - `dune build @test/awskit-s3/runtest`
  - `dune build @examples @doc`
- Public-interface audit has no remaining typed endpoint-config constructor
  arguments. Remaining typed region matches are internal/advanced endpoint
  accessors, `Endpoint_resolver`, and presigned APIs scheduled for plan 04.

## Exact Verification Commands

```bash
dune build @packages/awskit-s3/all
```

```bash
dune build @test/awskit-s3/runtest
```

```bash
rg -n "endpoint:Awskit\\.Endpoint\\.t|signing_region:Awskit\\.Region\\.t|region:Awskit\\.Region\\.t|type endpoint_config = Endpoint_resolver\\.t" packages/awskit-s3 --glob '*.mli'
```

Expected signal: no normal public user-facing matches. Remaining resolver
matches must be custom-runtime internals or explicitly documented exceptions.

## Rollback Notes

- Do not keep typed overloads for compatibility.
- If `Endpoint_resolver` cannot be hidden yet, keep it only as an advanced
  custom-runtime surface and stop aliasing it as normal endpoint config.
