# MinIO Test Double Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make MinIO's role explicit as a local S3-compatible test double and
add small real-stack coverage that complements simulator and protocol evidence.

**Architecture:** Keep the existing Lwt Unix MinIO contract as the main
Docker-backed local integration gate. Add Eio MinIO smoke only as a small
coverage layer, not as a duplicate full contract.

**Tech Stack:** OCaml, Dune aliases, Alcotest, Docker Compose, MinIO,
`awskit-s3-lwt-unix`, `awskit-s3-eio`.

---

## Related Draft

For deeper rationale, ownership boundaries, CI cost controls, and MinIO
capability guidance, read:

- `docs/testing-strategy/drafts/2026-06-25-minio-test-double.md`

## No-Fix Rule

This is a test-construction and documentation pass. Do not edit S3 client
implementations. If MinIO exposes a real integration bug, keep the test and
record the evidence.

Read-only production files for this pass:

- `packages/awskit-s3/lwt/**/*.ml`
- `packages/awskit-s3/lwt/unix/**/*.ml`
- `packages/awskit-s3/eio/**/*.ml`
- `packages/awskit/lwt/**/*.ml`
- `packages/awskit/eio/**/*.ml`

Package docs under `packages/awskit-s3/doc/**` are editable.

## Files

- Modify `test/awskit-s3/lwt/unix/test_minio_contract.ml`
- Modify `docs/testing.md`
- Modify `docs/ci.md`
- Modify `packages/awskit-s3/doc/support_matrix.mld`
- Modify `packages/awskit-s3/doc/guides.mld`
- Create `test/awskit-s3/eio/test_minio_smoke.ml`
- Modify `test/awskit-s3/eio/dune`
- Modify `.github/workflows/main.yml` only if Eio smoke is promoted to CI

## Task 1: Lwt MinIO Contract Boundary

- [ ] Add a concise comment near `Minio_subject.capabilities` in
  `test/awskit-s3/lwt/unix/test_minio_contract.ml`.

- [ ] The comment must say:

  - MinIO is a local S3-compatible test double.
  - MinIO is not the AWS S3 semantic oracle.
  - Capability differences belong here only when the shared contract or
    assertion branches use them.

- [ ] Confirm `expected_capability_differences` only names differences encoded
  by `test/awskit-s3/support/s3_contract.ml`.

- [ ] Do not add generic provider language or provider certification wording.

## Task 2: Maintainer Docs

- [ ] Update `docs/testing.md`.

  State that `@minio-contract` proves local adapter interoperability through a
  real S3-compatible service and remains outside no-network protocol gates.

- [ ] Update `docs/ci.md`.

  State that the `s3-minio-contract` job is a Docker-backed local integration
  job, separate from `protocol-evidence`.

- [ ] Keep `@minio-contract` out of `@check-fast` and `@check-protocol`.

## Task 3: Package Docs

- [ ] Update `packages/awskit-s3/doc/support_matrix.mld`.

  Preserve the support boundary: arbitrary S3-compatible providers are not
  certified.

- [ ] Update `packages/awskit-s3/doc/guides.mld`.

  Describe local MinIO usage as a development and testing workflow, not a
  provider support guarantee.

## Task 4: Eio MinIO Smoke

- [ ] Create `test/awskit-s3/eio/test_minio_smoke.ml`.

- [ ] Use the same environment variable names as the Lwt Unix MinIO contract:

  - `AWSKIT_S3_MINIO_ENDPOINT`
  - `AWSKIT_S3_MINIO_UNSAFE_HTTP`
  - `AWSKIT_S3_MINIO_ACCESS_KEY_ID`
  - `AWSKIT_S3_MINIO_SECRET_ACCESS_KEY`
  - `AWSKIT_S3_MINIO_REGION`

- [ ] Use one fresh bucket name with the process id in it.

- [ ] Run this public workflow through `awskit-s3-eio`:

  - Create bucket.
  - Put object.
  - Head object.
  - Get object.
  - Range read.
  - Delete object.
  - Delete bucket.

- [ ] Ensure cleanup runs if a test assertion fails after bucket creation.

- [ ] Add the test module to `test/awskit-s3/eio/dune`.

- [ ] Add a focused alias:

  ```scheme
  (rule
   (alias minio-smoke-eio)
   (package awskit-s3-eio)
   (deps test_awskit_s3_eio.exe)
   (action
    (run ./test_awskit_s3_eio.exe test "integration:awskit-s3-eio:minio-smoke")))
  ```

- [ ] Keep the Eio smoke small. Do not duplicate the full Lwt Unix shared
  contract in this pass.

## Task 5: CI Decision

- [ ] First run Eio MinIO smoke locally with Docker.

- [ ] If the smoke is fast and stable, add it to the existing
  `s3-minio-contract` job or to a separate Eio MinIO job with OCaml 5 only.

- [ ] If the smoke adds too much setup cost for PRs, document it as
  release/scheduled-only in `docs/ci.md` and leave it out of PR CI.

- [ ] Do not include Eio MinIO smoke in no-network aliases.

## Task 6: Validation

- [ ] Run Lwt MinIO contract:

  ```sh
  docker compose up -d
  opam exec -- dune build --force @minio-contract
  docker compose down -v
  ```

- [ ] Run Eio MinIO smoke:

  ```sh
  docker compose up -d
  opam exec -- dune build --force @test/awskit-s3/eio/minio-smoke-eio
  docker compose down -v
  ```

- [ ] Run docs and whitespace validation:

  ```sh
  opam exec -- dune build @doc
  git diff --check
  ```

- [ ] If a MinIO assertion fails because of product behavior, write failure
  evidence and do not edit production code.

## Backlog

- Presigned GET or PUT roundtrip through MinIO.
- Multipart stale part and abort coverage.
- Versioned-bucket cleanup edge cases.
- Larger transfer resume variants on scheduled or release-only runs.
