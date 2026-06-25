# CI And Release Wiring Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the new testing evidence discoverable, correctly aliased, and
cost-bounded in local, CI, and release workflows.

**Architecture:** Promote only aliases that exist and run. Keep no-network
evidence in `@check-protocol`, keep Docker-backed MinIO evidence separate, and
keep release validation aligned with documented maintainer workflows.

**Tech Stack:** Dune aliases, GitHub Actions, shell release script, maintainer
docs.

---

## Related Drafts

This pass wires the evidence created by the other plans. Read the relevant
draft before changing that layer's aliases or CI placement:

- Runtime: `docs/testing-strategy/drafts/2026-06-25-runtime-http-contract.md`
- Protocol: `docs/testing-strategy/drafts/2026-06-25-protocol-fuzz-replay.md`
- Simulator: `docs/testing-strategy/drafts/2026-06-25-simulator-stateful-pbt.md`
- MinIO: `docs/testing-strategy/drafts/2026-06-25-minio-test-double.md`

## No-Fix Rule

This is a wiring and documentation pass. Do not edit production implementation
code. Failing tests discovered by wired aliases must be recorded, not fixed.

## Files

- Modify `docs/testing.md`
- Modify `docs/ci.md`
- Modify `.github/workflows/main.yml` only when a new CI command is promoted
- Modify `scripts/release-check.sh` only when a new release gate is promoted
- Modify root `dune` only if package-filtered aliases need adjustment
- Modify package or test Dune files only for aliases created by earlier passes

## Task 1: Alias Audit

- [ ] Verify these aliases exist before documenting them:

  - `@protocol-pbt`
  - `@protocol-fixtures`
  - `@fuzz-replay`
  - `@runtime-conformance`
  - `@simulator-contract`
  - `@minio-contract`

- [ ] Verify newly introduced focused aliases exist before documenting them:

  - `@test/awskit/eio/runtime-http-contract`
  - `@test/awskit/lwt/runtime-http-contract`
  - `@test/awskit-s3/sim/simulator-stateful-pbt`
  - `@test/awskit-s3/eio/minio-smoke-eio`

- [ ] Do not document `@fuzz-offline`.

## Task 2: `@check-protocol`

- [ ] Confirm root `dune` keeps no-network protocol evidence in
  `@check-protocol`.

- [ ] Confirm `@check-protocol` includes:

  - `@protocol-pbt`
  - `@protocol-fixtures`
  - `@fuzz-replay`
  - `@runtime-conformance`
  - `@simulator-contract`

- [ ] Confirm `@check-protocol` does not include:

  - `@minio-contract`
  - Eio MinIO smoke
  - Any Docker-backed alias
  - Any offline fuzzing alias

## Task 3: Maintainer Docs

- [ ] Update `docs/testing.md` to list every maintained alias and its evidence
  layer.

- [ ] State that runtime HTTP contract belongs under runtime conformance.

- [ ] State that simulator stateful PBT belongs under simulator contracts.

- [ ] State that MinIO remains Docker-backed local integration evidence.

- [ ] Update `docs/ci.md` when CI job contents change.

## Task 4: CI

- [ ] Keep `protocol-evidence` running:

  ```sh
  opam exec -- dune build @check-protocol
  ```

- [ ] Keep `s3-minio-contract` running:

  ```sh
  opam exec -- dune build --force @minio-contract
  ```

- [ ] If Eio MinIO smoke is promoted, run it in an OCaml 5 job with Eio
  packages installed.

- [ ] Preserve failure logs for Docker-backed jobs.

- [ ] Preserve `docker compose down -v` in an always-run cleanup step.

## Task 5: Release Script

- [ ] Inspect `scripts/release-check.sh`.

- [ ] Keep no-network protocol evidence as:

  ```sh
  opam exec -- dune build @check-protocol
  ```

- [ ] Keep MinIO evidence as:

  ```sh
  opam exec -- dune build --force @minio-contract
  ```

- [ ] Add Eio MinIO smoke to release validation only if Pass 4 promoted it as
  release-ready and the script installs the needed Eio packages.

## Task 6: Validation

- [ ] Run no-network evidence:

  ```sh
  opam exec -- dune build @check-protocol
  ```

- [ ] Run MinIO evidence when Docker is available:

  ```sh
  docker compose up -d
  opam exec -- dune build --force @minio-contract
  docker compose down -v
  ```

- [ ] Run whitespace validation:

  ```sh
  git diff --check
  ```

- [ ] If wired tests fail because of product behavior, write failure evidence
  and do not edit production code.

## Backlog

- Separate scheduled MinIO canary for image bumps.
- Release-only extended transfer and multipart contract jobs.
- Offline fuzzing discovery workflow outside default CI.
