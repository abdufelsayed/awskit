# Fix Failures Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix product bugs only after the new test evidence exposes and records
them.

**Architecture:** Treat each failure note as the source of truth for one fix.
Keep the red test unchanged first, make the narrow production fix in the owning
layer, then run the focused and broader evidence gates.

**Tech Stack:** OCaml, Dune aliases, Alcotest, QCheck, MinIO/Docker only for
MinIO failures.

---

## Related Drafts

Before fixing a failure, read the draft for the layer that produced it. The
drafts describe ownership boundaries and later edge cases that may affect the
fix shape:

- Runtime: `docs/testing-strategy/drafts/2026-06-25-runtime-http-contract.md`
- Protocol: `docs/testing-strategy/drafts/2026-06-25-protocol-fuzz-replay.md`
- Simulator: `docs/testing-strategy/drafts/2026-06-25-simulator-stateful-pbt.md`
- MinIO: `docs/testing-strategy/drafts/2026-06-25-minio-test-double.md`

## Entry Criteria

Start this pass only after at least one earlier pass records failure evidence
under:

```text
docs/testing-strategy/failures/
```

If there is no failure evidence, this pass has no work.

## Files

Production files are allowed in this pass, but only those required by the
failure's owning layer:

- Runtime failures: `packages/awskit/eio/**`,
  `packages/awskit/lwt/**`, or `packages/awskit/runtime.*`
- Protocol failures: `packages/awskit/**` or `packages/awskit-s3/**`
- Simulator failures: `packages/awskit-s3/sim/**`
- MinIO integration failures: the smallest owning S3 or runtime adapter package

Do not batch unrelated failure classes into one fix.

## Task 1: Triage Failure Evidence

- [ ] Read every file under `docs/testing-strategy/failures/`.

- [ ] Group failures by ownership layer:

  - Runtime adapter bug.
  - Protocol parser or canonicalization bug.
  - Simulator semantic bug.
  - MinIO local integration bug.
  - Test harness bug.

- [ ] For each failure, identify the focused command that reproduces it.

- [ ] Re-run the focused command before editing production code.

## Task 2: Fix Runtime Failures

- [ ] Keep the failing runtime contract test unchanged first.

- [ ] Edit only the owning runtime adapter implementation.

- [ ] Run the focused contract alias from the failure note.

- [ ] Run:

  ```sh
  opam exec -- dune build @runtime-conformance
  ```

- [ ] Run:

  ```sh
  opam exec -- dune build @check-protocol
  ```

## Task 3: Fix Protocol Failures

- [ ] Keep the failing property, fixture, or replay case unchanged first.

- [ ] Edit only the owning parser, validator, endpoint, signing, XML, or
  response module.

- [ ] Run the focused alias from the failure note.

- [ ] Run:

  ```sh
  opam exec -- dune build @protocol-pbt
  opam exec -- dune build @protocol-fixtures
  opam exec -- dune build @fuzz-replay
  ```

- [ ] Run:

  ```sh
  opam exec -- dune build @check-protocol
  ```

## Task 4: Fix Simulator Failures

- [ ] Keep the failing stateful property unchanged first.

- [ ] If the failure is hard to understand from the shrunk transcript, add a
  deterministic regression test in `test/awskit-s3/sim/test_simulator.ml`
  before editing simulator implementation code.

- [ ] Edit only the owning simulator module.

- [ ] Run:

  ```sh
  opam exec -- dune build @simulator-contract
  ```

- [ ] Run:

  ```sh
  opam exec -- dune build @check-protocol
  ```

## Task 5: Fix MinIO Integration Failures

- [ ] Keep the failing MinIO contract or smoke test unchanged first.

- [ ] Confirm Docker and local MinIO are healthy before treating the failure as
  a product bug.

- [ ] Edit only the owning adapter or S3 operation module.

- [ ] Run:

  ```sh
  docker compose up -d
  opam exec -- dune build --force @minio-contract
  docker compose down -v
  ```

- [ ] If Eio MinIO smoke exists and is relevant, run:

  ```sh
  docker compose up -d
  opam exec -- dune build --force @test/awskit-s3/eio/minio-smoke-eio
  docker compose down -v
  ```

## Task 6: Final Validation

- [ ] Run the broad no-network gate:

  ```sh
  opam exec -- dune build @check-protocol
  ```

- [ ] Run MinIO only when the fix touched integration-sensitive S3 or runtime
  behavior:

  ```sh
  docker compose up -d
  opam exec -- dune build --force @minio-contract
  docker compose down -v
  ```

- [ ] Run whitespace validation:

  ```sh
  git diff --check
  ```

- [ ] Update or close the matching failure evidence note by adding a short
  fixed-by section with the focused command that now passes.

## Completion Criteria

- Every failure fixed in this pass still has a test that would have failed
  before the production change.
- Each fix touches only the owning layer.
- Focused and broader gates pass for the changed layer.
- No MinIO behavior is promoted into global AWS semantics.
