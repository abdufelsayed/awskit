# MinIO Test Double Contract Draft

Draft status: reviewed by the original MinIO explorer and revised from the
formatting draft.

## Purpose

Define MinIO as Awskit's current local S3-compatible test double for
`awskit-s3-lwt-unix`, not as the semantic source of truth for S3. Awskit owns
AWS S3 behavior. The simulator is the deterministic semantic test arena, and
protocol/runtime layers own exact wire and body-framing rules.

MinIO exists to catch integration regressions in real HTTP execution, signed
requests, response decoding, streaming, cleanup, and local adapter wiring.

## Current Gap

The suite already has `@minio-contract`, shared S3 contract tests, and a MinIO
capability profile, but the contract boundary is easy to misread:

- A passing MinIO run can look like a provider support claim unless docs and
  tests consistently call it a local test double.
- Some behavior is covered by both shared contract cases and targeted MinIO
  cases without a short ownership rule for where new scenarios belong.
- Capability differences live in
  `test/awskit-s3/lwt/unix/test_minio_contract.ml`, but maintainers need a
  durable checklist for when to add a capability, branch assertions, or move a
  scenario to the simulator.
- There is currently no Eio MinIO coverage in `test/awskit-s3/eio/dune`.

## What MinIO Proves

MinIO should prove that Awskit's public S3 client can execute covered workflows
through a real local HTTP/storage service.

Concrete proof points:

- `Awskit_s3_lwt_unix.create` can connect with explicit credentials, region,
  endpoint config, path-style addressing, and local plaintext policy.
- Signed requests are accepted for covered workflows.
- XML bodies and response decoders interoperate for covered operations.
- Request bodies, response readers, bounded buffering, streaming reads, and
  cleanup behavior survive real Cohttp/Lwt Unix execution through public S3
  operations.
- Core object workflows work end to end: put/get/head/delete, metadata, tags,
  range reads, copy, listing, versioning, delete markers, multipart upload, and
  local file transfer.
- The shared contract in `test/awskit-s3/support/s3_contract.ml` can run
  against both the strict simulator profile and the MinIO profile with explicit
  capability differences.

Exact canonical request behavior belongs in protocol fixtures and PBT. Low-level
body ownership, drain, cancellation, and bodiless HTTP rules belong in runtime
conformance and runtime HTTP contract tests. MinIO can exercise symptoms through
public S3 operations, but it should not decide those lower-level contracts.

## What MinIO Must Not Decide

MinIO must not decide AWS S3 semantics, public support scope, or provider
compatibility.

It must not be used to:

- Loosen simulator assertions because MinIO behaves differently.
- Treat MinIO behavior as AWS behavior when AWS docs, protocol fixtures, or
  simulator semantics disagree.
- Add support claims for arbitrary S3-compatible providers, including Ceph/RGW.
- Decide edge semantics for `HEAD`, `204`, or `304` bodiless responses.
- Replace protocol evidence aliases such as `@protocol-fixtures`,
  `@protocol-pbt`, `@fuzz-replay`, `@runtime-conformance`, or
  `@simulator-contract`.

When MinIO differs from the strict profile, record the difference as a MinIO
capability only if that difference is encoded by the shared contract or used to
branch assertions. Avoid building a provider matrix.

## Files And Aliases

Primary files:

- `test/awskit-s3/lwt/unix/test_minio_contract.ml`: MinIO endpoint setup,
  cleanup, capability declaration, targeted MinIO cases, and
  `S3_contract.Make` instantiation.
- `test/awskit-s3/lwt/unix/dune`: `test_minio_contract` executable and
  `(alias minio-contract)`.
- `test/awskit-s3/support/s3_contract.ml`: shared S3 contract and strict
  capability profile.
- `test/awskit-s3/sim/test_simulator_contract.ml`: strict simulator subject.
- `test/awskit-s3/sim/dune`: `(alias simulator-contract)`.
- `test/awskit-s3/eio/dune`: future Eio smoke coverage would start here.
- `docker-compose.yml`: pinned local MinIO and setup images.
- `.github/workflows/main.yml`: `s3-minio-contract` job.
- `scripts/release-check.sh`: release validation, including `@minio-contract`.
- `docs/testing.md`: evidence-layer and alias rules.
- `docs/ci.md`: CI placement and debugging notes.
- `packages/awskit-s3/doc/support_matrix.mld`: release-facing support
  boundaries.
- `packages/awskit-s3/doc/guides.mld`: local MinIO usage instructions.

Relevant commands:

- `docker compose up -d`.
- `opam exec -- dune build --force @minio-contract`.
- `docker compose down -v`.
- `opam exec -- dune build @simulator-contract`.
- `opam exec -- dune build @check-protocol`.

## First Milestone

Make the MinIO contract self-describing and hard to misuse.

Implementation-sized steps for a later change:

- Add a short contract comment near `Minio_subject.capabilities` in
  `test/awskit-s3/lwt/unix/test_minio_contract.ml` saying MinIO is a local
  test double and explaining when a capability difference belongs there.
- Keep `expected_capability_differences` as the guardrail for deviations that
  the shared contract encodes or assertion branches use.
- Add or tighten one docs paragraph in `packages/awskit-s3/doc/guides.mld` and
  `docs/testing.md` saying `@minio-contract` proves local adapter
  interoperability only.
- Ensure `packages/awskit-s3/doc/support_matrix.mld` continues to say
  arbitrary S3-compatible providers, including Ceph/RGW, are not certified.
- Validate with `docker compose up -d`,
  `opam exec -- dune build --force @minio-contract`, and
  `docker compose down -v`.

## Scenario Backlog

Good MinIO candidates:

- Public S3 operation symptoms that require a real HTTP/storage service.
- Multipart upload lifecycle through real upload IDs, stale part ETags, abort,
  completion, and resumed local-file transfer.
- Range downloads and local-file download publish behavior through actual
  response streams.
- Endpoint policy and path-style routing using `AWSKIT_S3_MINIO_ENDPOINT`.
- Cleanup robustness for versioned buckets, delete markers, and bulk deletes.
- One presigned GET or PUT roundtrip as local HTTP integration evidence, while
  exact presign artifacts remain fixture-owned.
- Eio smoke coverage: start small with create bucket, put, head, get, range,
  delete, and one small transfer. Do not duplicate the full Lwt Unix contract
  until the lifecycle harness is proven.

Simulator-first candidates:

- Semantic edges and exhaustive state rules for versioning, delete markers,
  preconditions, multipart transitions, and fault injection.
- Deterministic clocks, retry classification, lost responses, and operation
  history.
- New public operation semantics before they are promoted into MinIO adapter
  evidence.

Protocol fixture candidates:

- Exact canonical requests, presigned artifacts, XML encode/decode bodies,
  error XML, pagination payloads, and normalized wire summaries.

Out of scope:

- Certifying arbitrary S3-compatible providers.
- Provider-specific MinIO extensions.
- Live AWS release gates unless the support policy changes.

## CI Placement And Cost Controls

Keep MinIO in its own CI job, `s3-minio-contract`, on Ubuntu only. It should
remain separate from default build/test and protocol evidence because it
requires Docker and cleanup.

Current cost controls:

- Run on non-draft pull requests, pushes to `main` and `release/**`, scheduled
  runs, and manual dispatch.
- Keep the job on latest OCaml rather than the full compiler matrix.
- Use `--force @minio-contract` because external service state is outside
  Dune's dependency graph.
- Dump Docker logs on failure and always run `docker compose down -v`.
- Keep MinIO out of `@check-fast`.

Future split:

- PR-safe: existing Lwt Unix MinIO contract while stable and quick, plus any
  small Eio smoke if path-gated or cheap enough.
- Release or scheduled: larger pagination/delimiter cases, larger multipart
  resume variants, versioning edge cases, presigned roundtrip, and MinIO image
  bump canaries.

## Risks And Open Questions

- Risk: maintainers treat a passing MinIO run as provider support. Mitigation:
  keep wording consistent in `docs/testing.md`, `support_matrix.mld`, and test
  comments.
- Risk: capability differences grow until the shared contract becomes too weak.
  Mitigation: keep strict simulator coverage as the default and name only
  differences the shared contract or assertion branches actually need.
- Risk: Docker or MinIO image changes cause unrelated CI failures. Mitigation:
  keep images pinned and preserve failure logs.
- Risk: local MinIO state leaks between tests. Mitigation: keep per-process
  bucket names, version-aware cleanup, and `docker compose down -v`.
- Open question: when should Eio MinIO smoke become PR-safe rather than
  release/scheduled-only?
- Open question: how small can a presigned MinIO roundtrip be while still
  proving useful real-stack behavior?
