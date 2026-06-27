# Tests

This tree is organized around workload-based correctness checks.

Add focused private test libraries, workload models, and package-owned runners
under this directory. Use concise names that describe behavior, such as
`runtime_http_workload`, `s3_model`, `s3_workload`, `protocol_wire`, and
`transfer_fault_workload`.

Shared test-helper contracts live under `test/support` and are part of
`@correctness` through `@awskit-test-contracts`:

- `@test/support/runtest`
- `@awskit-test-contracts`

The suite IDs are `contract:awskit-test:workload-coverage` and
`contract:awskit-test:runtime-http-loopback`.

Core Awskit contracts and recording-runtime support contracts live under
`test/awskit` and run without network access:

- `@test/awskit/awskit-core-contracts`
- `@test/awskit/runtime/awskit-runtime-contracts`
- `@awskit-core-contracts`
- `@awskit-runtime-contracts`

The suite IDs are `unit:awskit:error-redaction`,
`unit:awskit:core-contracts`, `pbt:awskit:retry`, `unit:awskit:retry`,
and `contract:awskit:runtime-core`.
The retry PBT suite covers jitter bounds plus generated retry schedules across
attempt limits, retryable and non-retryable errors, throttling classification,
timeout classification, and caller cancellation classification.

Runtime HTTP adapter work runs through package-owned aliases:

- `@test/awskit/eio/runtime-http-contracts`
- `@test/awskit/lwt/runtime-http-contracts`
- `@runtime-http-contracts`

The shared runtime HTTP workload covers generated response framing, bodiless
responses, adversarial framing conflicts, early closes, malformed header/body
wire cases, response-body reader consumption modes, and callback exception
preservation. It distinguishes errors Awskit can observe after the Cohttp
transport boundary from raw HTTP syntax that Cohttp has already collapsed into
decoded response data.

Reduced runtime HTTP replay scenarios live in the path-scoped corpus directory
`test/fixtures/runtime-http-replay` and are parsed by
`test/support/runtime_http_replay.ml`.

The suite IDs are `workload:awskit-eio:runtime-http` and
`workload:awskit-lwt:runtime-http`.
The package-owned runtime HTTP aliases set
`AWSKIT_RUNTIME_HTTP_REQUIRE_LOOPBACK=1` so a sandbox-denied local listener is a
failed evidence gate instead of a successful skip.

S3 workload support lives in `test/awskit-s3/support`. The private
`awskit_s3_workload` library provides reusable command definitions,
model-aware history synthesis, pure model state, replay text helpers, and a
target functor for package-owned runners. It also provides `transfer_model.ml`
and `transfer_fault_workload.ml` for runtime-neutral transfer byte-movement,
progress traces, cancellation, multipart ownership, and cleanup-secondary
workloads.

Pure S3 model contracts run through the support-owned alias, which is also part
of the composed `@s3-simulator` gate:

- `@test/awskit-s3/support/s3-model-contracts`

The suite ID is `contract:awskit-s3:model`.

The simulator runner lives in `test/awskit-s3/sim` and runs the shared S3 state
workload, deterministic simulator contracts, and generated transfer fault
workload without network access:

- `@test/awskit-s3/sim/simulator-state`
- `@test/awskit-s3/sim/transfer-faults`
- `@s3-simulator`
- `@s3-transfer-faults`

The suite IDs are `workload:awskit-s3-sim:s3-state` and
`workload:awskit-s3-sim:transfer-faults`, plus
`replay:awskit-s3-sim:s3-state`,
`contract:awskit-s3-sim:multipart-validation`,
`contract:awskit-s3-sim:list-pagination`, and
`contract:awskit-s3-sim:body-lifecycle`.

Reduced S3 workload replays live under
`test/awskit-s3/fixtures/workload-replay` and run through the simulator
workload alias. Each `*.txt` replay file is discovered automatically as a
deterministic command transcript whose path is the replay identifier.

S3 protocol wire workloads live in `test/awskit-s3/protocol`. The private
`awskit_s3_protocol_test_support` library keeps fixture diffing, independent
wire-model helpers, protocol generators, fixed credentials/time helpers, and
the recording runtime local to those tests.

The golden protocol fixture corpus lives in
`test/awskit-s3/fixtures/protocol` and covers inspectable request artifacts,
including endpoint style, presign, copy-source/checksum headers,
object-tagging XML, multipart XML, pagination XML, and service errors. The
deterministic replay corpus lives under
`test/awskit-s3/fixtures/protocol/fuzz-replay`.

Protocol wire properties cover canonical query ordering, duplicate and absent
query values, encoded query sort order, endpoint URL-part rejection, endpoint
path selection, percent-encoded object-key spellings, tagging XML validation
boundaries, request header validation and canonicalization, presigned URL
safety, transfer planning laws, and multipart request prevalidation. Mutation
properties exercise hostile endpoint bytes and tagging XML fragments; minimized
failures belong in the replay corpus as public error-category checks.

Protocol and domain runners use no network access. The protocol alias runs
wire-level properties, and the domain alias runs pure S3 domain validation
properties plus deterministic boundary regressions:

- `@test/awskit-s3/protocol/protocol-laws`
- `@test/awskit-s3/protocol/domain-laws`
- `@test/awskit-s3/protocol/fixtures`
- `@test/awskit-s3/protocol/replay`
- `@s3-protocol-laws`
- `@s3-domain-laws`
- `@s3-protocol-fixtures`
- `@s3-protocol-replay`

The suite IDs are `workload:awskit-s3:protocol-wire`,
`pbt:awskit-s3:domain:bucket`, `pbt:awskit-s3:domain:object`,
`pbt:awskit-s3:domain:headers`, `pbt:awskit-s3:domain:metadata-tags`,
`pbt:awskit-s3:domain:range`, `pbt:awskit-s3:domain:multipart`,
`unit:awskit-s3:domain:regression`, `fixture:awskit-s3:protocol-wire`,
and `replay:awskit-s3:protocol-wire`.

The local-service runner lives in `test/awskit-s3/lwt/unix` and runs a MinIO
profile of the same shared S3 state workload plus deterministic transfer cases
against a local MinIO service:

- `@test/awskit-s3/lwt/unix/local-service`
- `@s3-local-service`

The suite IDs are `integration:minio:profile`, `workload:minio:s3-state`,
`integration:minio:s3-state`, `integration:minio:object`,
`integration:minio:multipart`, `integration:minio:transfer`, and
`integration:minio:configuration`.

`AWSKIT_INTEGRATION_PROFILE` selects the MinIO workload cost profile. Unset or
empty means `bounded`; invalid non-empty values fail with the allowed values.

- `bounded` is the default local and CI profile.
- `expensive` explores broader values and longer generated histories while
  keeping the default count modest enough for a local MinIO service.

The generated MinIO profile honors `AWSKIT_QCHECK_COUNT=<positive-int>` as an
explicit count override. It excludes command families whose behavior is outside
this local-service profile, such as metadata-specific shared workload commands,
suspended versioning, version-list pages, self-copy, and object keys the pinned
MinIO test double rejects even though the strict shared model still exercises
them, currently `prefix//double-slash` (`XMinioInvalidObjectName`). It also
excludes MinIO histories that rely on version ids for accepted key shapes where
MinIO omits those ids, currently enabled-versioning writes to
`prefix/trailing/`, and histories that would require delete-marker version ids
after versioning is enabled. It also excludes enabling versioning after current
objects already exist, because the pinned MinIO test double does not report null
version ids for those pre-versioning objects.

Deterministic MinIO state cases keep target-profile laws visible without
weakening shared oracles. For suffix ranges over empty objects, the MinIO runner
allows only Awskit's decode rejection of MinIO's malformed `Content-Range`; the
strict model still expects `InvalidRange`, and a successful read still fails the
test. This gate is evidence for the configured local MinIO test double only; it
is not a compatibility claim for other S3-compatible providers.

The local-service alias fails when the local service is not reachable. Use
`scripts/test.sh integration` for the script-managed Docker lifecycle, or start
MinIO yourself and pass explicit `AWSKIT_S3_MINIO_*` configuration before
running `@s3-local-service` or `@integration` directly.

## Discovery And Backtesting

`@stress` is the opt-in no-network pressure gate. It recurses through
focused `discovery` aliases for runtime HTTP, S3 simulator, S3 transfer faults,
S3 protocol wire properties, and S3 protocol replay. Local MinIO stays under
`@s3-local-service` and `@integration`; use `scripts/test.sh stress` when the
pressure run should leave a durable report.

Use `AWSKIT_QCHECK_COUNT=<positive-int>` to override generated workload counts,
optionally raising them for deeper discovery. Unset or invalid values keep the
workload defaults, and deterministic examples, fixtures, and replay cases remain
ordinary fixed evidence. QCheck chooses fresh seeds by default; replay failures
with `QCHECK_SEED=<seed>` on the focused alias that reported the seed.

Backtesting uses temporary mutations to verify a workload catches a bug class.
Restore the mutation before committing. Commit workload improvements or reduced
replay cases, not the mutation itself.

Backtesting selectors should stay focused:

- Runtime bodiless response mutants should fail under
  `@test/awskit/eio/runtime-http-contracts` or `@test/awskit/lwt/runtime-http-contracts` with
  replay-ready output.
- Simulator object-tag mutants should fail under `@s3-simulator` with a
  shrunk `put-object-tags` transcript.
- Protocol XML mutants should fail under `@s3-protocol-replay` with the
  fixture path as the identifier.
- Transfer progress mutants can be checked with the deterministic
  `workload:awskit-s3-sim:transfer-faults` case that reports progress trace
  monotonicity.

The generated workload Dune rules track `AWSKIT_QCHECK_COUNT` and
`QCHECK_SEED`; changing those variables reruns the owning focused aliases
instead of relying on cached evidence. The current no-network transfer fault
target covers simulated transfer byte movement, progress, callback, and owned
multipart cleanup behavior. Local-file download publication and caller-owned
resumable upload cleanup need separate no-network target coverage before they
can be claimed by `@s3-transfer-faults`.

When generated or fuzz workloads expose a product bug, reduce the failure into
the smallest replay artifact that preserves the behavior. Use the fixture path
as the durable identifier; do not add a central manifest, central registry, or
alias layer around old replay spellings.

Promotion flow:

1. Reproduce with the focused alias and the printed seed or replay path.
2. Reduce with the QCheck shrinker or by manually trimming the fixture.
3. Add a path-scoped replay artifact owned by the failing surface.
4. Verify the replay fails before the product fix when practical.
5. Fix production behavior in a separate pass, then keep the replay as
   regression evidence.

## Saved Reports

Use `scripts/test.sh` from the repository root when a workflow should leave a
durable transcript. Reports default to `.logs/` and are intentionally outside
version control.

- `scripts/test.sh quick` runs `@correctness` tests.
- `scripts/test.sh integration` starts MinIO, runs `@integration` tests, logs
  service output on failure, and cleans the service up.
- `scripts/test.sh stress` runs `@stress` tests.

Use `--label <name>` to make a report easy to identify, and replay a failure
with the printed `QCHECK_SEED` on the focused alias that produced it.

## Semantic Coverage

Generated workloads include lightweight semantic coverage checks. These checks
do not prove product correctness by themselves; they prove that a generator is
reaching important behavior classes before the target oracle runs.

Coverage bins should describe semantic behavior, not implementation trivia:
runtime framing and consume-mode combinations, S3 model states and transitions,
transfer length boundaries and fault families, protocol generator families, and
similar evidence that a generated workload is exploring the contract space.

Use presence checks for rare but required behavior classes. Use conservative
thresholds when a generator should hit a behavior class regularly across a
fixed diagnostic sample. Keep those diagnostic sample sizes independent from
`AWSKIT_QCHECK_COUNT`; that environment variable changes property-run counts
for discovery or quick reruns and should not make coverage diagnostics brittle.

Semantic coverage is not a replacement for oracles, shrinkers, or replay
artifacts. When a coverage check names a behavior class, the workload still
needs an independent oracle that can fail if the target mishandles that class.
When discovery finds a real bug, reduce it into the replay corpus instead of
turning the coverage bin into a hard-coded case list.

If a semantic coverage check fails, fix the generator or split rare behavior
into a directed generator. Do not pin a random seed to make the check pass.
