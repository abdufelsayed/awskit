# Tests

This tree is organized around workload-based correctness checks.

Add focused private test libraries, workload models, and package-owned runners
under this directory. Use concise names that describe behavior, such as
`runtime_http_workload`, `s3_model`, `s3_workload`, `protocol_wire`, and
`transfer_fault_workload`.

Core Awskit contracts and recording-runtime support contracts live under
`test/awskit` and run without network access:

- `@test/awskit/awskit-core-contracts`
- `@test/awskit/runtime/awskit-runtime-contracts`
- `@awskit-core-contracts`
- `@awskit-runtime-contracts`

The suite IDs are `unit:awskit:error-redaction`,
`unit:awskit:core-contracts`, `pbt:awskit:retry`, `unit:awskit:retry`,
and `contract:awskit:runtime-core`.

Runtime HTTP adapter work runs through package-owned aliases:

- `@test/awskit/eio/runtime-http-workload`
- `@test/awskit/lwt/runtime-http-workload`
- `@runtime-http-workload`

The shared runtime HTTP workload covers generated response framing, bodiless
responses, adversarial framing conflicts, early closes, malformed header/body
wire cases, response-body reader consumption modes, and callback exception
preservation. It distinguishes errors Awskit can observe after the Cohttp
transport boundary from raw HTTP syntax that Cohttp has already collapsed into
decoded response data.

Reduced runtime HTTP replay scenarios live in
`test/support/runtime_http_replay.ml`. The path-scoped corpus directory
`test/fixtures/runtime-http-replay` is reserved for opaque replay bytes when a
case needs fixture data outside OCaml.

The suite IDs are `workload:awskit-eio:runtime-http` and
`workload:awskit-lwt:runtime-http`.

S3 workload support lives in `test/awskit-s3/support`. The private
`awskit_s3_workload` library provides reusable command definitions,
model-aware history synthesis, pure model state, replay text helpers, and a
target functor for package-owned runners. It also provides `transfer_model.ml`
and `transfer_fault_workload.ml` for runtime-neutral transfer byte-movement,
progress callback, cancellation, multipart ownership, and cleanup-secondary
workloads.

Pure S3 model contracts run through the support-owned alias:

- `@test/awskit-s3/support/s3-model-contracts`

The suite ID is `contract:awskit-s3:model`.

The simulator runner lives in `test/awskit-s3/sim` and runs the shared S3 state
workload, deterministic simulator contracts, and generated transfer fault
workload without network access:

- `@test/awskit-s3/sim/s3-sim-workload`
- `@test/awskit-s3/sim/s3-transfer-faults`
- `@s3-sim-workload`
- `@s3-transfer-faults`

The suite IDs are `workload:awskit-s3-sim:s3-state` and
`workload:awskit-s3-sim:transfer-faults`, plus
`replay:awskit-s3-sim:s3-state`,
`contract:awskit-s3-sim:multipart-validation`,
`contract:awskit-s3-sim:list-pagination`, and
`contract:awskit-s3-sim:body-lifecycle`.

Reduced S3 workload replays live under
`test/awskit-s3/fixtures/workload-replay` and run through the simulator
workload alias. Each replay file is a deterministic command transcript whose
path is the replay identifier.

S3 protocol wire workloads live in `test/awskit-s3/protocol`. The private
`awskit_s3_protocol_test_support` library keeps fixture diffing, independent
wire-model helpers, protocol generators, fixed credentials/time helpers, and
the recording runtime local to those tests.

The golden protocol fixture corpus lives in
`test/awskit-s3/fixtures/protocol`. The deterministic replay corpus lives under
`test/awskit-s3/fixtures/protocol/fuzz-replay`.

Protocol wire properties cover canonical query ordering, endpoint URL-part
rejection, endpoint path selection, percent-encoded object-key spellings,
tagging XML validation boundaries, request header validation, presigned URL
safety, transfer planning laws, and multipart request prevalidation. Mutation
properties exercise hostile endpoint bytes and tagging XML fragments; minimized
failures belong in the replay corpus as public error-category checks.

Protocol wire runners use no network access. The `s3-protocol-wire` alias also
runs pure S3 domain validation properties and deterministic boundary
regressions:

- `@test/awskit-s3/protocol/s3-protocol-wire`
- `@test/awskit-s3/protocol/s3-protocol-fixtures`
- `@test/awskit-s3/protocol/s3-protocol-replay`
- `@s3-protocol-wire`
- `@s3-protocol-fixtures`
- `@s3-protocol-replay`

The suite IDs are `workload:awskit-s3:protocol-wire`,
`pbt:awskit-s3:domain:bucket`, `pbt:awskit-s3:domain:object`,
`pbt:awskit-s3:domain:headers`, `pbt:awskit-s3:domain:metadata-tags`,
`pbt:awskit-s3:domain:range`, `pbt:awskit-s3:domain:multipart`,
`unit:awskit-s3:domain:regression`, `fixture:awskit-s3:protocol-wire`,
and `replay:awskit-s3:protocol-wire`.

The MinIO runner lives in `test/awskit-s3/lwt/unix` and runs a bounded
generated profile of the same shared S3 state workload plus deterministic
transfer cases against a local MinIO service:

- `@test/awskit-s3/lwt/unix/s3-minio-workload`
- `@s3-minio-workload`

The suite IDs are `integration:minio:s3-state`, `integration:minio:object`,
`integration:minio:multipart`, `integration:minio:transfer`, and
`integration:minio:configuration`.

The generated MinIO profile defaults to 25 cases and honors
`AWSKIT_QCHECK_COUNT=<positive-int>`. It excludes command families whose
behavior is outside this local-service profile, such as metadata-specific
shared workload commands, suspended versioning, version-list pages, and
self-copy. It also excludes enabling versioning after current objects already
exist, because the pinned MinIO test double does not report null version ids for
those pre-versioning objects. This gate is evidence for the configured local
MinIO test double only; it is not a compatibility claim for other S3-compatible
providers.

## Discovery And Backtesting

`@check-discovery` is the opt-in no-network discovery gate. It recurses through
focused `discovery` aliases for runtime HTTP, S3 simulator, S3 transfer faults,
S3 protocol wire properties, and S3 protocol replay. Local MinIO stays
under `@s3-minio-workload` and `@check-integration`.

Use `AWSKIT_QCHECK_COUNT=<positive-int>` to override generated workload counts,
optionally raising them for deeper discovery. Unset or invalid values keep the
workload defaults, and deterministic examples, fixtures, and replay cases remain
ordinary fixed evidence. QCheck chooses fresh seeds by default; replay failures
with `QCHECK_SEED=<seed>` on the focused alias that reported the seed.

Backtesting uses temporary mutations to verify a workload catches a bug class.
Restore the mutation before committing. Commit workload improvements or reduced
replay cases, not the mutation itself.

When generated or fuzz workloads expose a product bug, reduce the failure into
the smallest replay artifact that preserves the behavior. Use the fixture path
as the durable identifier; do not add a central manifest, central registry, or
alias layer around old replay spellings.

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
