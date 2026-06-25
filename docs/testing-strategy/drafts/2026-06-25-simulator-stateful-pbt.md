# S3 Simulator Stateful PBT Draft

Draft status: reviewed by the original simulator explorer and revised from the
formatting draft.

## Purpose

Add a no-network stateful property-testing layer for `awskit-s3-sim` that uses
the simulator as Awskit's deterministic S3 semantic test arena. The oracle for
the tests is a small independent model of AWS-intended behavior, compared
against public simulator observations.

MinIO remains the local S3-compatible contract double for `@minio-contract`;
it should not become the oracle for simulator behavior. Arbitrary
S3-compatible providers, including Ceph/RGW, stay out of scope.

## Current Gap

Existing simulator coverage is strong but mostly scenario-based:

- `test/awskit-s3/sim/test_simulator.ml` covers focused simulator behavior.
- `test/awskit-s3/sim/test_simulator_contract.ml` runs the shared
  `S3_contract` strict profile.
- Existing QCheck coverage spans domain validation, paginator behavior,
  transfer planning, signing, and protocol normalization, but not simulator
  stateful operation sequences.

What is missing is generated sequences like put, delete, list, copy, find, and
head across a small key space, with assertions after each step that public API
results and public simulator inspection still match an independent model.

## Proposed Test Layer

Create a simulator-only stateful PBT suite under `test/awskit-s3/sim`. It
should call the public `Awskit_s3_sim` API plus public inspection helpers such
as `Simulator.keys`, `Simulator.objects_as_strings`, `Simulator.object_metadata`,
and `Simulator.history`.

Use the repo's existing QCheck style:

- Fixed seed, such as `0xA5111`.
- `Awskit_test.Qcheck.to_alcotest`.
- Alcotest suite ID `pbt:awskit-s3-sim:simulator-stateful`.
- Compact printers for generated scenarios.
- Explicit list and command-field shrinkers.
- Small, deterministic generated domains.

This layer should run under `@simulator-contract`, with a focused alias only if
it becomes a maintained public check documented in `docs/testing.md`.

## Model And Invariants

The first model should be intentionally small: one pre-created bucket plus
current object bodies. Starting with one bucket avoids duplicate-create and
missing-bucket history ambiguity in the first milestone.

Model state:

- A single bucket name.
- Objects: `string -> string`.
- No versioning, multipart, tags, metadata, checksums, or faults in the first
  milestone.

Initial command set:

- `Put_string (key, body)`.
- `Get_string key`.
- `Find_string key`.
- `Head_object key`.
- `Exists_object key`.
- `Delete_object key`.
- `List_keys`.
- `Copy_object (source_key, destination_key)`.

Core invariants after each command:

- `Simulator.objects_as_strings store ~bucket` equals the model's current
  objects in lexicographic key order.
- `Simulator.keys store ~bucket` equals the model's current key set in
  lexicographic order.
- `Get_string` returns the modeled body for present objects and `NoSuchKey`
  for absent objects.
- `Find_string` maps only missing objects in the existing bucket to `Ok None`.
- `Head_object` and `Exists_object` agree with the model without reading object
  bodies.
- `Delete_object` removes present objects and succeeds for absent objects in
  the existing bucket.
- Validation or preflight failures do not mutate simulator state.

Use normalized error comparisons: `Error.is_no_such_bucket`,
`Error.is_no_such_key`, service code/status, validation field, or error kind.
Avoid full human error strings except in focused diagnostics tests.

Separate assertion classes:

- AWS-intended semantics: object presence, get/find/head/list/delete behavior.
- Simulator harness behavior: operation history shape and inspection helpers.
- Known simulator limitations: anything deliberately not modeled by the
  simulator.

History assertions must be precise. Simulator history does not record all
public calls. Bucket create/delete/list and missing-bucket paths may not
record object operations. Present-object `find` records `Get_object`; `exists`
records `Head_object` only when the request reaches that operation boundary.

Also note that `Simulator.keys` and `Simulator.objects_as_strings` can return
`[]` for a missing bucket, so they cannot alone distinguish an empty bucket
from a missing bucket. When bucket existence matters, assert through public
bucket APIs.

## Files And Aliases

Likely implementation files:

- Create `test/awskit-s3/sim/test_simulator_stateful_pbt.ml`.
- Modify `test/awskit-s3/sim/test_awskit_s3_sim.ml` to include
  `Test_simulator_stateful_pbt.suite`.
- Modify `test/awskit-s3/sim/dune` to add test libraries `awskit_test`,
  `qcheck-core`, and `qcheck-alcotest`.
- Optionally add a focused Dune alias in `test/awskit-s3/sim/dune` for
  `@simulator-stateful-pbt`.
- Modify `dune-project` to add `qcheck-core` and `qcheck-alcotest` as
  `:with-test` dependencies for `awskit-s3-sim`.
- Regenerate `awskit-s3-sim.opam` through
  `opam exec -- dune build @opam` if package metadata changes.

Likely aliases:

- `opam exec -- dune build @simulator-contract`.
- `opam exec -- dune build @check-protocol`.
- Optional: `opam exec -- dune build @simulator-stateful-pbt`.

## First Milestone

Land a narrow object-lifecycle stateful property:

- 100-200 generated scenarios.
- 1-40 commands per scenario.
- One pre-created valid bucket.
- Keys from a small valid set, including prefix-like keys such as
  `logs/a.txt`.
- Bodies are short printable strings.
- Assertions run after every command.

The first milestone should not touch production simulator code unless the
property finds a real bug. If it does, add a deterministic regression test in
`test/awskit-s3/sim/test_simulator.ml` before fixing the implementation.

## Scenario Backlog

Extend in this order after the first milestone is stable:

- Prefix and paginated listing: `Object.List.options_exn ~prefix ~max_keys`,
  paginator helpers, duplicate/omission checks.
- `Copy_object` and `Delete_objects`, including absent source and mixed delete
  results.
- Metadata, content type, and tags with normalized assertions.
- Read/write/delete preconditions using generated matching and non-matching
  ETags.
- Versioning enabled/suspended, delete markers, explicit version reads/deletes,
  and `List_object_versions`.
- Multipart create/upload/list/complete/abort with small modeled parts and
  edge ordering.
- Deterministic fault injection using `inject_faults`, with explicit
  invariants for FIFO consumption, `faulted` history, and mutation/no-mutation
  behavior.
- Selected bucket configuration roundtrips where the model is simple and AWS
  semantics are clear.

Treat checksums and storage class as later, careful additions. They are useful
but can become brittle if modeled before the supported contract is narrow.

## Replay And Shrinking

Provide a list shrinker and command-field shrinkers. The scenario printer
should emit a compact replayable transcript: command name, bucket, key, body,
and options.

When a property finds a bug:

- Keep the shrunk transcript in the failure output.
- Add a focused deterministic regression test for the minimized case.
- Fix simulator behavior after the regression exists.
- Consider adding replay fixtures later under
  `test/awskit-s3/sim/fixtures/stateful-pbt/` only if failures become hard to
  preserve as ordinary Alcotest cases.

Fault scheduling should use explicit generated schedules by eligible operation
index, not raw command index. Invalid or missing-bucket operations may not
consume queued faults, which can shift the scenario if scheduling is tied to
raw command position.

Keep `enable_random_faults` out of shrinkable stateful PBT. Use it only for a
small deterministic smoke test. Treat `Response_lost` as a body-read failure
for valid `Get` or `Find` of existing objects, not as a general operation
fault.

## Risks And Open Questions

- Adding QCheck to `awskit-s3-sim` test dependencies affects generated opam
  metadata; confirm this is acceptable for the release branch.
- Public inspection helpers may be enough for object lifecycle, but versioning
  and multipart may need richer public inspection or carefully chosen API-only
  assertions.
- Generated scenarios can become noisy if the model allows too many invalid
  operations. Keep missing-bucket and missing-key cases deliberate and well
  printed.
- Do not weaken AWS S3 semantics to match MinIO quirks. MinIO differences
  belong in its capability profile, not in the simulator model.
- Keep CI runtime bounded; raise scenario counts only after the focused suite
  is fast and stable.
