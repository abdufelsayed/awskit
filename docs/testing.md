# Testing And Validation

This document defines how Awskit changes should be tested and validated.

## Evidence Layers

Use the narrowest evidence layer that proves the behavior, then broaden when a
change affects public APIs, wire formats, runtime behavior, package metadata,
docs, or releases.

| Evidence layer | Use for | Narrow check |
| --- | --- | --- |
| Deterministic examples | Named regressions, common workflows, and resource/lifecycle stories whose expected behavior is clearest as a short scenario. | `opam exec -- dune runtest <dir>` |
| Unit tests | Pure validation, option builders, error classification, request construction, and focused parser failures. | package or directory `runtest` |
| Property tests | Parsers, formatters, validators, endpoint policy, canonical query/header normalization, pagination, retry jitter bounds, and transfer planning. Explore fresh generated cases by default and replay failures with `QCHECK_SEED`. | `opam exec -- dune build @protocol-pbt` |
| Golden fixtures | Exact protocol artifacts that reviewers should inspect: presigned artifacts, endpoint resolution, XML decode/encode bodies, pagination, multipart XML, service errors, and normalized wire summaries. | `opam exec -- dune build @protocol-fixtures` |
| Fuzz replay | Minimized failures found by manual or mutation fuzzing. Commit the reduced input and replay it as an ordinary deterministic test before treating the bug as fixed. | `opam exec -- dune build @fuzz-replay` |
| Simulator contracts | No-network S3 behavior, model-oracle state, fault injection, and docs/test backend behavior. The simulator is not an AWS wire authority. | `opam exec -- dune build @simulator-contract` |
| Runtime conformance | Runtime authoring laws: request/response body ownership, retry sleep/random/timeout capability, scoped readers, drains, and error precedence. | `opam exec -- dune build @runtime-conformance` |
| MinIO contracts | Local adapter interoperability through a real S3-compatible test double. Requires Docker and cleanup, and remains outside no-network protocol gates. | `opam exec -- dune build --force @minio-contract` |
| Examples/docs | Extracted examples, odoc pages, and future MDX/docs checks. Examples should compile, and simulator-backed examples should execute when practical. | `opam exec -- dune build @examples @doc` |
| Release gates | The composed local evidence plus opam/install/archive/docs checks and external-service lifecycle. | `scripts/release-check.sh` |

For protocol behavior, prefer structured assertions or fixture comparisons over
one-off string containment. String containment is acceptable only when the
contract is truly the presence of a fragment, such as a human diagnostic
mentioning a field name.

## Evidence Aliases

| Alias | Purpose | External service |
| --- | --- | --- |
| `@check-fast` | Local `runtest` and examples. | No |
| `@check-protocol` | Protocol PBT, protocol fixtures, fuzz replay, simulator contract, and runtime conformance. | No |
| `@protocol-pbt` | Fast deterministic protocol property tests. | No |
| `@protocol-fixtures` | Fixture-backed protocol artifact tests with normalized comparisons. | No |
| `@fuzz-replay` | Deterministic replay of committed minimized parser/validator failures. | No |
| `@simulator-contract` | Simulator contract and simulator-specific fault/lifecycle tests. | No |
| `@runtime-conformance` | Runtime capability and lifecycle conformance checks. | No |
| `@minio-contract` | MinIO-backed S3-compatible contract tests. | Local Docker |
| `@examples` | Build example executables. | No |

Long-running mutation fuzzing, live AWS account tests, and broader provider
compatibility tests are opt-in unless a support policy explicitly promotes them
to release gates. `@minio-contract` is the named local MinIO test-double gate
for adapter integration; it is not part of `@check-fast` or
`@check-protocol`, and a passing run is not a claim about arbitrary
S3-compatible providers.

`@docs-mdx` does not exist yet. Do not add a placeholder alias. Add it only
when README or package-guide snippets are normalized into real MDX or extracted
checks that compile meaningful code.

Shared S3 contract suites should name backend capability differences explicitly
instead of weakening assertions globally. For example, the simulator can run the
strict profile while MinIO uses a documented S3-compatible profile for APIs it
actually supports.

## Test Identity

Keep test identifiers scoped and stable so maintainers and agents can select
the same behavior without guessing local naming conventions.

Test identities have three layers:

- Dune aliases are command-facing evidence IDs. Keep them kebab-case, such as
  `@protocol-pbt`, `@runtime-conformance`, or
  `@test/awskit/eio/runtime-http-contract`.
- Alcotest executable names identify the runnable package or evidence binary.
  Keep them close to the package or evidence name, such as `awskit-s3` or
  `awskit-s3-protocol-pbt`.
- Alcotest suite IDs are selector-facing test IDs. New or modified suite IDs
  must use colon scopes:

  ```text
  <evidence>:<subject>:<area>[:<detail>]
  ```

Use lowercase ASCII and kebab-case inside each segment. Supported evidence
segments are `unit`, `integration`, `contract`, `pbt`, `fixture`, and `replay`.
The subject should be the package, runtime adapter, or backend under test, such
as `awskit`, `awskit-eio`, `awskit-s3`, `awskit-s3-sim`, or `minio`.

Examples:

- `pbt:awskit:signing:canonical-query`
- `pbt:awskit-s3:domain:bucket`
- `contract:awskit-eio:runtime-http`
- `contract:awskit-s3-sim:bucket`
- `contract:minio:multipart`
- `fixture:awskit-s3:protocol`
- `replay:awskit-s3:fuzz`

QCheck property names can remain short human-readable sentences because they
are reported inside a scoped PBT suite. Fixture corpus paths use filesystem
scoping under `test/*/fixtures/**`; do not duplicate the full suite ID in every
fixture filename.

Property tests should not hard-code `Random.State` seeds in ordinary Dune
aliases. Let `QCheck_alcotest` choose and print a fresh `qcheck random seed` on
each run so repeated CI and local runs explore different examples. To reproduce
a failure, re-run the same alias with the printed seed:

```sh
QCHECK_SEED=<seed> opam exec -- dune build @protocol-pbt
```

When touching an older unscoped suite ID, normalize it to the scoped form and
update any Dune rule that selects it. Do not use a Dune alias name as an
Alcotest suite ID unless it already follows the colon-scoped suite format. See
`test/README.md` for the local registry and examples.

## Test Current Behavior

Tests should protect the current supported contract. When removing old
behavior, do not add tests that only prove the old symbol, option, or module is
gone.

Use these replacements instead:

- Test the new API that replaces the old one.
- Test the migration-sensitive behavior users rely on now.
- Test the regression symptom that motivated the change.
- Test public boundaries through exposed modules.
- Let `dune build` prove removed symbols are no longer available.

Good:

```ocaml
let test_object_get_uses_scoped_reader () =
  (* Verify the supported reader workflow. *)
  ()
```

Bad:

```ocaml
let test_removed_legacy_object_api_no_longer_exists () =
  (* Tombstone tests for removed symbols do not belong in the suite. *)
  ()
```

## Regression Fixes

For real bugs:

1. Reproduce the failure.
2. Add a focused regression test for the supported behavior.
3. Verify the test fails before the fix when practical.
4. Implement the narrow fix.
5. Run the focused test and the failing CI-equivalent command.
6. Update `CHANGES.md` if the fix belongs in the release notes.

For resource fixes, test both the normal path and the callback or IO failure
path when the failure mode is observable.

## OCaml Test Shape

Tests should cover behavior through the public surface whenever practical.
Implementation-level tests are acceptable when the behavior cannot be reached
through a public API without making the public API worse.

- Put the bulk of tests under `test/`, not inside production libraries.
- Keep tests deterministic and fast unless they are explicitly integration or
  contract tests.
- Use Alcotest assertions that show useful values on failure.
- Use property tests for parsers, formatters, validation boundaries, and
  round-trips when examples alone would miss important combinations.
- Use examples or trace-style tests for workflows whose behavior is easier to
  understand from a short execution story.

## Check Selection

Use the narrowest check that proves the change, then run broader checks when
the change affects shared behavior, packaging, CI, or releases.

Common commands:

```sh
opam exec -- dune fmt
opam exec -- dune build
opam exec -- dune test
opam exec -- dune build @check-fast
opam exec -- dune build @check-protocol
opam exec -- dune build @examples @doc
opam exec -- dune build @opam
git diff --check
```

For S3 contract work:

```sh
docker compose up -d
opam exec -- dune build --force @minio-contract
docker compose down -v
```

For runtime contract work:

```sh
opam exec -- dune build @runtime-conformance
```

For releases:

```sh
scripts/release-check.sh
```

If a broad command is too expensive for the current change, run the focused
command that proves the touched behavior and clearly state what was not run.
