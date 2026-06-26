# CI Workflows

Awskit uses separate GitHub Actions workflows for required CI, generated
documentation publishing, advisory stress evidence, and release validation.

## Workflow Files

`.github/workflows/ci.yml`

- Required build, test, docs/examples, correctness, and MinIO evidence.
- Runs on pull requests, pushes to `main`, pushes to `release/**`, and manual
  dispatch.
- Skips draft pull requests.
- Ends with the stable aggregate check named `Required CI`.

`.github/workflows/docs.yml`

- Publishes generated odoc documentation to GitHub Pages.
- Runs after the `CI` workflow succeeds on `main`.
- Can be dispatched manually from `main`.
- Checks out the exact commit whose CI completed before building docs.

`.github/workflows/stress.yml`

- Runs high-cost discovery evidence outside required PR CI.
- Runs weekly and by manual dispatch.
- Executes `scripts/test-report.sh stress --label stress-evidence`.
- Uploads `.logs/` as an artifact.

`.github/workflows/release-validation.yml`

- Runs `scripts/release-check.sh`.
- Runs on pushes to `release/**` and by manual dispatch.
- Uses the release branch name to infer `AWSKIT_RELEASE_VERSION`, or a manual
  `release_version` input when validating another ref.
- Uploads `.logs/` as an artifact.

## Required CI

Branch protection should require the aggregate `Required CI` check. The
aggregate depends on:

`Package metadata`

- Builds generated opam metadata with `opam exec -- dune build @opam`.
- Runs `opam lint ./*.opam`.
- Runs `opam exec -- dune fmt`, `git diff --check`, and
  `git diff --exit-code` so generated-file and formatting drift fail early.

`Default packages`

- Runs on Ubuntu and macOS.
- Tests OCaml 4.14 and the latest OCaml 5 compiler.
- Excludes Eio packages.

`Eio packages`

- Runs on Ubuntu and macOS.
- Tests OCaml 5.2 and the latest OCaml 5 compiler.
- Covers Eio packages.

`Documentation and examples`

- Runs on Ubuntu with the latest OCaml 5 compiler.
- Installs test and documentation dependencies for all packages.
- Installs Eio HTTPS example-only packages separately because they are not part
  of the published package dependency graph.
- Builds executable examples and odoc documentation with
  `opam exec -- dune build @examples @doc`.

`No-network correctness`

- Runs on Ubuntu with the latest OCaml 5 compiler.
- Installs test, documentation, and development dependencies for all packages.
- Builds no-network correctness evidence with
  `scripts/test-report.sh quick --label no-network-correctness`.
- Does not start local services; MinIO adapter evidence stays in the separate
  `S3 MinIO integration` job.
- Uploads `.logs/` as a workflow artifact even when the evidence fails.

`S3 MinIO integration`

- Runs on Ubuntu.
- Starts MinIO with Docker Compose as a local S3-compatible test double.
- Runs `scripts/test-report.sh integration --label s3-minio-integration`.
- Provides local adapter integration evidence, separate from no-network
  correctness evidence.
- Dumps MinIO logs into the saved report on failure.
- Uploads `.logs/` as a workflow artifact even when the evidence fails.

## Advisory Stress Evidence

The `Stress evidence` workflow is not a required PR gate. It is for scheduled
and manual discovery pressure:

```sh
scripts/test-report.sh stress --label stress-evidence
```

Manual dispatch accepts:

- `qcheck_count`: no-network stress alias count. Defaults to `2000`.
- `minio_qcheck_count`: optional count override for script-managed MinIO
  phases. Empty means the MinIO profile defaults apply.

Failures should be triaged like regressions, but the workflow is intentionally
separate from normal PR feedback so expensive exploration does not slow every
change.

## Release Validation

Release branch pushes run both required CI and the release validation workflow.
Before merging a release PR, record:

- the `Required CI` result;
- the `Release check` result from `.github/workflows/release-validation.yml`;
- the local `scripts/release-check.sh` result when required by
  `docs/release-gates.md`.

The release validation workflow is a CI mirror for the local release script. It
does not replace the release gate policy in `docs/release-gates.md`.

## Documentation Publishing

Generated documentation is published by `.github/workflows/docs.yml` only after
the `CI` workflow succeeds on `main`, or by manual dispatch from `main`. The
workflow builds:

```sh
opam exec -- dune build @doc
```

and deploys `_build/default/_doc/_html` to GitHub Pages.

## Debugging Failed CI

Start with the PR check summary:

```sh
gh pr checks <pr-number> --watch=false
```

Inspect failed logs:

```sh
gh run view <run-id> --log-failed
```

Watch a rerun:

```sh
gh run watch <run-id> --interval 10 --exit-status
```

For `No-network correctness`, `S3 MinIO integration`, `Stress evidence`, and
`Release check` failures, download the `.logs/` artifact and reproduce locally
with the closest command. For MinIO failures:

```sh
scripts/test-report.sh integration --label minio-debug
```

Fix the underlying contract mismatch. Do not paper over CI with looser checks,
skips, or unrelated retries unless the failure is proven to be infrastructure
only.

## Regression Fix Standard

Follow `docs/testing.md` for regression-test and validation rules.
Do not add tests that only assert removed APIs or old behavior are absent.
