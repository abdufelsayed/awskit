# CI Workflow

Awskit uses `.github/workflows/main.yml` for build, test, MinIO integration,
and documentation publishing workflows.

## Events

The workflow runs on:

- `pull_request`
- pushes to `main`
- pushes to `release/**`
- manual dispatch
- weekly schedule

Build and test jobs skip draft pull requests. They run for non-PR events such
as release branch pushes, scheduled runs, and manual dispatches.

## Jobs

`build-test-default`

- Runs on Ubuntu and macOS.
- Tests OCaml 4.14 and the latest OCaml 5 compiler.
- Excludes Eio packages.

`build-test-eio`

- Runs on Ubuntu and macOS.
- Tests OCaml 5 and 5.2.
- Covers Eio packages.

`docs-examples`

- Runs on Ubuntu with the latest OCaml 5 compiler.
- Installs test and documentation dependencies for all packages.
- Installs Eio HTTPS example-only packages separately because they are not part
  of the published package dependency graph.
- Builds executable examples and odoc documentation with
  `opam exec -- dune build @examples @doc`.

`no-network-correctness`

- Runs on Ubuntu with the latest OCaml 5 compiler.
- Installs test and documentation dependencies for all packages.
- Builds no-network correctness evidence with
  `scripts/test-report.sh local --label no-network-correctness`.
- Does not start local services; MinIO adapter evidence stays in the separate
  `s3-minio-integration` job.
- Uploads `.logs/` as a workflow artifact even when the evidence fails.

`s3-minio-integration`

- Runs on Ubuntu.
- Starts MinIO with Docker Compose as a local S3-compatible test double.
- Runs `scripts/test-report.sh integration --label s3-minio-integration`.
- Provides local adapter integration evidence, separate from no-network
  correctness evidence.
- Dumps MinIO logs into the saved report on failure.
- Uploads `.logs/` as a workflow artifact even when the evidence fails.

`publish-docs`

- Runs only on pushes to `main`.
- Requires build/test, docs/examples, and MinIO jobs to pass.
- Also requires the no-network correctness job to pass.
- Builds odoc documentation and deploys `_build/default/_doc/_html` to GitHub
  Pages.

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

Reproduce locally with the closest command. For MinIO failures:

```sh
scripts/test-report.sh integration --label minio-debug
```

Fix the underlying contract mismatch. Do not paper over CI with looser checks,
skips, or unrelated retries unless the failure is proven to be infrastructure
only.

## Regression Fix Standard

Follow `docs/testing.md` for regression-test and validation rules.
Do not add tests that only assert removed APIs or old behavior are absent.
