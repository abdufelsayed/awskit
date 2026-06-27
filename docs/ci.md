# CI Workflows

Awskit uses separate GitHub Actions workflows for required CI, generated
documentation publishing, advisory stress evidence, and release validation.

Related maintainer docs:

- `docs/testing.md` explains the local evidence behind each CI job.
- `docs/release-gates.md` defines release evidence required before merge.
- `docs/docs-publishing.md` covers the GitHub Pages workflow.

## Workflow Map

| Workflow | File | Purpose | Runs on |
| --- | --- | --- | --- |
| `CI` | `.github/workflows/ci.yml` | Required build, test, docs/examples, no-network correctness, and MinIO evidence. Ends with the stable aggregate check named `Required CI`. | Pull requests, pushes to `main`, pushes to `release/**`, and manual dispatch. Draft pull requests are skipped. |
| `Publish docs` | `.github/workflows/docs.yml` | Publishes generated odoc documentation to GitHub Pages. | Successful `CI` workflow runs on `main`, and manual dispatch from `main`. |
| `Stress evidence` | `.github/workflows/stress.yml` | Runs high-cost discovery evidence outside required PR CI. | Weekly schedule and manual dispatch. |
| `Release validation` | `.github/workflows/release-validation.yml` | Mirrors the local release script in CI. | Pushes to `release/**` and manual dispatch. |

## Required CI

Branch protection should require the aggregate `Required CI` check. It depends
on these jobs:

| Job | Evidence |
| --- | --- |
| `Package metadata` | Builds generated opam metadata with `opam exec -- dune build @opam`, runs `opam lint ./*.opam`, and checks formatting/generated-file drift with `opam exec -- dune fmt`, `git diff --check`, and `git diff --exit-code`. |
| `Default packages` | Runs on Ubuntu and macOS for OCaml 4.14 and the latest OCaml 5 compiler. Covers the default package set and excludes Eio packages. |
| `Eio packages` | Runs on Ubuntu and macOS for OCaml 5.2 and the latest OCaml 5 compiler. Covers Eio packages. |
| `Documentation and examples` | Installs test and documentation dependencies for all packages, installs Eio HTTPS example-only packages separately, and builds `opam exec -- dune build @examples @doc`. |
| `No-network correctness` | Runs `scripts/test-report.sh quick --label no-network-correctness` and uploads `.logs/` as an artifact with `if: always()`. |
| `S3 MinIO integration` | Runs `scripts/test-report.sh integration --label s3-minio-integration` against script-managed local MinIO and uploads `.logs/` as an artifact with `if: always()`. |

`No-network correctness` does not start local services. MinIO adapter evidence
stays in `S3 MinIO integration`.

## Advisory Stress Evidence

The `Stress evidence` workflow is not a required PR gate. It is scheduled and
manually dispatched discovery pressure:

```sh
scripts/test-report.sh stress --label stress-evidence
```

Manual dispatch accepts:

- `qcheck_count`: no-network stress alias count. Defaults to `2000`.
- `minio_qcheck_count`: optional count override for script-managed MinIO
  phases. Empty means the MinIO profile defaults apply.

Treat stress failures as regressions to triage. Keep the workflow separate from
normal PR feedback so expensive exploration does not slow every change.

## Release Validation

Release branch pushes run both required CI and the release validation workflow.
Before merging a release PR, record:

- the `Required CI` result;
- the `Release check` result from `.github/workflows/release-validation.yml`;
- the local `scripts/release-check.sh` result when required by
  `docs/release-gates.md`.

The release validation workflow uses the release branch name to infer
`AWSKIT_RELEASE_VERSION`, or a manual `release_version` input when validating
another ref. It uploads `.logs/` as an artifact.

The workflow is a CI mirror for the local release script. It does not replace
the release gate policy in `docs/release-gates.md`.

## Documentation Publishing

Generated documentation is published by `.github/workflows/docs.yml` after the
`CI` workflow succeeds on `main`, or by manual dispatch from `main`. The
workflow builds:

```sh
opam exec -- dune build @doc
```

and deploys `_build/default/_doc/_html` to GitHub Pages. See
`docs/docs-publishing.md` for repository settings, URL policy, and local
validation.

## Debug Failed CI

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

Fix the underlying contract mismatch. Use skips, looser checks, or retries only
after the failure is proven to be infrastructure-only.

## Regression Fix Standard

Follow `docs/testing.md` for regression-test and validation rules. Regression
tests should protect current supported behavior rather than tombstoning removed
APIs.
