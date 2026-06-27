# CI Workflows

Awskit uses GitHub Actions for required CI, generated documentation publishing,
and advisory stress evidence.

Related maintainer docs:

- `docs/testing.md` explains the local evidence behind each CI job.
- `docs/release-gates.md` defines release evidence required before merge.
- `docs/docs-publishing.md` covers the GitHub Pages workflow.

## Workflow Map

| Workflow | File | Purpose | Runs on |
| --- | --- | --- | --- |
| `CI` | `.github/workflows/ci.yml` | Required package metadata, per-package opam install/test, docs/examples, no-network correctness, and MinIO evidence. Ends with the stable aggregate check named `Required CI`. | Pull requests, pushes to `main`, pushes to `release/**`, pushes to `v*` tags, and manual dispatch. Draft pull requests are skipped. |
| `Publish docs` | `.github/workflows/docs.yml` | Publishes generated odoc documentation to GitHub Pages. | Successful `CI` workflow runs on `main`, and manual dispatch from `main`. |
| `Stress evidence` | `.github/workflows/stress.yml` | Runs high-cost discovery evidence outside required PR CI. | Weekly schedule and manual dispatch. |

## Required CI

Branch protection should require the aggregate `Required CI` check. It depends
on these jobs:

| Job | Evidence |
| --- | --- |
| `Package metadata` | Runs `scripts/check.sh package-metadata` for generated opam metadata, opam lint, formatting, whitespace, and generated-file drift. |
| `Default package install/test matrix` | Runs `scripts/check.sh package --package <package>` for each non-Eio opam package on Ubuntu and macOS with OCaml 4.14 and the latest OCaml 5 compiler. |
| `Eio package install/test matrix` | Runs `scripts/check.sh package --package <package>` for each Eio-compatible opam package on Ubuntu and macOS with OCaml 5.2 and the latest OCaml 5 compiler. |
| `Documentation and examples` | Runs `scripts/check.sh docs-examples` for example and odoc builds. |
| `No-network correctness` | Runs `scripts/check.sh no-network --label no-network-correctness` and uploads `.logs/` as an artifact with `if: always()`. |
| `S3 MinIO integration` | Runs `scripts/check.sh minio --label s3-minio-integration` against script-managed local MinIO and uploads `.logs/` as an artifact with `if: always()`. |

`No-network correctness` does not start local services. MinIO adapter evidence
stays in `S3 MinIO integration`.

## Advisory Stress Evidence

The `Stress evidence` workflow is not a required PR gate. It is scheduled and
manually dispatched discovery pressure:

```sh
scripts/check.sh stress --label stress-evidence
```

Manual dispatch accepts:

- `qcheck_count`: no-network stress alias count. Defaults to `2000`.
- `minio_qcheck_count`: optional count override for script-managed MinIO
  phases. Empty means the MinIO profile defaults apply.

Treat stress failures as regressions to triage. Keep the workflow separate from
normal PR feedback so expensive exploration does not slow every change.

## Package And Release Checks

The required package matrices are Awskit's opam-ci-shaped pre-publication gate.
Each matrix leaf installs one package's test dependencies and builds only that
package's install and test targets:

```sh
scripts/check.sh package --package <package>
```

This catches package-specific `with-test` dependency gaps in normal PR CI
instead of waiting for the opam-repository PR.

Source distribution validation is not a GitHub CI job. Run it locally when
creating a release:

```sh
scripts/check.sh source-distribution
```

That command runs `dune-release check`, builds the source distribution archive,
extracts it, and builds documentation from the extracted tree.

The complete local release gate remains:

```sh
scripts/check.sh release
```

That local gate includes package metadata, temporary-switch package isolation,
docs/examples, source distribution generation, no-network evidence, and MinIO
evidence. The official opam publication flow still happens after the release
tag through `opam publish` or the equivalent dune-release opam submission flow.

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

For `No-network correctness`, `S3 MinIO integration`, and `Stress evidence`
failures, download the `.logs/` artifact when available and reproduce locally
with the closest command. For MinIO failures:

```sh
scripts/check.sh minio --label minio-debug
```

Fix the underlying contract mismatch. Use skips, looser checks, or retries only
after the failure is proven to be infrastructure-only.

## Regression Fix Standard

Follow `docs/testing.md` for regression-test and validation rules. Regression
tests should protect current supported behavior rather than tombstoning removed
APIs.
