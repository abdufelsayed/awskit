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
| `Release validation` | `.github/workflows/release-validation.yml` | Mirrors `scripts/check.sh release` in CI, including package-isolated opam install/test validation. | Pushes to `main`, pushes to `release/**`, and manual dispatch. |

## Required CI

Branch protection should require the aggregate `Required CI` check. It depends
on these jobs:

| Job | Evidence |
| --- | --- |
| `Package metadata` | Runs `scripts/check.sh package-metadata` for generated opam metadata, opam lint, formatting, whitespace, and generated-file drift. |
| `Default packages` | Runs `scripts/check.sh packages-default` on Ubuntu and macOS for OCaml 4.14 and the latest OCaml 5 compiler. Covers the default package set and excludes Eio packages. |
| `Eio packages` | Runs `scripts/check.sh packages-eio` on Ubuntu and macOS for OCaml 5.2 and the latest OCaml 5 compiler. Covers Eio packages. |
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

## Release Validation

Pushes to `main` and release branches run both required CI and the release
validation workflow. Before merging a release PR, record:

- the `Required CI` result;
- the `Release check` result from `.github/workflows/release-validation.yml`;
- the local `scripts/check.sh release` result when required by
  `docs/release-gates.md`.

The release validation workflow uses the release branch name, an exact release
tag, or `dune-project` to infer `AWSKIT_RELEASE_VERSION`; manual dispatch may
also pass a `release_version` input. It uploads `.logs/` as an artifact.

The `package-isolation` check creates a temporary opam switch, pins the
released packages from the checkout, and resets the switch between packages
before running isolated `opam install --with-test --deps-only <package>` plus
`dune build -p <package> @install @runtest`. By default the temporary switch
uses `ocaml-base-compiler` for the active compiler version; set
`AWSKIT_OPAM_ISOLATION_COMPILER_PACKAGE` to validate with another compiler
package. This catches package-specific test dependencies that grouped CI jobs
can mask.

Release validation is Awskit's pre-merge opam packaging check. The official
opam publication flow still happens after the release tag, through
`opam publish` or the equivalent dune-release opam submission flow.

The workflow is a CI mirror for the local release check. It does not replace
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
scripts/check.sh minio --label minio-debug
```

Fix the underlying contract mismatch. Use skips, looser checks, or retries only
after the failure is proven to be infrastructure-only.

## Regression Fix Standard

Follow `docs/testing.md` for regression-test and validation rules. Regression
tests should protect current supported behavior rather than tombstoning removed
APIs.
