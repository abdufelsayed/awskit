# CI Workflows

Awskit uses GitHub Actions for required CI, generated documentation publishing,
and manually requested deeper stress evidence.

Related maintainer docs:

- `docs/testing.md` explains the local evidence behind each CI job.
- `docs/release-gates.md` defines release evidence required before merge.
- `docs/docs-publishing.md` covers the GitHub Pages workflow.

## Workflow Map

| Workflow | File | Purpose | Runs on |
| --- | --- | --- | --- |
| `CI` | `.github/workflows/ci.yml` | Hygiene, no-service stress, docs/examples, local-service stress, and per-package install/build/test matrices. Ends with the stable aggregate check named `Required CI`. | Pull requests, pushes to `main`, pushes to `release/**`, pushes to `v*` tags, and manual dispatch. Draft pull requests are skipped. |
| `Publish docs` | `.github/workflows/docs.yml` | Publishes generated odoc documentation to GitHub Pages. | Successful `CI` workflow runs on `main`, and manual dispatch from `main`. |
| `Stress tests` | `.github/workflows/stress.yml` | Runs manually requested no-service stress tests and expensive local-service integration tests for ad hoc reruns or raised no-service counts. | Manual dispatch. |

## Required CI

Branch protection should require the aggregate `Required CI` check. It depends
on these jobs:

| Job | Evidence |
| --- | --- |
| `Hygiene` | Installs project dependencies, regenerates opam metadata, runs `opam lint`, runs `dune fmt`, and checks whitespace plus generated-file drift. |
| `No-service stress tests` | Runs `AWSKIT_QCHECK_COUNT=2000 scripts/test.sh stress --label no-service-stress`, which runs `@stress` as randomized correctness pressure and uploads `.logs/`. |
| `Documentation and example build` | Builds `@examples @doc` after installing documentation and example dependencies. |
| `S3 local-service stress tests` | Runs `AWSKIT_INTEGRATION_PROFILE=expensive scripts/test.sh integration --label s3-local-service-stress`, which runs `@integration` tests against script-managed local MinIO with the expensive profile and uploads `.logs/`. |
| `Default package install/build/test matrix` | Runs direct package `opam install --with-test --deps-only <package>` and `dune build -p <package> @install @runtest` commands for each non-Eio opam package on Ubuntu and macOS with OCaml 4.14 and the latest OCaml 5 compiler. |
| `Eio package install/build/test matrix` | Runs the same direct package commands for each Eio-compatible opam package on Ubuntu and macOS with OCaml 5.2 and the latest OCaml 5 compiler. |

This keeps failures grouped by evidence type while preserving direct
`opam`/`dune` commands in CI.

Each job configures `https://opam.ocaml.org/cache` as an opam archive mirror
before installing dependencies. Package installs should not depend on every
upstream source host being reachable during a matrix run.

## Stress Tests

Required CI includes stress-shaped no-service and local-service evidence. The
no-service job raises the generated workload count while staying inside
no-network correctness checks:

```sh
AWSKIT_QCHECK_COUNT=2000 scripts/test.sh stress --label no-service-stress
```

The local-service job uses the expensive MinIO-backed profile:

```sh
AWSKIT_INTEGRATION_PROFILE=expensive scripts/test.sh integration --label s3-local-service-stress
```

The manual `Stress tests` workflow is for deeper ad hoc pressure. Its
no-service stress count defaults to `2000` and can be raised at dispatch time.
It also runs the expensive local-service integration profile once:

```sh
AWSKIT_INTEGRATION_PROFILE=expensive scripts/test.sh integration --label local-service-expensive
```

Manual dispatch accepts:

- `qcheck_count`: stress alias count. Defaults to `2000`.

Treat stress failures as correctness regressions to triage.

## Package And Release Checks

The required package matrices are Awskit's opam-ci-shaped pre-publication gate.
Each matrix leaf installs one package's test dependencies, then builds that
package's install and test targets:

```sh
opam install --yes --with-test --deps-only <package>
opam exec -- dune build -p <package> @install @runtest
```

Documentation and example evidence runs in its own CI job:

```sh
opam install --yes eio_main tls-eio tls ca-certs domain-name mirage-crypto-rng
opam exec -- dune build @examples @doc
```

Release source-distribution checks remain local release evidence.

This catches package-specific `with-test` dependency gaps in normal PR CI
instead of waiting for the opam-repository PR.

Source distribution validation is not a GitHub CI job. Run the direct
`dune-release` commands locally when creating a release; `docs/release.md` and
`docs/release-gates.md` list the complete release evidence. The official opam
publication flow still happens after the release tag through `opam publish` or
the equivalent dune-release opam submission flow.

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

For `No-service stress tests`, `S3 local-service stress tests`, and
`Stress tests` failures, download the `.logs/` artifact when available and
reproduce locally with the closest command. For local-service stress failures:

```sh
AWSKIT_INTEGRATION_PROFILE=expensive scripts/test.sh integration --label minio-debug
```

Fix the underlying contract mismatch. Use skips, looser checks, or retries only
after the failure is proven to be infrastructure-only.

## Regression Fix Standard

Follow `docs/testing.md` for regression-test and validation rules. Regression
tests should protect current supported behavior rather than tombstoning removed
APIs.
