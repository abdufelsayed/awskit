# CI Workflow

Awskit uses `.github/workflows/main.yml` for build, test, MinIO contract, and
documentation publishing workflows.

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

`s3-minio-contract`

- Runs on Ubuntu.
- Starts MinIO with Docker Compose.
- Runs `opam exec -- dune build --force @minio-contract`.
- Dumps MinIO logs on failure.

`publish-docs`

- Runs only on pushes to `main`.
- Requires build/test and MinIO jobs to pass.
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
docker compose up -d
opam exec -- dune build --force @minio-contract
docker compose down -v
```

Fix the underlying contract mismatch. Do not paper over CI with looser checks,
skips, or unrelated retries unless the failure is proven to be infrastructure
only.

## Regression Fix Standard

Follow `docs/maintenance/testing.md` for regression-test and validation rules.
Do not add tests that only assert removed APIs or old behavior are absent.
