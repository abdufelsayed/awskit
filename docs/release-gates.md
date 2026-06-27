# Release Gates

This document defines the evidence required before a release can be described as
production-ready for the scope in `SUPPORT.md`.

Related maintainer docs:

- `docs/release.md` is the step-by-step release playbook.
- `docs/ci.md` explains the `Required CI` package matrix.
- `docs/testing.md` explains the local evidence included in release gates.
- `docs/security-threat-model.md` names security-sensitive contracts that need
  review before support claims expand.

## Gate Checklist

Record each item in the release PR before merge:

| Gate | Evidence to record |
| --- | --- |
| Release branch identity | Release branch head SHA used for validation. |
| Required CI | `gh pr checks <pr-number> --watch=false` result showing `Required CI` green. |
| Local release gate | Transcript of the direct local release commands below from the release branch. |
| Package install/build/test | Required CI package matrix result, or local fresh-switch evidence showing each released package passed `opam install --with-test --deps-only <package>` plus `dune build -p <package> @install @runtest`. |
| Public API review | Status of `.mli` files, package docs, examples, and focused behavior tests. |
| Support/security scope | Status of `SUPPORT.md` and `SECURITY.md` against the release scope. |
| Live AWS scope | Statement that live AWS is outside the gate unless `SUPPORT.md` promises live AWS coverage. |

## Local Gate

Run local release checks with direct tool commands from the release branch:

```sh
opam install --yes --with-test --with-doc --with-dev-setup --deps-only .
opam exec -- dune build @opam
opam lint ./*.opam
opam exec -- dune fmt
git diff --check
git diff --exit-code
scripts/test.sh quick --label release-correctness
opam install --yes eio_main tls-eio tls ca-certs domain-name mirage-crypto-rng
opam exec -- dune build @examples @doc
scripts/test.sh integration --label release-integration
```

This validates generated opam metadata, opam lint, formatting, drift, examples,
documentation, no-service test evidence, and MinIO-backed evidence. The test
commands record durable reports under `.logs/`.

Before building the source distribution, the worktree must be clean, including
untracked files. Then run the archive checks with the release version:

```sh
version=X.Y.Z
opam exec -- dune-release check -V "$version"
opam exec -- dune-release distrib -V "$version"
dist_dir=$(mktemp -d)
tar -xjf "_build/awskit-$version.tbz" -C "$dist_dir"
(cd "$dist_dir/awskit-$version" && opam exec -- dune build @doc)
```

Review documentation build output for warnings, errors, or unresolved
references before recording the gate as passed.

## CI Gates

Release branch CI must be green before merging the release PR. The required CI
evidence is:

- `Required CI` from `.github/workflows/ci.yml`, covering package metadata,
  no-service stress, documentation/examples, local-service stress, and
  per-package install/build/test matrices on Linux and macOS.

Check PR state with:

```sh
gh pr checks <pr-number> --watch=false
```

## Public API Review

Public `.mli` files, package documentation, examples, and focused behavior
tests are the review baseline for public API changes before 1.0. They are not a
promise to preserve pilot APIs.

Intentional public breaking changes are allowed when they improve code quality,
maintainability, developer experience, ergonomics, correctness, or production
readiness. Such changes must update:

- the implementation and public `.mli` files;
- focused behavior tests for supported workflows;
- package docs and examples;
- `CHANGES.md`.

## Support And Security Scope

Before merge, confirm `SUPPORT.md` and `SECURITY.md` describe the release scope
accurately. Security-sensitive SDK material should also be checked against
`docs/security-threat-model.md`, especially diagnostics, credential handling,
endpoint policy, presigned artifacts, and documentation examples.

Live AWS is not a release gate unless `SUPPORT.md` is updated to promise live
AWS coverage. If that promise changes, add the live-service policy and evidence
before using the release as the first proof point.
