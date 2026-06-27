# Release Gates

This document defines the evidence required before a release can be described as
production-ready for the scope in `SUPPORT.md`.

Related maintainer docs:

- `docs/release.md` is the step-by-step release playbook.
- `docs/ci.md` explains the `Required CI` package matrix and source
  distribution job.
- `docs/testing.md` explains the local evidence included in release gates.
- `docs/security-threat-model.md` names security-sensitive contracts that need
  review before support claims expand.

## Gate Checklist

Record each item in the release PR before merge:

| Gate | Evidence to record |
| --- | --- |
| Release branch identity | Release branch head SHA used for validation. |
| Required CI | `gh pr checks <pr-number> --watch=false` result showing `Required CI` green. |
| Source distribution CI | `Source distribution` job result from `.github/workflows/ci.yml`. |
| Local release gate | `scripts/check.sh release` result from the release branch. |
| Package isolation | `scripts/check.sh package-isolation` result showing each released package passed isolated `opam install --with-test --deps-only <package>` and `dune build -p <package> @install @runtest`. |
| Public API review | Status of `.mli` files, package docs, examples, and focused behavior tests. |
| Support/security scope | Status of `SUPPORT.md` and `SECURITY.md` against the release scope. |
| Live AWS scope | Statement that live AWS is outside the gate unless `SUPPORT.md` promises live AWS coverage. |

## Local Gate

Run the complete local release gate with:

```sh
scripts/check.sh release
```

The script validates generated opam metadata, package-isolated opam
install/test metadata, formatting, tests, no-network correctness evidence,
examples, odoc output, install artifacts, distribution archive documentation,
and MinIO integration evidence. It requires a clean worktree before building
the source distribution.

## CI Gates

Release branch CI must be green before merging the release PR. The required CI
evidence is:

- `Required CI` from `.github/workflows/ci.yml`, covering package metadata,
  per-package opam install/test matrices on Linux and macOS, docs/examples,
  no-network correctness evidence, MinIO S3 integration evidence, and release
  source distribution validation;
- `Source distribution` from `.github/workflows/ci.yml`, covering source
  distribution generation and extracted documentation checks.

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
