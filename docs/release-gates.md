# Release Gates

This document defines the evidence required before a release can be described
as production-ready for the scope in `SUPPORT.md`.

## Local Gates

Run the complete local release validation with:

```sh
scripts/release-check.sh
```

`scripts/release-check.sh` also validates generated opam metadata, formatting,
tests, protocol evidence, examples, odoc output, install artifacts,
distribution archive documentation, the Lwt Unix MinIO contract, and the
focused Eio MinIO smoke.

## CI Gates

Release branch CI must be green before merging the release PR. The required CI
evidence is:

- default package build and tests on Linux and macOS;
- Eio package build and tests on Linux and macOS;
- docs and examples;
- protocol evidence aliases;
- Lwt Unix MinIO S3 contract and focused Eio MinIO smoke.

Check PR state with:

```sh
gh pr checks <pr-number> --watch=false
```

## Public API Review

Public `.mli` files, package documentation, examples, and focused behavior
tests are the review baseline for public API changes before 1.0. They are not
a promise to preserve pilot APIs.

Intentional public breaking changes are allowed when they improve code quality,
maintainability, DX, ergonomics, correctness, or production readiness. Such
changes must update:

- the implementation and public `.mli` files;
- focused behavior tests for supported workflows;
- package docs and examples;
- `CHANGES.md`.

## Release PR Evidence

The release PR must record:

- release branch head SHA used for validation;
- `gh pr checks` result;
- `scripts/release-check.sh` result;
- public API review status;
- support/security docs status;
- whether live AWS tests are outside the support promise.

Live AWS is not a release gate unless `SUPPORT.md` is updated to promise live
AWS coverage.
