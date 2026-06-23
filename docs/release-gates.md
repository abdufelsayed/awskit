# Release Gates

This document defines the evidence required before a release can be described
as production-ready for the scope in `SUPPORT.md`.

## Local Gates

Run the local governance gate before release tagging:

```sh
scripts/check-release-governance.sh
```

This verifies:

- root support and security policy files;
- API snapshot and public API tier metadata;
- compile-only API checks;
- docs content policy for support, security, Eio, S3-compatible, and presign
  claims.

Run the complete local release validation with:

```sh
scripts/release-check.sh
```

`scripts/release-check.sh` also validates generated opam metadata, formatting,
tests, protocol evidence, examples, odoc output, install artifacts,
distribution archive documentation, and the MinIO contract.

## CI Gates

Release branch CI must be green before merging the release PR. The required CI
evidence is:

- default package build and tests on Linux and macOS;
- Eio package build and tests on Linux and macOS;
- docs, examples, and docs content policy;
- release governance and API snapshot checks;
- protocol evidence aliases;
- MinIO S3 contract.

Check PR state with:

```sh
gh pr checks <pr-number> --watch=false
```

## API Snapshot Review

`@api-snapshot` records the installed public `.cmi` surface. It is a review
baseline, not a promise to preserve pilot APIs before 1.0.

Intentional public breaking changes are allowed when they improve code quality,
maintainability, DX, ergonomics, correctness, or production readiness. Such
changes must update:

- the implementation and public `.mli` files;
- compile-only API tests;
- `api-snapshot/current-installed-cmis.txt` when the installed surface changes;
- `api-snapshot/public-api-tiers.sexp` when a role classification changes;
- package docs and examples;
- `CHANGES.md`.

Refresh the snapshot only after reviewing the diff:

```sh
AWSKIT_UPDATE_API_SNAPSHOT=1 scripts/api-snapshot.sh
```

## GitHub Branch And Ruleset Gate

GitHub branch protection or an enabled repository ruleset is a release gate, but
it is not part of ordinary local developer checks because it depends on GitHub
credentials and repository settings.

Verify it during release review:

```sh
scripts/check-github-ruleset.sh main
```

Equivalent manual commands are:

```sh
gh api repos/abdufelsayed/awskit/branches/main/protection
gh api repos/abdufelsayed/awskit/rulesets
```

At least one must prove that `main` is protected by branch protection or an
enabled ruleset before a production-ready release is merged.

## Release PR Evidence

The release PR must record:

- release branch head SHA used for validation;
- `gh pr checks` result;
- `scripts/release-check.sh` result;
- API snapshot review status;
- support/security docs status;
- branch protection or ruleset verification;
- whether live AWS tests are outside the support promise.

Live AWS is not a release gate unless `SUPPORT.md` is updated to promise live
AWS coverage.
