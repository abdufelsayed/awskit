# Abandoned v0.3 Release Line

The first Awskit v0.3 release line was abandoned on 2026-07-12 after public
API review. Pull requests #25 and #27 were closed, and the release branch must
not be merged, tagged, or published as a unit. Awskit v0.2 on `main` remains
the baseline for any future release work.

## Why This Release Was Abandoned

The release did not meet its stated goal of using a breaking release to make
the public API materially clearer and easier to use.

- Several breaks moved names or removed facade aliases without improving
  safety or common call sites. For example, removing the S3 facade aliases for
  regions, credentials, and endpoints made application code longer while
  preserving the same underlying types.
- Runtime composition became cleaner for adapter implementors, but the release
  documentation initially presented that low-level change as an application
  migration even though ordinary Lwt and Eio client construction had not
  changed.
- Validation concerns leaked into deeply nested public construction APIs.
  Common operations such as `DeleteObjects` required callers to construct
  abstract validation containers even though the operation already returns a
  result and can reject invalid input before transport.
- Constructor fallibility was inconsistent with call-site ergonomics. Valid
  endpoint policies gained result-handling ceremony because one combination
  was invalid, instead of giving the common cases a direct and structurally
  valid API.
- The migration guides were not reliable release evidence. Review found
  unchanged before-and-after examples, examples that did not type-check,
  reversed version-specific argument types, and references to APIs that did
  not exist in the documented target release.
- Implementation proceeded without an approved, call-site-first public API
  specification. Internal boundary cleanup and stronger invariants were treated
  as sufficient evidence of better developer experience, but representative
  application workflows did not support that conclusion.

These failures are release-blocking. Passing implementation tests cannot make
an incoherent public API or an inaccurate migration contract safe to publish.

## What May Be Reused

No commit from this release line should be merged wholesale. Protocol and
correctness fixes, including request-size validation and multipart checksum
validation, may be reconsidered individually after confirming that they do not
carry forward the abandoned public API design.

## Requirements For A Future Restart

A future v0.3 effort should start from `main` and satisfy these conditions
before broad implementation begins:

1. Write representative call sites for client creation, object reads and
   writes, streaming, multipart upload, managed transfer, presigning, custom
   endpoints, and custom runtime integration.
2. Approve the desired public signatures from those call sites, including the
   common path and the advanced path separately.
3. Spend breaking-change compatibility only where the replacement provides a
   concrete user-visible improvement in safety, clarity, or capability.
4. Keep validation inside operations unless callers gain a clear benefit from
   constructing, storing, or reusing a validated value.
5. Compile migration examples against both exact release versions and review
   the complete installed public API diff before declaring the release ready.

## Archived State

The abandoned work is preserved under these remote branches:

- `archive/v0.3.0-release` preserves the release tip formerly used by PR #25.
- `archive/v0.3.0-pr-27` preserves the release-review fixes from PR #27.
- `archive/v0.3.0-local-release` preserves the distinct local release tip that
  had not been pushed to the remote release branch.

These branches are historical evidence, not active release or development
branches.
