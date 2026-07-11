## Summary

<!-- Name the release version and main release scope. -->

## Release Notes Preview

<!-- Use CHANGES.md entries with material commit references and any useful
earlier contributing PR references. Never cite this release PR in its own
preview. Remove empty sections. -->

### Breaking

<!-- Add breaking changes with material commits and any earlier contributing PRs. -->

### Added

<!-- Add additions with material commits and any earlier contributing PRs. -->

### Fixed

<!-- Add fixes with material commits and any earlier contributing PRs. -->

### Documentation, CI, and Release

<!-- Add documentation, CI, and release changes with material commits and any
earlier contributing PRs. -->

## Release Files

<!-- Name changed release-facing files, public APIs, docs, support/security
surfaces, and publication targets. -->

## Validation

<!-- Fill in completed release evidence with links, SHAs, and command results. -->

- Branch head:
- Required CI:
- Source distribution:
- Local release check:
- Opam preflight:
- Public API review:
- Support/security docs:
- Live AWS scope:
- Documentation publishing:

## Publication

<!-- List post-merge publication steps: tag, GitHub release, v-prefixed
dune-release distribution archive upload, checksum verification,
opam-repository PR, opam-ci status, and generated docs publication.

If opam-ci fails while the opam-repository PR is still open and unpublished,
merge the Awskit fix, wait for main branch CI, retag and rebuild the same
version, then rerun the opam publish flow so the existing opam-repository PR is
updated. Do not hand-edit generated opam-repository package files while leaving
Awskit metadata out of date. After opam-repository accepts a package version,
later corrections require a new patch release. -->

- Final archive URL and checksum plan:
- opam-repository PR and opam-ci:
