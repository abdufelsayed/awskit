Prepare `vX.Y.Z`.

This PR is the release vehicle. `CHANGES.md` is the canonical changelog; the
release notes preview includes curated entries with material PR and commit
references.

## Release Notes Preview

### Breaking

<!-- Add breaking changes with all material PR and commit references. -->

### Added

<!-- Add additions with all material PR and commit references. -->

### Fixed

<!-- Add fixes with all material PR and commit references. -->

### Documentation, CI, and Release

<!-- Add documentation, CI, and release changes with all material PR and commit references. -->

## Release Files

- `CHANGES.md`
- `dune-project`
- `*.opam`
- `.github/workflows/main.yml`

## Validation

- CI:
- Local:
- Release check:

## Release Gates

Before merge:

- Release branch CI is green.
- GitHub Pages source is set to GitHub Actions when documentation publishing is
  part of the release.

After merge:

- Tag `vX.Y.Z`.
- Create the GitHub release from `CHANGES.md`.
- Confirm docs deploy to `https://abdufelsayed.github.io/awskit/`.
