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

List the release-facing files changed in this PR. Common release files include:

- `CHANGES.md`
- `dune-project` and generated `*.opam`
- `README.md`, `SUPPORT.md`, `SECURITY.md`, and `docs/*.md`
- package documentation under `packages/*/doc`
- examples under `examples/`
- release and CI automation such as `.github/workflows/main.yml` and `scripts/`

## Validation

- Release branch head SHA:
- CI (`gh pr checks <number> --watch=false`):
- Local (`scripts/release-check.sh`):
- Public API review:
- Support/security docs:

## Release Gates

Before merge:

- Release branch CI is green.
- `scripts/release-check.sh` passes.
- Public API diffs are reviewed and intentional.
- `SUPPORT.md` and `SECURITY.md` match the release scope.
- Live AWS is outside the release gate unless `SUPPORT.md` promises live AWS
  coverage.
- GitHub Pages source is set to GitHub Actions when documentation publishing is
  part of the release.

After merge:

- Tag `vX.Y.Z`.
- Create the GitHub release from `CHANGES.md`.
- Confirm docs deploy to `https://abdufelsayed.github.io/awskit/`.
