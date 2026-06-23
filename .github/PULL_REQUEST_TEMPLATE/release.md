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

- Release branch head SHA:
- CI (`gh pr checks <number> --watch=false`):
- Local (`scripts/release-check.sh`):
- API snapshot review:
- Support/security docs:

## Release Gates

Before merge:

- Release branch CI is green.
- `scripts/release-check.sh` passes.
- API snapshot diffs are reviewed and intentional.
- `SUPPORT.md` and `SECURITY.md` match the release scope.
- Branch protection or an enabled ruleset is verified with
  `scripts/check-github-ruleset.sh main` or equivalent `gh api` commands.
- Live AWS is outside the release gate unless `SUPPORT.md` promises live AWS
  coverage.
- GitHub Pages source is set to GitHub Actions when documentation publishing is
  part of the release.

After merge:

- Tag `vX.Y.Z`.
- Create the GitHub release from `CHANGES.md`.
- Confirm docs deploy to `https://abdufelsayed.github.io/awskit/`.
