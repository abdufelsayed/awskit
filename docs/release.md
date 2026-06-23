# Release Playbook

This is the release workflow for Awskit.

## Release Branch

Create release branches from `main`:

```sh
git checkout main
git pull --ff-only
git checkout -b release/vX.Y.Z
```

The release version is inferred by `scripts/release-env.sh` from a branch named
`release/vX.Y.Z` or from an exact `vX.Y.Z` tag. Set `AWSKIT_RELEASE_VERSION`
only when running release checks outside that shape.

## Build The Release Ledger

Inspect the release timeline:

```sh
git log --reverse --oneline main..HEAD
```

Update `CHANGES.md` with the entries for the release. Follow
`docs/changelog.md`:

- Include PR numbers when available.
- Include commit hashes on every entry.
- Include every PR and commit that materially contributed to a release entry.
- Use the timeline as source material, not as one public entry per commit.
- Order entries by release-branch timeline inside each section.
- Describe shipped changes, not process commentary.

## Package Metadata

When release-facing package metadata changes, update `dune-project` first and
then regenerate opam files:

```sh
opam exec -- dune build @opam
```

Commit generated `*.opam` changes with the source metadata change.

## Release PR

Open the PR from `release/vX.Y.Z` to `main` using the release template:

```sh
gh pr create \
  --base main \
  --head release/vX.Y.Z \
  --title "chore(release): Prepare vX.Y.Z" \
  --body-file .github/PULL_REQUEST_TEMPLATE/release.md
```

Then replace the template placeholders with the actual release notes preview.
The preview should use the same curated entries as `CHANGES.md`: one bullet per
meaningful released item, with all material PR and commit references attached
to that bullet.

The release PR body must include:

- A short statement that this is the release vehicle.
- A release notes preview with PR and commit references visible in the bullets.
  Do not expand the preview into one bullet per commit.
- Release files touched.
- CI and local validation status.
- Release gates before merge.
- Publication steps after merge.

The PR body previews `CHANGES.md`; it does not replace it.

## CI Requirements

Release branch pushes must run CI. Before merging, confirm:

```sh
gh pr checks <pr-number> --watch=false
```

Required release-branch checks:

- Default package build and tests.
- Eio package build and tests.
- Documentation and examples.
- Protocol evidence.
- MinIO S3 contract.

The `publish-docs` job is expected to skip on release branches because it only
runs on pushes to `main`.

## Local Release Validation

Before tagging, run:

```sh
scripts/release-check.sh
```

This validates package metadata, formatting, tests, examples, protocol
evidence, generated documentation, distribution archives, and the MinIO
contract.

## Release Gates

Follow `docs/release-gates.md` before merging a production-ready release PR.
In particular:

- record the release branch head SHA used for validation;
- run `scripts/release-check.sh`;
- confirm public API diffs were reviewed;
- confirm `SUPPORT.md` and `SECURITY.md` match the release scope;
- state that live AWS is not a release gate unless `SUPPORT.md` promises live
  AWS coverage.

## Merge, Tag, And Publish

After the release PR is approved and green:

```sh
git checkout main
git pull --ff-only
git tag -a vX.Y.Z -m "vX.Y.Z"
git push origin vX.Y.Z
```

Create the GitHub release from the `CHANGES.md` section for the version.

After merge to `main`, confirm the documentation publishing job completes and
the generated docs are available at:

```text
https://abdufelsayed.github.io/awskit/
```

## Release PR Writing Rules

Release PR descriptions must not say:

```md
We discussed that the changelog should mention PRs.
```

They should say:

```md
The release notes preview mirrors `CHANGES.md` with curated entries and all
material PR and commit references.
```

Release PRs can describe release mechanics because reviewers need that context.
`CHANGES.md` entries should describe only the shipped changes.
