# Release Playbook

This is the release workflow for Awskit. A release is ready to merge when the
release PR, release gates, changelog, package metadata, and publication plan all
describe the same version and evidence.

Related maintainer docs:

- `docs/changelog.md` defines `CHANGES.md` entry format and selection rules.
- `docs/release-gates.md` defines production-ready release evidence.
- `docs/ci.md` maps release branch CI jobs.
- `docs/docs-publishing.md` covers generated documentation publication.

Official opam publication references:

- `https://opam.ocaml.org/doc/Packaging.html`
- `https://github.com/ocaml-opam/opam-publish`

## Release Flow

1. Create `release/vX.Y.Z` from `main`.
2. Build the `CHANGES.md` release ledger from the branch timeline.
3. Update release-facing metadata and regenerate opam files when needed.
4. Open a release PR with the release template.
5. Record CI and local release-gate evidence.
6. Merge, tag from updated `main`, create the GitHub release, upload the
   distribution archive, submit opam packages, and confirm docs publication.

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
- Describe shipped changes.

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

Replace template placeholders with concrete, reviewable content. The release
notes preview should use the same curated entries as `CHANGES.md`: one bullet
per meaningful released item, with all material PR and commit references
attached to that bullet.

The release PR body must include:

- a short statement that this is the release vehicle;
- a release notes preview with PR and commit references visible in the bullets;
- release files touched;
- CI and local validation status;
- release gates before merge;
- publication steps after merge.

The PR body previews `CHANGES.md`; it does not replace it. Release PRs may
describe release mechanics because reviewers need that context. Public
`CHANGES.md` entries should describe shipped changes only.

## Validate Before Merge

Release branch pushes must run CI. Check PR state with:

```sh
gh pr checks <pr-number> --watch=false
```

Required release-branch checks:

- `Required CI` from `.github/workflows/ci.yml`, which aggregates package
  metadata, per-package opam install/test matrices,
  documentation/examples, no-network correctness evidence, and MinIO S3
  integration evidence.

The `Publish docs` workflow is expected to skip on release branches because it
only runs after `CI` succeeds on `main`.

Before marking the release PR ready to merge, run:

```sh
scripts/check.sh release
```

This validates package metadata, package-isolated opam installs and tests,
formatting, tests, examples, no-network correctness evidence, generated
documentation, distribution archives, and MinIO integration evidence. The
script requires a clean worktree before building the distribution archive.

Follow `docs/release-gates.md` before merging a production-ready release PR.
Record:

- the release branch head SHA used for validation;
- the `gh pr checks` result;
- the local `scripts/check.sh release` result;
- public API review status;
- support/security docs status;
- whether live AWS tests are outside the support promise.

## Opam Packaging Before Merge

Use the package matrices and local release gate as Awskit's pre-merge opam
packaging checks. The official opam publication flow starts from a tagged
release, then opens an opam-repository PR through `opam publish`.
Pre-merge validation is local
evidence that the tagged release should publish cleanly.

Before merging the release PR, `scripts/check.sh release` must catch the
opam-ci-style packaging failures that are practical to detect locally:

- generated opam metadata from `dune-project`;
- package-isolated `opam install --with-test --deps-only <package>`;
- package-isolated `dune build -p <package> @install @runtest`;
- source distribution creation and documentation build from the extracted
  archive.

## Merge, Tag, And Publish

After the release PR is approved, green, and merged, tag the release from the
updated `main` branch:

```sh
git checkout main
git pull --ff-only
git log -1 --oneline
git tag -a vX.Y.Z -m "vX.Y.Z"
git push origin vX.Y.Z
```

Confirm the last commit is the merged release PR, or the expected squash commit,
before creating the tag.

Create the GitHub release from the `CHANGES.md` section for the version.

Build the release distribution archive with the same `v`-prefixed name that
the GitHub release asset and opam-repository metadata will use:

```sh
dune-release distrib --tag vX.Y.Z --keep-v --pkg-version X.Y.Z
```

Upload the generated archive to the GitHub release:

```sh
gh release upload vX.Y.Z _build/awskit-vX.Y.Z.tbz
```

Verify that the uploaded asset is reachable and that its checksums match the
local archive:

```sh
shasum -a 256 _build/awskit-vX.Y.Z.tbz
shasum -a 512 _build/awskit-vX.Y.Z.tbz
curl -L --fail \
  https://github.com/abdufelsayed/awskit/releases/download/vX.Y.Z/awskit-vX.Y.Z.tbz \
  -o /tmp/awskit-vX.Y.Z.tbz
shasum -a 256 /tmp/awskit-vX.Y.Z.tbz
shasum -a 512 /tmp/awskit-vX.Y.Z.tbz
```

Generate and submit opam-repository package metadata against the uploaded
archive URL:

```sh
dune-release opam pkg \
  --tag vX.Y.Z \
  --keep-v \
  --pkg-version X.Y.Z \
  --dist-uri https://github.com/abdufelsayed/awskit/releases/download/vX.Y.Z/awskit-vX.Y.Z.tbz

dune-release opam submit --tag vX.Y.Z --keep-v --pkg-version X.Y.Z
```

The equivalent `opam publish` shape is `opam publish` from the source
directory for a normal GitHub-hosted release, or `opam publish URL .` when the
archive URL is explicit and the opam files should be read from the current
directory.

After opening the opam-repository PR, monitor opam-ci.

## Update An Open opam-repository PR

If opam-ci fails, classify the failure before changing anything:

- For metadata-only failures, such as missing `with-test` dependencies,
  constraints, source URLs, or checksums, fix Awskit source metadata first.
  Update `dune-project`, regenerate `*.opam` with `opam exec -- dune build
  @opam`, validate locally, merge the Awskit fix, and rerun the opam publish
  flow for the same version. `opam publish` handles initial publications, new
  releases, and metadata updates through the same submission flow.
- For failures that require changing released source code, tests, or archive
  contents, do not mutate the existing tag or uploaded archive. Prepare a new
  Awskit patch release; opam-publish warns that changing an already published
  package archive breaks reproducibility.

Do not hand-edit generated package metadata only in the opam-repository PR
while leaving `dune-project` or Awskit `*.opam` out of date. Downstream
metadata must stay derived from the source release metadata.

After merge to `main`, confirm the documentation publishing job completes and
the generated docs are available at:

```text
https://abdufelsayed.github.io/awskit/
```
