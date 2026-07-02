# S3 0.3 Release Readiness

Status: done_with_concerns
Issue: none
Goal: Prepare the 0.3 release evidence, changelog, package metadata, and support/security signoff after the breaking API cleanup lands.
Risk: high
Verification: Follow docs/release-gates.md local gate and record Required CI in the release PR.

## Context

Depends on: plans 01 through 06.

This plan is for the release vehicle, not for preserving compatibility. Version
identity comes from the release branch/tag and `dune-release --pkg-version`; do
not invent a source version bump unless the repo adds one.

## Files Likely To Change

- `/Users/abdllahdev/dev/awskit/CHANGES.md`
- `/Users/abdllahdev/dev/awskit/dune-project`
- `/Users/abdllahdev/dev/awskit/*.opam`
- `/Users/abdllahdev/dev/awskit/SUPPORT.md`
- `/Users/abdllahdev/dev/awskit/SECURITY.md`
- `/Users/abdllahdev/dev/awskit/docs/security-threat-model.md`
- `/Users/abdllahdev/dev/awskit/.github/PULL_REQUEST_TEMPLATE/release.md`

## Acceptance Criteria

- `CHANGES.md` has a `0.3.0` section with a `Breaking` entry that names caller
  action:
  - pass strings/ints for request inputs,
  - use option builders,
  - use endpoint config string constructors,
  - use presigned builders,
  - use `Object.Delete_objects`.
- Every changelog entry has PR/commit traceability once refs are known.
- Package metadata changes are made in `dune-project` first, then generated
  opam files are regenerated through `dune build @opam`.
- S3 package-family validation covers `awskit-s3`, `awskit-s3-sim`,
  `awskit-s3-lwt`, `awskit-s3-lwt-unix`, and `awskit-s3-eio`, plus
  same-version core/runtime dependencies through CI or local matrix evidence.
- If Required CI is used for package-family isolation, record the exact
  successful matrix/check names. If CI is unavailable, run a local per-package
  fallback with `opam install --with-test --deps-only <package>` and
  `dune build -p <package> @install @runtest` for the S3 package family.
- Release gate evidence from `docs/release-gates.md` is run or explicitly
  recorded as deferred with a reason.
- `SUPPORT.md`, `SECURITY.md`, and `docs/security-threat-model.md` still match
  the release scope, especially endpoint policy, diagnostics, and presigned
  bearer URL docs.
- Live AWS remains outside the release gate unless `SUPPORT.md` changes.

## Tasks

- [x] Inspect release timeline after implementation commits exist:
  ```bash
  git log --reverse --oneline main..HEAD
  ```
- [x] Add the `0.3.0` `CHANGES.md` section following `docs/changelog.md`.
- [x] Update `dune-project` only if release-facing metadata changes.
- [x] If `dune-project` changes, regenerate generated opam files:
  ```bash
  opam exec -- dune build @opam
  ```
- [x] Run opam lint after any opam regeneration. No opam regeneration occurred;
  release-branch `opam lint ./*.opam` remains deferred.
- [x] Confirm support/security scope docs still match actual claims.
- [x] Record public API review status from plans 01 through 06.
- [ ] Run the local release gate from `docs/release-gates.md` on the release
  branch.
- [ ] Record Required CI status in the release PR, including S3 package-family
  matrix/check names; if unavailable, record the local per-package fallback.
- [ ] Run clean-tree `dune-release check/distrib` and archive doc build before
  declaring the release production-ready.

## Exact Verification Commands

Implementation branch checks before release branch:

```bash
dune build @fmt
dune build
dune runtest
dune build @examples @doc
dune build @opam
git diff --check
```

Release branch local gate:

```bash
opam install --yes --with-test --with-doc --with-dev-setup --deps-only .
opam exec -- dune build @opam
opam lint ./*.opam
opam exec -- dune fmt
git diff --check
git diff --exit-code
scripts/test.sh quick --label release-correctness
opam install --yes eio_main tls-eio tls ca-certs domain-name mirage-crypto-rng
opam exec -- dune build @examples @doc
scripts/test.sh integration --label release-integration
```

Package-family fallback if Required CI evidence is unavailable:

```bash
for pkg in awskit-s3 awskit-s3-sim awskit-s3-lwt awskit-s3-lwt-unix awskit-s3-eio; do
  opam install --yes --with-test --deps-only "$pkg"
  opam exec -- dune build -p "$pkg" @install @runtest
done
```

Clean release branch archive checks:

```bash
version=0.3.0
opam exec -- dune-release check -V "$version"
opam exec -- dune-release distrib -V "$version"
dist_dir=$(mktemp -d)
tar -xjf "_build/awskit-$version.tbz" -C "$dist_dir"
(cd "$dist_dir/awskit-$version" && opam exec -- dune build @doc)
```

## Rollback Notes

- If release gates fail because implementation evidence is incomplete, return
  to the relevant earlier plan. Do not weaken the release gate.
- Do not hand-edit generated opam files without changing `dune-project`.

## Completion Notes

Release timeline used for changelog traceability:

```text
6b65539 feat(s3)!: Clean public request API boundaries
bdf4762 feat(s3)!: Add presigned option builders
9e78792 test(s3): Cover public validation seams
04d3541 docs(s3): Teach string-facing public API
```

Changed `CHANGES.md` with a `0.3.0` section. The `Breaking` entry names the
caller actions required by this release: pass strings/ints for request inputs,
use operation option builders, use endpoint config string constructors, use
presigned builders, and use `Object.Delete_objects`. No PR number was available
on this local branch, so entries use commit hashes.

No release-facing package metadata change was identified. `dune-project` and
generated `.opam` files were left unchanged. `dune build @opam` passed as a
drift check. `opam lint ./*.opam` was not run because no opam files were
regenerated; it remains part of the release-branch local gate.

Public API review status from plans 01 through 06:

- Plans 01 through 04 updated public S3 `.mli` surfaces, examples, adapters,
  endpoint config, operation options, presigned APIs, and multi-delete
  boundaries in `6b65539` and `bdf4762`.
- Plan 05 added focused public validation seam coverage in `9e78792`.
- Plan 06 updated README and package guide/odoc examples for the string-facing
  public API in `04d3541`.
- This plan did not edit implementation, package docs, examples, or tests.

Support/security scope review:

- `SUPPORT.md` still scopes MinIO as the named local S3-compatible integration
  target and keeps live AWS release-gate coverage unsupported/optional.
- `SECURITY.md` still documents public diagnostic redaction, unsafe diagnostic
  access, presigned bearer URL handling through `reveal_url`, safe presigned
  accessors, and explicit endpoint policy requirements.
- `docs/security-threat-model.md` still matches the release scope for endpoint
  policy, diagnostics, presigned bearer artifacts, documentation examples, and
  live AWS being outside the gate.
- No support/security document edits were needed.

Implementation-branch checks:

```text
git log --reverse --oneline main..HEAD  PASS
dune build @fmt                         PASS
dune build                              PASS
dune runtest                            PASS
dune build @examples @doc               PASS
dune build @opam                        PASS
git diff --check                        PASS
```

Deferred release gates:

- Required CI and S3 package-family matrix: deferred because no release PR
  number exists in this local implementation branch. Remaining command:
  `gh pr checks <pr-number> --watch=false`.
- Package-family fresh-switch fallback: deferred to the release branch if
  Required CI is unavailable there. Remaining loop is the package-family
  fallback command listed above.
- Release-branch local gate: deferred because the current branch is not a clean
  `release/v0.3.0` branch and the worktree intentionally contains release
  readiness edits. Remaining command block is the release branch local gate
  listed above.
- Clean release branch drift check and archive gate: deferred because
  `git diff --exit-code`, `dune-release check`, `dune-release distrib`, and
  extracted archive docs require a clean release branch after review.
- Optional/live AWS integration was not run and is not claimed.
