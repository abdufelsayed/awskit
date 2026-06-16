# Agent Instructions

These instructions apply to the whole repository. Read this file before making
changes, writing descriptions, updating release notes, or opening pull requests.

## Source of Truth

- Public release notes live in `CHANGES.md`.
- Release PRs preview the changelog, but `CHANGES.md` is the canonical release
  ledger.
- Public package documentation lives in `packages/*/doc`.
- Maintainer workflows live in `docs/maintenance`.
- OCaml development rules live in `docs/maintenance/ocaml-development.md`.
- CI and documentation publishing live in `.github/workflows/main.yml`.
- Release validation lives in `scripts/release-check.sh`.

## Required Checks

Use the narrowest check that proves the change, then run broader checks when the
change affects shared behavior, packaging, CI, or releases.

- Format: `opam exec -- dune fmt`
- Build: `opam exec -- dune build`
- Tests: `opam exec -- dune test`
- Documentation: `opam exec -- dune build @doc`
- Generated opam files: `opam exec -- dune build @opam`
- MinIO S3 contract: `docker compose up -d` then
  `opam exec -- dune build --force @minio-contract`
- Full release check: `scripts/release-check.sh`

## Test Rules

- Tests must describe and protect current supported behavior.
- When removing an API or feature, do not add tests whose only purpose is to
  prove that the removed thing no longer exists.
- Prefer tests for the replacement API, migration behavior, current public
  contract, regression symptom, or compatibility boundary.
- Let the compiler and build check prove deleted symbols are gone.
- When fixing a bug, add or update a regression test that fails for the original
  bug and passes for the correct behavior.

## OCaml Development Rules

Follow `docs/maintenance/ocaml-development.md` when changing OCaml code,
public interfaces, runtime adapters, parsers, tests, examples, or package
metadata.

- Design public `.mli` files around the call site before committing to an
  implementation shape.
- Keep invariant-bearing types opaque or private, and expose concrete types only
  when pattern matching is part of the intended API.
- Use `('a, Awskit.Error.t) result` for expected failures and `option` for
  normal absence; reserve exceptions for `_exn` bridges and exceptional
  conditions.
- Avoid catch-all variant matches where the compiler should help future
  refactors.
- Scope resource ownership clearly, and clean up response bodies, files, and
  multipart uploads on failure paths.
- Keep runtime-neutral logic out of Lwt and Eio adapters, and keep adapter
  effects out of core packages.
- Use structured parsers and typed serializers for AWS wire formats.
- Prefer public-surface tests, deterministic fixtures, and symptom-focused
  regressions.

## Writing Rules

Docs, changelogs, commit messages, PR descriptions, and release notes must stand
alone for future readers.

- Describe what changed and why it matters.
- Describe user-visible behavior, public API impact, maintainer workflow impact,
  or validation evidence.
- Do not refer to the chat, the user, an agent, the discussion, or the act of
  writing the document.
- Do not copy conversational wording into repository artifacts.
- Do not add self-referential process text such as "this changelog includes
  commit references".
- Do not describe internal reasoning when the artifact should describe the
  shipped change.

Good:

```md
- Added native streaming S3 upload and download APIs. (#5, 53a7642)
```

Bad:

```md
- Documented that changelog entries include PR numbers and commits.
- Added this because the user asked for release logs to mention PRs.
```

## Changelog Rules

- Every release entry must include a PR number when one exists and a commit hash.
- Use `(#PR, commit)` when GitHub has an associated PR.
- Use `(commit)` when there is no associated PR.
- Order entries by release-branch timeline inside each section unless a section
  has a stronger reader-facing order.
- Do not add entries about the mechanics of maintaining the changelog.
- Keep entries user-facing and concrete.

See `docs/maintenance/changelog.md` for examples and review rules.

## Release Rules

- Release branches are named `release/vX.Y.Z`.
- CI must run on release branch pushes.
- Release PR descriptions must include a traceable release notes preview with
  PR and commit references visible in the body.
- Before tagging, run `scripts/release-check.sh`.
- After merging a release PR, tag the release, create the GitHub release from
  `CHANGES.md`, and confirm documentation publishing.

See `docs/maintenance/release.md` for the full release playbook.

## PR Templates

Use the template that matches the change:

- `.github/pull_request_template.md` for ordinary changes.
- `.github/PULL_REQUEST_TEMPLATE/release.md` for release PRs.
- `.github/PULL_REQUEST_TEMPLATE/bugfix.md` for bug fixes and regressions.
- `.github/PULL_REQUEST_TEMPLATE/docs.md` for documentation-only changes.
- `.github/PULL_REQUEST_TEMPLATE/ci.md` for CI and release infrastructure.
- `.github/PULL_REQUEST_TEMPLATE/breaking-change.md` for breaking changes.
