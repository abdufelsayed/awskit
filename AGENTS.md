# Agent Instructions

These instructions apply to the whole repository. Read this file before making
changes, writing descriptions, updating release notes, or opening pull requests.

## Start Here

1. Read `docs/agent-workflow.md`.
2. Check the working tree before editing:

   ```sh
   git status --short --branch
   ```

3. Read the maintenance document that matches the change:

   - Codebase architecture and package boundaries:
     `docs/architecture.md`
   - OCaml implementation style: `docs/ocaml-development.md`
   - Tests and validation: `docs/testing.md`
   - Changelog entries: `docs/changelog.md`
   - Release work: `docs/release.md`
   - CI behavior and debugging: `docs/ci.md`
   - Documentation publishing: `docs/docs-publishing.md`

## Source Of Truth

- Public release notes live in `CHANGES.md`.
- Release PRs preview the changelog, but `CHANGES.md` is the canonical release
  ledger.
- Public package documentation lives in `packages/*/doc`.
- Maintainer workflows live in `docs`.
- CI and documentation publishing live in `.github/workflows/main.yml`.
- Release validation lives in `scripts/release-check.sh`.

## Required Checks

Use the narrowest check that proves the change, then run broader checks when the
change affects shared behavior, packaging, CI, or releases. See
`docs/testing.md` for check-selection rules.
