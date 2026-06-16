# Agent Instructions

These instructions apply to the whole repository. Read this file before making
changes, writing descriptions, updating release notes, or opening pull requests.

## Start Here

1. Read `docs/maintenance/agent-workflow.md`.
2. Check the working tree before editing:

   ```sh
   git status --short --branch
   ```

3. Read the maintenance document that matches the change:

   - Codebase architecture and package boundaries:
     `docs/maintenance/architecture.md`
   - OCaml implementation style: `docs/maintenance/ocaml-development.md`
   - Tests and validation: `docs/maintenance/testing.md`
   - Changelog entries: `docs/maintenance/changelog.md`
   - Release work: `docs/maintenance/release.md`
   - CI behavior and debugging: `docs/maintenance/ci.md`
   - Documentation publishing: `docs/maintenance/docs-publishing.md`

## Source Of Truth

- Public release notes live in `CHANGES.md`.
- Release PRs preview the changelog, but `CHANGES.md` is the canonical release
  ledger.
- Public package documentation lives in `packages/*/doc`.
- Maintainer workflows live in `docs/maintenance`.
- CI and documentation publishing live in `.github/workflows/main.yml`.
- Release validation lives in `scripts/release-check.sh`.

## Required Checks

Use the narrowest check that proves the change, then run broader checks when the
change affects shared behavior, packaging, CI, or releases. See
`docs/maintenance/testing.md` for check-selection rules.
