# Agent Instructions

These instructions apply to automated contributors. Human maintainer policy
lives in `docs/maintainer-workflow.md` and the rest of `docs`.

## Start Here

1. Read `docs/maintainer-workflow.md`.
2. Check the working tree before editing:

   ```sh
   git status --short --branch
   ```

3. Read the maintainer document that matches the change. `docs/README.md`
   lists the available guides.
4. Inspect the relevant files before editing and keep changes within the
   requested scope. When scope or ownership is unclear, ask before editing.

## Repository Text

Use durable maintainer language in commits, PR bodies, release notes, and docs.
Do not encode conversation history or agent process in repository artifacts.

## Required Checks

Use the narrowest check that proves the change, then run broader checks when the
change affects shared behavior, packaging, CI, or releases. See
`docs/testing.md` for check-selection rules.
