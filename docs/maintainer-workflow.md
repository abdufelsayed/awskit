# Maintainer Workflow

Use this workflow before changing Awskit code, documentation, CI, release
processes, or repository metadata. It is written for maintainers and fresh
contributors who need durable project policy in one place.

## Start Here

1. Check the working tree:

   ```sh
   git status --short --branch
   ```

2. Read the maintainer document that matches the change:

   - `docs/architecture.md` for repository architecture and package-boundary
     rules.
   - `docs/development.md` for Awskit package, API, and implementation style.
   - `docs/testing.md` for test and validation selection.
   - `docs/security-threat-model.md` for security-sensitive behavior and
     protected assets.
   - `docs/release-gates.md` for production-ready release evidence.
   - `docs/changelog.md` for release-note and changelog rules.
   - `docs/release.md` for release branches, release PRs, tagging, and
     publication.
   - `docs/ci.md` for GitHub Actions expectations and CI debugging.
   - `docs/docs-publishing.md` for generated documentation publishing.

3. Inspect the files relevant to the change before editing.
4. Prefer established repository patterns over new abstractions.
5. Make the smallest change that correctly handles the requirement.
6. Clarify scope, ownership, or validation expectations before editing when
   they are not clear.

## Source Of Truth

- Public release notes live in `CHANGES.md`.
- Release PRs preview the changelog, but `CHANGES.md` is the canonical release
  ledger.
- Public package documentation lives in `packages/*/doc`.
- Maintainer workflows live in `docs`.
- CI, stress evidence, release validation, and documentation publishing live in
  `.github/workflows/*.yml`.
- Local and CI validation entrypoints live in `scripts/check.sh`.

## Working In The Tree

- Treat existing local changes as work in progress unless the owner asks for
  them to be reverted.
- Keep edits within the requested ownership boundary.
- Avoid unrelated refactors while making focused fixes.
- Preserve package boundaries and public API compatibility unless the relevant
  maintainer guide calls for a broader change.

## Writing Repository Text

Commit messages, PR bodies, docs, and changelogs should be factual and useful
without external context.

- Describe what changed.
- Explain why it matters when the reason is not obvious.
- Record validation when validation is relevant.
- Include issue, PR, and commit references where the workflow requires them.
- Omit conversation history, temporary process notes, and authorship commentary.
- Keep changelog entries focused on shipped public or maintainer-relevant
  changes.

## Pull Requests

The default PR template is `.github/pull_request_template.md`. Specialized
templates live in `.github/PULL_REQUEST_TEMPLATE`.

Use a specialized template with GitHub CLI by passing it as the body file:

```sh
gh pr create --body-file .github/PULL_REQUEST_TEMPLATE/release.md
```

Use a specialized template in the browser with the `template` query parameter:

```text
https://github.com/abdufelsayed/awskit/compare/main...release/vX.Y.Z?expand=1&template=release.md
```

After loading a template, replace every prompt with concrete, reviewable
content.

## Verification

Use the narrowest check that proves the change. Run broader checks when the
change affects shared behavior, packaging, CI, releases, or public APIs. See
`docs/testing.md` for check-selection and regression-test rules.
