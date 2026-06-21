# Agent Workflow

This repository is maintained with human and agent contributors in mind. The
goal is for each repository artifact to be useful without access to the chat or
the person who requested the change.

## Start Here

1. If this document was opened directly, read `AGENTS.md` first.
2. Read the maintenance document that matches the change:

   - `docs/architecture.md` for repository architecture and
     package-boundary rules.
   - `docs/ocaml-development.md` for OCaml implementation style.
   - `docs/testing.md` for test and validation rules.
   - `docs/changelog.md` for changelog entries.
   - `docs/release.md` for release work.

3. Check the working tree:

   ```sh
   git status --short --branch
   ```

4. Inspect the files relevant to the request before editing.
5. Prefer the repository's existing patterns over new abstractions.
6. Make the smallest change that correctly handles the request.

## Do Not Write Chat Transcripts Into The Repo

Repository artifacts must describe the change, not the conversation that led to
the change.

Do not write:

```md
We discussed that release logs should mention PRs and commits.
```

Write:

```md
Release entries include PR and commit references for traceability.
```

Do not write:

```md
The user asked us to publish docs on GitHub Pages.
```

Write:

```md
Added GitHub Pages publishing for generated package documentation.
```

In changelogs, prefer the second pattern only when it describes a shipped change.
Do not add policy explanations as release entries.

## Write Descriptions For Future Readers

Commit messages, PR bodies, docs, and changelogs should be factual and durable.

- Say what changed.
- Say why it matters when the reason is not obvious.
- Say how it was validated when validation is relevant.
- Include issue, PR, and commit references where the workflow requires them.
- Avoid "as discussed", "the user asked", "we decided", "this document says",
  and similar self-referential phrasing.

## Choose The Right PR Template

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
content. Do not leave instructions in the final PR body.

## Verification

Before claiming a change is finished, run a command that proves the claim. See
`docs/testing.md` for check-selection and regression-test rules.
