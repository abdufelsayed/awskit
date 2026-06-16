# Agent Workflow

This repository is maintained with human and agent contributors in mind. The
goal is for each repository artifact to be useful without access to the chat or
the person who requested the change.

## Start Here

1. Read `AGENTS.md`.
2. Check the working tree:

   ```sh
   git status --short --branch
   ```

3. Inspect the files relevant to the request before editing.
4. Prefer the repository's existing patterns over new abstractions.
5. Make the smallest change that correctly handles the request.

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

## Test Current Behavior

Tests should protect the current supported contract. When removing old behavior,
do not add tests that only prove the old symbol, option, or module is gone.

Use these replacements instead:

- Test the new API that replaces the old one.
- Test the migration-sensitive behavior users rely on now.
- Test the regression symptom that motivated the change.
- Test public boundaries through exposed modules.
- Let `dune build` prove removed symbols are no longer available.

Good:

```ocaml
let test_object_get_uses_scoped_reader () =
  (* Verify the supported reader workflow. *)
  ()
```

Bad:

```ocaml
let test_object_get_string_no_longer_exists () =
  (* Tombstone tests for removed symbols do not belong in the suite. *)
  ()
```

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

Before claiming a change is finished, run a command that proves the claim.

Common commands:

```sh
opam exec -- dune fmt
opam exec -- dune build
opam exec -- dune test
opam exec -- dune build @doc
git diff --check
```

For S3 contract work:

```sh
docker compose up -d
opam exec -- dune build --force @minio-contract
docker compose down -v
```

For releases:

```sh
scripts/release-check.sh
```

If a broad command is too expensive for the current change, run the focused
command that proves the touched behavior and clearly state what was not run.
