# Changelog Policy

`CHANGES.md` is the canonical public release ledger for Awskit. It should help
users understand what changed, what can break, what was added, and what was
fixed.

## Entry Format

Every release entry must include traceability:

- Use `(#PR, commit)` when the change has an associated pull request.
- Use `(commit)` when the change does not have an associated pull request.

Good:

```md
- Added native streaming S3 upload and download APIs with adapter-level `Body`
  and `Reader` modules. (#5, 53a7642)
```

Good:

```md
- Treat empty `ListObjectVersions` pagination marker elements as absent while
  keeping strict validation for malformed non-empty version IDs. (e83d337)
```

Bad:

```md
- Entries include PR numbers when GitHub has an associated PR and always include
  the release commit.
```

The bad example explains the changelog process instead of describing a shipped
change.

## Sections

Use these sections when they apply:

- `Breaking`
- `Added`
- `Changed`
- `Fixed`
- `Documentation, CI, and Release`

Do not create empty sections.

## Ordering

Within each section, order entries by the release-branch timeline unless a
reader-facing order is clearly better. For example, a breaking API replacement
can appear before a smaller related cleanup even if the cleanup commit happened
first.

If the order is ambiguous, use the order from:

```sh
git log --reverse --oneline main..HEAD
```

## What Belongs

Add entries for:

- User-visible API changes.
- Behavior changes.
- Bug fixes.
- Runtime, packaging, and documentation publishing changes.
- CI and release process changes that affect contributors or release quality.
- Public documentation updates that materially improve package usage.

Do not add entries for:

- The fact that the changelog was edited.
- The fact that an agent or maintainer discussed a workflow.
- Internal implementation details with no user, contributor, or release impact.
- Negative statements about deleted APIs that are already covered by the
  replacement or breaking-change entry.

## Language

Write as a release note, not as a transcript.

Good:

```md
- Added GitHub Pages publishing for package documentation on main pushes and
  updated package documentation URLs to the Pages site. (d5abfa6)
```

Bad:

```md
- Added docs publishing because we agreed GitHub Pages is the cleanest setup.
```

## Review Checklist

Before committing `CHANGES.md`:

1. Every bullet has a PR number when available and a commit hash.
2. No bullet describes the mechanics of writing the changelog.
3. No bullet mentions a user, agent, chat, or discussion.
4. Each section is ordered intentionally.
5. Breaking changes tell users what changed and what to use now.
