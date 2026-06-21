# Changelog Policy

`CHANGES.md` is the canonical public release ledger for Awskit. It should help
users understand what changed, what can break, what was added, and what was
fixed.

## Entry Format

Every release entry must include traceability for the work that materially
contributed to that released item:

- Use `(#PR, commit)` when the change has an associated pull request.
- Use `(commit)` when the change does not have an associated pull request.
- Include multiple PR and commit references when multiple changes contributed
  to the same release entry.

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

Good:

```md
- Added native streaming S3 upload and download APIs with adapter-level `Body`
  and `Reader` modules, examples, and package documentation. (#5, 53a7642; #6,
  8c9d012)
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

`CHANGES.md` is a curated release ledger, not a public `git log`. It should
describe the difference between the previous public release and the new public
release.

Add entries for:

- User-visible API changes.
- Behavior changes.
- Bug fixes.
- Runtime, packaging, and documentation publishing changes.
- CI and release process changes that affect contributors or release quality.
- Public documentation updates that materially improve package usage.

Do not add entries for:

- Every commit on the release branch.
- Fixups for bugs introduced and fixed before the release shipped.
- Formatting-only changes.
- Typo fixes to unreleased changelog or documentation edits.
- The fact that the changelog was edited.
- The fact that an agent or maintainer discussed a workflow.
- Internal implementation details with no user, contributor, or release impact.
- Negative statements about deleted APIs that are already covered by the
  replacement or breaking-change entry.

If a change was introduced and corrected before the release shipped, collapse
that work into the final user-facing entry. If a bug shipped in a previous
public release, include the fix as a release entry.

Traceability means every release entry has the PR and commit references that
materially contributed to that item. It does not mean every commit becomes a
release entry.

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
2. Every bullet describes a meaningful public or maintainer-relevant change.
3. No bullet describes unreleased self-repair or commit-by-commit history.
4. No bullet describes the mechanics of writing the changelog.
5. No bullet mentions a user, agent, chat, or discussion.
6. Each section is ordered intentionally.
7. Breaking changes tell users what changed and what to use now.
