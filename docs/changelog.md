# Changelog Policy

`CHANGES.md` is the canonical public release ledger for Awskit. It should help
users understand what changed, what can break, what was added, and what was
fixed between public releases.

Related maintainer docs:

- `docs/release.md` explains how to build the release ledger.
- `docs/release-gates.md` defines the evidence that release PRs must record.
- `docs/testing.md` explains when a fix needs new regression evidence.

## Entry Requirements

Every release entry describes a meaningful shipped change and includes
traceability for the work that materially contributed to that item:

- Use `(#PR, commit)` when the change has an associated pull request.
- Use `(commit)` when the change does not have an associated pull request.
- Include multiple PR and commit references when multiple changes contributed
  to the same release entry.

Examples:

```md
- Added native streaming S3 upload and download APIs with adapter-level `Body`
  and `Reader` modules. (#5, 53a7642)
```

```md
- Treat empty `ListObjectVersions` pagination marker elements as absent while
  keeping strict validation for malformed non-empty version IDs. (e83d337)
```

```md
- Added native streaming S3 upload and download APIs with adapter-level `Body`
  and `Reader` modules, examples, and package documentation. (#5, 53a7642; #6,
  8c9d012)
```

Traceability means every release entry has the PR and commit references that
materially contributed to that item. It does not mean every commit becomes a
release entry.

## Sections

Use these sections when they apply:

- `Breaking`
- `Added`
- `Changed`
- `Fixed`
- `Documentation, CI, and Release`

Do not create empty sections.

Breaking public API entries should name the replacement or caller action and
include the same material PR and commit references as the implementation.

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

Leave out:

- Commit-by-commit history.
- Fixups for bugs introduced and fixed before the release shipped.
- Formatting-only changes.
- Typo fixes to unreleased changelog or documentation edits.
- The mechanics of writing the changelog.
- Internal decision-making history.
- Internal implementation details with no user, contributor, or release impact.
- Negative statements about deleted APIs that are already covered by the
  replacement or breaking-change entry.

If a change was introduced and corrected before the release shipped, collapse
that work into the final user-facing entry. If a bug shipped in a previous
public release, include the fix as a release entry.

## Ordering

Within each section, order entries by the release-branch timeline unless a
reader-facing order is clearly better. For example, a breaking API replacement
can appear before a smaller related cleanup even if the cleanup commit happened
first.

If the order is ambiguous, use the order from:

```sh
git log --reverse --oneline main..HEAD
```

## Language

Write as a release note:

- Start with the user-visible change.
- Mention the reason only when it helps readers understand impact or migration.
- Keep process context in release PRs and maintainer docs, not in public release
  entries.
- Use past tense release-note phrasing such as `Added`, `Changed`, `Fixed`, and
  `Removed`.

Example:

```md
- Added GitHub Pages publishing for package documentation on main pushes and
  updated package documentation URLs to the Pages site. (d5abfa6)
```

## Review Checklist

Before committing `CHANGES.md`:

1. Every bullet has a PR number when available and a commit hash.
2. Every bullet describes a meaningful public or maintainer-relevant change.
3. Each section is ordered intentionally.
4. Breaking changes tell users what changed and what to use now.
5. Multiple commits that ship one user-facing change are collapsed into one
   entry with all material references.
6. Internal process, unreleased self-repair, and changelog-writing mechanics
   are absent from public entries.
