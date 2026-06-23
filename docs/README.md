# Maintenance Workflows

This directory documents maintainer and agent workflows for Awskit.

- `agent-workflow.md` explains how agents should work in this repository.
- `architecture.md` defines Awskit's repository-specific engineering
  constraints, including package boundaries, runtime adapters, error handling,
  resource ownership, wire formats, streaming, and compatibility.
- `ocaml-development.md` defines OCaml implementation and API style guidance.
- `testing.md` defines test and validation rules.
- `security-threat-model.md` summarizes protected SDK assets, trust
  boundaries, and the executable evidence for security-sensitive behavior.
- `release-gates.md` defines the local, CI, API snapshot, and GitHub
  branch/ruleset evidence required for production-ready release claims.
- `changelog.md` defines release-note and changelog rules.
- `release.md` is the release branch, release PR, tagging, and publication
  playbook.
- `ci.md` explains GitHub Actions expectations and CI debugging.
- `docs-publishing.md` explains GitHub Pages documentation publishing.

Repository root `SUPPORT.md` and `SECURITY.md` define public support and
security policy.

For repository-wide rules, start with `AGENTS.md`.
