# Maintainer Documentation

This directory contains Awskit's maintainer policies and workflows.

- `maintainer-workflow.md` is the starting point for repository maintenance.
- `architecture.md` defines Awskit's repository-specific engineering
  constraints, including package boundaries, runtime adapters, error handling,
  resource ownership, wire formats, streaming, and compatibility.
- `development.md` defines Awskit package, API, and implementation guidance.
- `testing.md` defines test and validation rules.
- `security-threat-model.md` summarizes protected SDK assets, trust
  boundaries, and the executable evidence for security-sensitive behavior.
- `observability.md` documents typed observation semantics, public sink
  contracts, and application-owned reporter/exporter policy.
- `release-gates.md` defines the local, CI, and public API review evidence
  required for production-ready release claims.
- `changelog.md` defines release-note and changelog rules.
- `release.md` is the release branch, release PR, tagging, and publication
  playbook.
- `ci.md` explains GitHub Actions expectations and CI debugging.
- `docs-publishing.md` explains GitHub Pages documentation publishing.

Repository root `SUPPORT.md` and `SECURITY.md` define public support and
security policy.

Automation-specific instructions live in repository root `AGENTS.md`; they
point back to these maintainer docs as the source of durable project policy.
