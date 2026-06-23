#!/bin/sh

set -eu

repo="${AWSKIT_GITHUB_REPOSITORY:-abdufelsayed/awskit}"
branch="${1:-main}"

if ! command -v gh >/dev/null 2>&1; then
  echo "GitHub CLI is required for branch/ruleset release verification." >&2
  exit 2
fi

if gh api "repos/$repo/branches/$branch/protection" >/dev/null 2>&1; then
  echo "Branch protection exists for $repo:$branch"
  exit 0
fi

branch_ruleset_count=$(
  gh api "repos/$repo/rulesets" \
    --jq '[.[] | select(.enforcement != "disabled" and .target == "branch")] | length' \
    2>/dev/null \
  || true
)

case "$branch_ruleset_count" in
  ''|*[!0-9]*)
    echo "Unable to verify repository rulesets for $repo." >&2
    echo "Run: gh api repos/$repo/rulesets" >&2
    exit 2
    ;;
  0)
    echo "No enabled branch protection or repository ruleset found for $repo:$branch." >&2
    echo "Release governance requires protected main or an enabled ruleset." >&2
    exit 1
    ;;
  *)
    echo "Found $branch_ruleset_count enabled branch ruleset(s) for $repo"
    ;;
esac
