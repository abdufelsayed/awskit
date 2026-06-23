#!/bin/sh

set -eu

for file in \
  SUPPORT.md \
  SECURITY.md \
  docs/release-gates.md \
  docs/security-threat-model.md \
  api-snapshot/public-api-tiers.sexp \
  api-snapshot/current-installed-cmis.txt \
  scripts/check-github-ruleset.sh
do
  test -f "$file"
done

grep -q "Live AWS is not a release gate" SUPPORT.md
grep -q "S3-compatible" SUPPORT.md
grep -q "Presigned URLs are bearer tokens" SECURITY.md
grep -q "API snapshot" docs/release-gates.md
grep -q "branch protection" docs/release-gates.md
grep -q "ruleset" docs/release-gates.md

opam exec -- dune build @api-compile @api-snapshot @docs-content
