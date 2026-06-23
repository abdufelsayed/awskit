#!/bin/sh

set -eu

for file in \
  SUPPORT.md \
  SECURITY.md \
  README.md \
  docs/release-gates.md \
  docs/security-threat-model.md
do
  test -f "$file"
done

grep -q "caller-owned" SUPPORT.md
grep -q "MinIO" SUPPORT.md
grep -q "Live AWS is not a release gate" SUPPORT.md
grep -q "S3-compatible" SUPPORT.md
grep -q "Credential" SUPPORT.md
grep -q "Do not include AWS access keys" SECURITY.md
grep -q "Presigned URLs are bearer tokens" SECURITY.md
grep -q "Unsafe_diagnostics" SECURITY.md
grep -q "API snapshot" docs/release-gates.md
grep -q "branch protection" docs/release-gates.md

if grep -R -n -E "Ready-to-use adapters for Eio|ready-to-use Eio" \
  README.md packages docs; then
  echo "Eio must be documented as caller-owned transport setup." >&2
  exit 1
fi

if grep -R -n -E \
  "supports every S3-compatible|support every S3-compatible|all S3-compatible providers|fully compatible with S3-compatible" \
  README.md SUPPORT.md packages docs; then
  echo "S3-compatible support claims are too broad." >&2
  exit 1
fi

if grep -R -n -E \
  "Format\\.printf.*reveal_url|Fmt\\.pr.*reveal_url|Lwt_io\\.print.*reveal_url|print_endline.*reveal_url" \
  README.md packages docs examples; then
  echo "Do not print raw presigned bearer URLs by default." >&2
  exit 1
fi
