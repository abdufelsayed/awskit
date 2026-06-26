#!/bin/sh

set -eu

. "$(dirname "$0")/release-env.sh"

cleanup_minio() {
  if [ "${MINIO_STARTED:-0}" = "1" ]; then
    docker compose down -v
  fi
}

trap cleanup_minio EXIT INT TERM

IFS=","
set -- $AWSKIT_RELEASE_PACKAGES
unset IFS

expected_count=0
for package in "$@"; do
  expected_count=$((expected_count + 1))
  test -f "$package.opam"
done

actual_count=$(find . -maxdepth 1 -name "*.opam" | wc -l | tr -d " ")
if [ "$actual_count" != "$expected_count" ]; then
  echo "Expected $expected_count opam packages from AWSKIT_RELEASE_PACKAGES, found $actual_count" >&2
  exit 1
fi

awskit_require_release_version

opam exec -- dune build @opam
opam lint ./*.opam
for package in "$@"; do
  opam pin add --yes --no-action "$package" .
done
opam install --yes --working-dir --with-test --with-doc --with-dev-setup "$@"
opam exec -- dune fmt
opam exec -- dune runtest --force
opam exec -- dune build @check-quick
if [ -n "$AWSKIT_EXAMPLE_OPAM_PACKAGES" ]; then
  opam install --yes $AWSKIT_EXAMPLE_OPAM_PACKAGES
fi
opam exec -- dune build @examples
opam exec -- dune build @all @doc @install
git diff --check
if ! git diff --quiet || ! git diff --cached --quiet ||
   [ -n "$(git ls-files --others --exclude-standard)" ]; then
  echo "Release checks require a clean git worktree before building the archive." >&2
  exit 1
fi
doc_log=$(mktemp "${TMPDIR:-/tmp}/awskit-doc.XXXXXX")
if ! opam exec -- dune build @doc >"$doc_log" 2>&1; then
  cat "$doc_log"
  rm -f "$doc_log"
  exit 1
fi
if grep -E "Warning|Error|Failed to resolve" "$doc_log"; then
  echo "Documentation build emitted warnings or unresolved references." >&2
  rm -f "$doc_log"
  exit 1
fi
rm -f "$doc_log"
opam exec -- dune-release check -V "$AWSKIT_RELEASE_VERSION"
opam exec -- dune-release distrib -V "$AWSKIT_RELEASE_VERSION"
dist_archive="_build/awskit-$AWSKIT_RELEASE_VERSION.tbz"
dist_dir=$(mktemp -d "${TMPDIR:-/tmp}/awskit-dist-doc.XXXXXX")
tar -xjf "$dist_archive" -C "$dist_dir"
dist_log=$(mktemp "${TMPDIR:-/tmp}/awskit-dist-doc.XXXXXX")
if ! (
  cd "$dist_dir/awskit-$AWSKIT_RELEASE_VERSION"
  opam exec -- dune build @doc
) >"$dist_log" 2>&1; then
  cat "$dist_log"
  rm -rf "$dist_dir" "$dist_log"
  exit 1
fi
if grep -E "Warning|Error|Failed to resolve" "$dist_log"; then
  echo "Distribution archive documentation emitted warnings or unresolved references." >&2
  rm -rf "$dist_dir" "$dist_log"
  exit 1
fi
rm -rf "$dist_dir" "$dist_log"

docker compose up -d
MINIO_STARTED=1
opam exec -- dune build --force @check-integration
