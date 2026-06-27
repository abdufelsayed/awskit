#!/bin/sh

set -eu

. "$(dirname "$0")/release-env.sh"

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
release_opam_switch=$(opam switch show)

opam exec -- dune build @opam
opam lint ./*.opam
isolation_compiler=${AWSKIT_OPAM_ISOLATION_COMPILER_PACKAGE:-}
if [ -z "$isolation_compiler" ]; then
  isolation_compiler="ocaml-base-compiler.$(opam exec -- ocamlc -version)"
fi
isolation_prefix=${AWSKIT_OPAM_ISOLATION_SWITCH_PREFIX:-awskit-release-isolation}
isolation_switch="$isolation_prefix-$$"
cleanup_isolation_switch() {
  if [ "${AWSKIT_OPAM_ISOLATION_KEEP_SWITCHES:-}" = "1" ]; then
    return 0
  fi

  opam switch remove --yes "$isolation_switch" >/dev/null 2>&1 || true
}
trap cleanup_isolation_switch EXIT HUP INT TERM
echo "Creating package isolation switch $isolation_switch with $isolation_compiler"
opam switch create --yes --no-switch "$isolation_switch" "$isolation_compiler"
for pinned_package in "$@"; do
  opam pin add --switch="$isolation_switch" --yes --no-action --kind=path \
    "$pinned_package" .
done
isolation_base_packages=$(opam list --switch="$isolation_switch" --installed --short)
cleanup_isolation_packages() {
  installed_packages=$(opam list --switch="$isolation_switch" --installed --short)
  remove_packages=""
  for installed_package in $installed_packages; do
    keep_package=0
    for base_package in $isolation_base_packages; do
      if [ "$installed_package" = "$base_package" ]; then
        keep_package=1
        break
      fi
    done
    if [ "$keep_package" = "0" ]; then
      remove_packages="$remove_packages $installed_package"
    fi
  done

  if [ -n "$remove_packages" ]; then
    opam remove --switch="$isolation_switch" --yes --auto-remove $remove_packages
  fi
}
for package in "$@"; do
  cleanup_isolation_packages
  echo "Checking isolated opam metadata for $package with $isolation_compiler"
  opam install --switch="$isolation_switch" --yes --deps-only --with-test \
    "$package"
  opam exec --switch="$isolation_switch" -- dune build -p "$package" @install @runtest
done
cleanup_isolation_packages
for package in "$@"; do
  opam pin add --yes --no-action --kind=path "$package" .
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
  opam exec --switch="$release_opam_switch" -- dune build @doc
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

scripts/test-report.sh integration --label release-minio-integration
