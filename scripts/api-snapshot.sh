#!/bin/sh

set -eu

snapshot_dir="api-snapshot"
snapshot_file="$snapshot_dir/current-installed-cmis.txt"
tiers_file="$snapshot_dir/public-api-tiers.sexp"

test -f "$tiers_file"

tmp_dir=""
tmp_file=""
cleanup() {
  if [ -n "$tmp_dir" ]; then
    rm -rf "$tmp_dir"
  fi
  if [ -n "$tmp_file" ]; then
    rm -f "$tmp_file"
  fi
}
trap cleanup EXIT INT TERM

if [ "${AWSKIT_API_SNAPSHOT_USE_CURRENT_BUILD:-0}" = "1" ]; then
  if [ -d "_build/install/default/lib" ]; then
    lib_dir="_build/install/default/lib"
  elif [ -d "../install/default/lib" ]; then
    lib_dir="../install/default/lib"
  else
    echo "Unable to find the current Dune install tree." >&2
    echo "Run opam exec -- dune build @install before snapshot comparison." >&2
    exit 1
  fi
  tmp_file=$(mktemp "${TMPDIR:-/tmp}/awskit-api-snapshot.XXXXXX")
  generated="$tmp_file"
else
  tmp_root="${AWSKIT_API_SNAPSHOT_TMPDIR:-/tmp}"
  tmp_dir=$(mktemp -d "$tmp_root/awskit-api-snapshot.XXXXXX")
  build_dir="$tmp_dir/build"
  generated="$tmp_dir/current-installed-cmis.txt"
  opam exec -- dune build --build-dir "$build_dir" @install
  lib_dir="$build_dir/install/default/lib"
fi

test -d "$lib_dir"

find "$lib_dir" -name "*.cmi" \
  ! -path "*/.private/*" \
  | sed "s#^$lib_dir/##" \
  | sort > "$generated"

if grep -E "/[Ss]imulator_[^/]*\\.cmi$" "$generated"; then
  echo "api snapshot found public simulator implementation CMIs" >&2
  echo "Keep the supported simulator API under Awskit_s3_sim." >&2
  exit 1
fi

if [ "${AWSKIT_UPDATE_API_SNAPSHOT:-0}" = "1" ]; then
  mkdir -p "$snapshot_dir"
  cp "$generated" "$snapshot_file"
  echo "Updated $snapshot_file"
  exit 0
fi

if [ ! -f "$snapshot_file" ]; then
  echo "Missing $snapshot_file" >&2
  echo "Run AWSKIT_UPDATE_API_SNAPSHOT=1 scripts/api-snapshot.sh" >&2
  exit 1
fi

if ! cmp -s "$generated" "$snapshot_file"; then
  echo "Public API snapshot drift detected." >&2
  echo "Review the diff, update role tiers if needed, then refresh intentionally:" >&2
  echo "  AWSKIT_UPDATE_API_SNAPSHOT=1 scripts/api-snapshot.sh" >&2
  diff -u "$snapshot_file" "$generated" || true
  exit 1
fi

echo "API snapshot matches $snapshot_file"
