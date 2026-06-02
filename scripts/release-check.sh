#!/bin/sh

set -eu

. "$(dirname "$0")/release-env.sh"

cleanup_minio() {
  if [ "${MINIO_STARTED:-0}" = "1" ]; then
    docker compose down
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

opam exec -- dune build @opam
opam lint ./*.opam
opam install . --yes --working-dir
opam exec -- dune fmt
opam exec -- dune runtest --force
opam exec -- dune build @all @doc @install
git diff --check

docker compose up -d
MINIO_STARTED=1
opam exec -- dune build --force @minio-contract
