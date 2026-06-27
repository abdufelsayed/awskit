#!/bin/sh

AWSKIT_RELEASE_PACKAGES="${AWSKIT_RELEASE_PACKAGES:-awskit,awskit-unix,awskit-lwt,awskit-lwt-unix,awskit-eio,awskit-s3,awskit-s3-sim,awskit-s3-lwt,awskit-s3-lwt-unix,awskit-s3-eio}"
AWSKIT_EXAMPLE_OPAM_PACKAGES="${AWSKIT_EXAMPLE_OPAM_PACKAGES:-eio_main tls-eio tls ca-certs domain-name mirage-crypto-rng}"

awskit_require_release_version() {
  if [ -n "${AWSKIT_RELEASE_VERSION:-}" ]; then
    export AWSKIT_RELEASE_VERSION
    return 0
  fi

  AWSKIT_RELEASE_VERSION=""
  awskit_release_branch=$(git symbolic-ref --quiet --short HEAD 2>/dev/null || true)
  case "$awskit_release_branch" in
    release/*)
      AWSKIT_RELEASE_VERSION=${awskit_release_branch#release/}
      AWSKIT_RELEASE_VERSION=${AWSKIT_RELEASE_VERSION#v}
      ;;
  esac

  if [ -z "$AWSKIT_RELEASE_VERSION" ]; then
    awskit_release_tag=$(git describe --exact-match --tags --match "v*" HEAD 2>/dev/null || true)
    case "$awskit_release_tag" in
      v*)
        AWSKIT_RELEASE_VERSION=${awskit_release_tag#v}
        ;;
    esac
  fi

  if [ -z "$AWSKIT_RELEASE_VERSION" ]; then
    echo "Unable to determine release version; set AWSKIT_RELEASE_VERSION." >&2
    exit 1
  fi

  export AWSKIT_RELEASE_VERSION
}
