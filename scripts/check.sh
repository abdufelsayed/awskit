#!/bin/sh

set -u

usage() {
  cat <<'USAGE'
Usage: scripts/check.sh CHECK [--package PACKAGE] [--log-dir DIR] [--label LABEL]

Checks:
  package            Install and test one opam package.
  package-metadata   Generated opam, opam lint, formatting, and drift checks.
  package-isolation  Per-package opam install/test isolation.
  docs-examples      Documentation and examples.
  no-network         Reported no-network correctness evidence.
  minio              Reported bounded MinIO integration evidence.
  stress             Reported high-cost discovery evidence.
  release-archive    Release archive and archive documentation checks.
  release            Composed local release gate.

Options:
  --package PACKAGE
                 Package name for the package check.
  --log-dir DIR  Write reports under DIR instead of .logs.
  --label LABEL  Use LABEL in the report filename.
  -h, --help     Show this help.

Environment:
  AWSKIT_MINIO_OPAM_PACKAGES
  AWSKIT_RELEASE_PACKAGES
  AWSKIT_EXAMPLE_OPAM_PACKAGES
                            Override check package sets.
  AWSKIT_QCHECK_COUNT       Overrides generated no-service workload counts.
  AWSKIT_STRESS_QCHECK_COUNT
                            Default count used by stress for @check-stress
                            when AWSKIT_QCHECK_COUNT is unset. Defaults to
                            2000.
  AWSKIT_MINIO_QCHECK_COUNT Optional count override for script-managed MinIO
                            aliases. Unset uses MinIO profile defaults.
  QCHECK_SEED              Replays a printed QCheck seed.
USAGE
}

check=
package=
log_dir=.logs
label=

while [ "$#" -gt 0 ]; do
  case "$1" in
    package | package-metadata | package-isolation | \
    docs-examples | no-network | minio | stress | release-archive | release)
      if [ -n "$check" ]; then
        echo "Only one CHECK may be provided." >&2
        exit 2
      fi
      check=$1
      shift
      ;;
    --package)
      if [ "$#" -lt 2 ]; then
        echo "--package requires a value" >&2
        exit 2
      fi
      package=$2
      shift 2
      ;;
    --log-dir)
      if [ "$#" -lt 2 ]; then
        echo "--log-dir requires a value" >&2
        exit 2
      fi
      log_dir=$2
      shift 2
      ;;
    --label)
      if [ "$#" -lt 2 ]; then
        echo "--label requires a value" >&2
        exit 2
      fi
      label=$2
      shift 2
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [ -z "$check" ]; then
  echo "Missing CHECK." >&2
  usage >&2
  exit 2
fi

if ! root=$(git rev-parse --show-toplevel 2>/dev/null); then
  echo "scripts/check.sh must run inside a git checkout." >&2
  exit 2
fi

cd "$root"

. "$root/scripts/release-env.sh"

AWSKIT_MINIO_OPAM_PACKAGES="${AWSKIT_MINIO_OPAM_PACKAGES:-awskit awskit-unix awskit-lwt awskit-lwt-unix awskit-eio awskit-s3 awskit-s3-lwt awskit-s3-lwt-unix awskit-s3-eio}"

stress_check_qcheck_count=
case "$check" in
  stress)
    if [ -n "${AWSKIT_QCHECK_COUNT:-}" ]; then
      stress_check_qcheck_count=$AWSKIT_QCHECK_COUNT
    else
      stress_check_qcheck_count=${AWSKIT_STRESS_QCHECK_COUNT:-2000}
    fi
    ;;
esac

slug_source=${label:-$check}
slug=$(printf "%s" "$slug_source" | tr -c "A-Za-z0-9_.-" "-" | sed 's/--*/-/g; s/^-//; s/-$//')
if [ -z "$slug" ]; then
  slug=$check
fi

timestamp=$(date -u +%Y%m%dT%H%M%SZ)
report="$log_dir/awskit-test-$slug-$timestamp.log"
latest="$log_dir/awskit-test-$slug-latest.log"
artifact_dir="$log_dir/awskit-test-$slug-$timestamp-artifacts"
artifact_count=0
alcotest_output_limit=20
alcotest_output_tail_lines=240
failed=0
minio_started=0
minio_failed=0
isolation_switch=
report_started=0

display_path() {
  case "$1" in
    /*)
      printf "%s\n" "$1"
      ;;
    *)
      printf "%s/%s\n" "$root" "$1"
      ;;
  esac
}

append_line() {
  printf "%s\n" "$*" | tee -a "$report"
}

append_file() {
  cat "$1" | tee -a "$report"
}

append_tail_file() {
  tail -n "$1" "$2" | tee -a "$report"
}

print_command() {
  printf "%s" "$"
  for part in "$@"; do
    printf " %s" "$part"
  done
  printf "\n"
}

record_failure() {
  failed=1
}

run_checked() {
  print_command "$@"
  "$@"
  status=$?
  if [ "$status" -ne 0 ]; then
    exit "$status"
  fi
}

alcotest_output_has_failure_marker() {
  grep -E \
    '\[exception\]|\[FAIL\]|\[ERROR\]|failed on|qcheck: user fail|Alcotest assertion failure|ASSERT .* failed' \
    "$1" >/dev/null 2>&1
}

append_recent_alcotest_outputs() {
  marker=$1
  output_list="$report.outputs.$$"
  captured_count=0
  omitted_count=0

  if [ ! -d "$root/_build" ]; then
    return
  fi

  find "$root/_build" -path "*/_build/_tests/*" -name "*.output" -type f \
    -newer "$marker" -print >"$output_list" 2>/dev/null || true

  if [ ! -s "$output_list" ]; then
    rm -f "$output_list"
    return
  fi

  while IFS= read -r output; do
    if [ ! -f "$output" ]; then
      continue
    fi
    if ! alcotest_output_has_failure_marker "$output"; then
      continue
    fi

    if [ "$captured_count" -eq 0 ]; then
      append_line ""
      append_line "Recent failure-marked Alcotest .output artifacts:"
    fi

    if [ "$captured_count" -ge "$alcotest_output_limit" ]; then
      omitted_count=$((omitted_count + 1))
      continue
    fi

    captured_count=$((captured_count + 1))
    artifact_count=$((artifact_count + 1))
    artifact_name=$(printf "alcotest-output-%03d.output" "$artifact_count")
    mkdir -p "$artifact_dir"
    cp "$output" "$artifact_dir/$artifact_name" 2>/dev/null || true

    append_line ""
    append_line "--- Alcotest output tail: $(display_path "$output") ---"
    if [ -f "$artifact_dir/$artifact_name" ]; then
      append_line "saved_copy=$(display_path "$artifact_dir/$artifact_name")"
    fi
    append_line "inline_tail_lines=$alcotest_output_tail_lines"
    append_tail_file "$alcotest_output_tail_lines" "$output"
    append_line "--- end Alcotest output ---"
  done <"$output_list"

  if [ "$omitted_count" -gt 0 ]; then
    append_line ""
    append_line "Omitted $omitted_count additional failure-marked Alcotest .output artifacts after cap=$alcotest_output_limit."
  fi

  rm -f "$output_list"
}

run_cmd() {
  description=$1
  shift
  tmp="$report.tmp.$$"
  marker="$report.marker.$$"

  append_line ""
  append_line "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] START $description"
  print_command "$@" >>"$report"

  : >"$marker"
  "$@" >"$tmp" 2>&1
  cmd_status=$?
  append_file "$tmp"
  rm -f "$tmp"

  append_line "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] EXIT $description status=$cmd_status"
  if [ "$cmd_status" -ne 0 ]; then
    append_recent_alcotest_outputs "$marker"
    record_failure
  fi
  rm -f "$marker"

  return "$cmd_status"
}

dune_build() {
  description=$1
  shift
  run_cmd "$description" opam exec -- dune build --root "$root" "$@"
}

dune_build_force() {
  description=$1
  shift
  run_cmd "$description" opam exec -- dune build --root "$root" --force "$@"
}

with_qcheck_count() {
  qcheck_count=$1
  shift

  qcheck_count_was_set=0
  qcheck_count_old=
  if [ "${AWSKIT_QCHECK_COUNT+x}" = x ]; then
    qcheck_count_was_set=1
    qcheck_count_old=$AWSKIT_QCHECK_COUNT
  fi

  if [ -n "$qcheck_count" ]; then
    AWSKIT_QCHECK_COUNT=$qcheck_count
    export AWSKIT_QCHECK_COUNT
  else
    unset AWSKIT_QCHECK_COUNT
  fi

  "$@"
  qcheck_count_status=$?

  if [ "$qcheck_count_was_set" = "1" ]; then
    AWSKIT_QCHECK_COUNT=$qcheck_count_old
    export AWSKIT_QCHECK_COUNT
  else
    unset AWSKIT_QCHECK_COUNT
  fi

  return "$qcheck_count_status"
}

dune_build_force_with_qcheck_count() {
  qcheck_count=$1
  description=$2
  shift 2
  with_qcheck_count "$qcheck_count" dune_build_force "$description" "$@"
}

run_cmd_with_qcheck_count() {
  qcheck_count=$1
  description=$2
  shift 2
  with_qcheck_count "$qcheck_count" run_cmd "$description" "$@"
}

run_metadata() {
  {
    echo "# Awskit test report"
    echo "started_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "check=$check"
    echo "root=$root"
    echo "branch=$(git branch --show-current 2>/dev/null || true)"
    echo "head=$(git rev-parse --short HEAD 2>/dev/null || true)"
    echo "log_path=$(display_path "$report")"
    echo "AWSKIT_QCHECK_COUNT=${AWSKIT_QCHECK_COUNT:-}"
    echo "AWSKIT_STRESS_QCHECK_COUNT=${AWSKIT_STRESS_QCHECK_COUNT:-}"
    echo "AWSKIT_MINIO_QCHECK_COUNT=${AWSKIT_MINIO_QCHECK_COUNT:-}"
    echo "stress_check_qcheck_count=${stress_check_qcheck_count:-}"
    echo "QCHECK_SEED=${QCHECK_SEED:-}"
    echo "AWSKIT_INTEGRATION_PROFILE=${AWSKIT_INTEGRATION_PROFILE:-}"
  } | tee -a "$report"

  run_cmd "git status --short --branch" git status --short --branch
  run_cmd "opam exec -- dune --version" opam exec -- dune --version
  run_cmd "opam exec -- ocamlc -version" opam exec -- ocamlc -version
}

start_report() {
  if [ "$report_started" = "1" ]; then
    return 0
  fi

  mkdir -p "$log_dir"
  report_started=1
  run_metadata
}

finish_report() {
  if [ "$report_started" != "1" ]; then
    return 0
  fi

  append_line ""
  append_line "finished_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  append_line "overall_failed=$failed"
  append_line "report_path=$(display_path "$report")"
  if [ -d "$artifact_dir" ]; then
    append_line "artifact_dir=$(display_path "$artifact_dir")"
  fi

  ln -sf "$(basename "$report")" "$latest"
  append_line "latest_report=$(display_path "$latest")"

  if [ "$failed" -ne 0 ]; then
    exit 1
  fi
}

report_no_network() {
  start_report
  dune_build_force "opam exec -- dune build --force @check-quick" @check-quick
}

report_docs_examples() {
  dune_build "opam exec -- dune build @examples @doc" @examples @doc
}

dump_minio_logs_if_needed() {
  if [ "$minio_started" = "1" ] && [ "$minio_failed" = "1" ]; then
    run_cmd "docker compose ps -a" docker compose ps -a
    run_cmd "docker compose logs minio" docker compose logs minio
    run_cmd "docker compose logs minio-setup" docker compose logs minio-setup
  fi
}

compose_project_has_containers() {
  compose_container_ids=$(docker compose ps -a -q 2>/dev/null || true)
  [ -n "$compose_container_ids" ]
}

configure_script_minio() {
  if [ -z "${AWSKIT_S3_MINIO_ENDPOINT:-}" ]; then
    AWSKIT_S3_MINIO_ENDPOINT=http://127.0.0.1:9000
  fi
  if [ -z "${AWSKIT_S3_MINIO_ACCESS_KEY_ID:-}" ]; then
    AWSKIT_S3_MINIO_ACCESS_KEY_ID=minioadmin
  fi
  if [ -z "${AWSKIT_S3_MINIO_SECRET_ACCESS_KEY:-}" ]; then
    AWSKIT_S3_MINIO_SECRET_ACCESS_KEY=minioadmin
  fi
  if [ -z "${AWSKIT_S3_MINIO_REGION:-}" ]; then
    AWSKIT_S3_MINIO_REGION=us-east-1
  fi

  export AWSKIT_S3_MINIO_ENDPOINT
  export AWSKIT_S3_MINIO_ACCESS_KEY_ID
  export AWSKIT_S3_MINIO_SECRET_ACCESS_KEY
  export AWSKIT_S3_MINIO_REGION

  append_line ""
  append_line "Script-managed MinIO configuration:"
  append_line "AWSKIT_S3_MINIO_ENDPOINT=$AWSKIT_S3_MINIO_ENDPOINT"
  append_line "AWSKIT_S3_MINIO_ACCESS_KEY_ID=$AWSKIT_S3_MINIO_ACCESS_KEY_ID"
  append_line "AWSKIT_S3_MINIO_SECRET_ACCESS_KEY=<set>"
  append_line "AWSKIT_S3_MINIO_REGION=$AWSKIT_S3_MINIO_REGION"
  append_line "AWSKIT_S3_MINIO_UNSAFE_HTTP=${AWSKIT_S3_MINIO_UNSAFE_HTTP:-}"
  if [ -n "${AWSKIT_MINIO_QCHECK_COUNT:-}" ]; then
    append_line "AWSKIT_MINIO_QCHECK_COUNT=$AWSKIT_MINIO_QCHECK_COUNT"
  else
    append_line "AWSKIT_MINIO_QCHECK_COUNT=<unset; using MinIO profile defaults>"
  fi
}

start_minio() {
  if ! command -v docker >/dev/null 2>&1; then
    append_line ""
    append_line "docker CLI not found; MinIO integration was not run."
    record_failure
    return 1
  fi

  minio_started=1
  if compose_project_has_containers; then
    minio_started=0
    append_line ""
    append_line "Existing Docker Compose resources for this checkout are already present."
    append_line "Refusing to share a script-managed MinIO lifecycle; run docker compose down -v or use a separate checkout."
    run_cmd "docker compose ps -a" docker compose ps -a
    record_failure
    return 1
  fi

  if ! run_cmd "docker compose up -d" docker compose up -d; then
    minio_failed=1
    append_line "docker compose up failed; MinIO integration aliases were not run."
    dump_minio_logs_if_needed
    stop_minio
    return 1
  fi
}

stop_minio() {
  if [ "$minio_started" = "1" ]; then
    if run_cmd "docker compose down -v" docker compose down -v; then
      minio_started=0
    fi
  fi
}

run_minio_bounded() {
  minio_failed=0
  if ! start_minio; then
    return 1
  fi

  configure_script_minio
  minio_qcheck_prefix=
  if [ -n "${AWSKIT_MINIO_QCHECK_COUNT:-}" ]; then
    minio_qcheck_prefix="AWSKIT_QCHECK_COUNT=$AWSKIT_MINIO_QCHECK_COUNT "
  fi
  if ! dune_build_force_with_qcheck_count "${AWSKIT_MINIO_QCHECK_COUNT:-}" \
    "${minio_qcheck_prefix}opam exec -- dune build --force @check-integration" \
    @check-integration
  then
    minio_failed=1
  fi

  dump_minio_logs_if_needed
  stop_minio
}

run_minio_expensive() {
  minio_failed=0
  if ! start_minio; then
    return 1
  fi

  configure_script_minio
  minio_qcheck_prefix=
  if [ -n "${AWSKIT_MINIO_QCHECK_COUNT:-}" ]; then
    minio_qcheck_prefix="AWSKIT_QCHECK_COUNT=$AWSKIT_MINIO_QCHECK_COUNT "
  fi
  if ! run_cmd_with_qcheck_count "${AWSKIT_MINIO_QCHECK_COUNT:-}" \
    "${minio_qcheck_prefix}AWSKIT_INTEGRATION_PROFILE=expensive opam exec -- dune build --force @s3-minio" \
    env AWSKIT_INTEGRATION_PROFILE=expensive opam exec -- dune build --root "$root" \
      --force @s3-minio
  then
    minio_failed=1
  fi

  dump_minio_logs_if_needed
  stop_minio
}

run_minio() {
  include_expensive=$1

  run_minio_bounded

  if [ "$include_expensive" = "yes" ]; then
    run_minio_expensive
  fi
}

cleanup_isolation_switch() {
  if [ -z "$isolation_switch" ]; then
    return 0
  fi
  if [ "${AWSKIT_OPAM_ISOLATION_KEEP_SWITCHES:-}" = "1" ]; then
    return 0
  fi

  opam switch remove --yes "$isolation_switch" >/dev/null 2>&1 || true
  isolation_switch=
}

release_packages() {
  printf "%s" "$AWSKIT_RELEASE_PACKAGES" | tr "," " "
}

check_release_package_count() {
  expected_count=0
  for package in $(release_packages); do
    expected_count=$((expected_count + 1))
    test -f "$package.opam"
  done

  actual_count=$(find . -maxdepth 1 -name "*.opam" | wc -l | tr -d " ")
  if [ "$actual_count" != "$expected_count" ]; then
    echo "Expected $expected_count opam packages from AWSKIT_RELEASE_PACKAGES, found $actual_count" >&2
    exit 1
  fi
}

check_clean_worktree() {
  git diff --check
  if ! git diff --quiet || ! git diff --cached --quiet ||
     [ -n "$(git ls-files --others --exclude-standard)" ]; then
    echo "Release checks require a clean git worktree before building the archive." >&2
    exit 1
  fi
}

check_package_metadata() {
  run_checked opam install --yes --with-test --with-doc --with-dev-setup --deps-only .
  run_checked opam exec -- dune build @opam
  run_checked opam lint ./*.opam
  run_checked opam exec -- dune fmt
  run_checked git diff --check
  run_checked git diff --exit-code
}

check_package() {
  if [ -z "$package" ]; then
    echo "The package check requires --package PACKAGE." >&2
    exit 2
  fi
  if [ ! -f "$package.opam" ]; then
    echo "No opam file found for package: $package" >&2
    exit 2
  fi

  run_checked opam install --yes --with-test --deps-only "$package"
  run_checked opam exec -- dune build -p "$package" @install @runtest
}

check_docs_examples() {
  run_checked opam install --yes --with-test --with-doc --deps-only .
  if [ -n "$AWSKIT_EXAMPLE_OPAM_PACKAGES" ]; then
    run_checked opam install --yes $AWSKIT_EXAMPLE_OPAM_PACKAGES
  fi
  run_checked opam exec -- dune build @examples @doc
}

check_no_network() {
  run_checked opam install --yes --with-test --with-doc --with-dev-setup --deps-only .
  report_no_network
  finish_report
}

check_minio() {
  run_checked opam install --yes --with-test --deps-only $AWSKIT_MINIO_OPAM_PACKAGES
  start_report
  run_minio no
  finish_report
}

check_stress() {
  run_checked opam install --yes --with-test --with-doc --with-dev-setup --deps-only .
  if [ -n "$AWSKIT_EXAMPLE_OPAM_PACKAGES" ]; then
    run_checked opam install --yes $AWSKIT_EXAMPLE_OPAM_PACKAGES
  fi
  report_stress
  finish_report
}

check_package_isolation() {
  check_release_package_count

  isolation_compiler=${AWSKIT_OPAM_ISOLATION_COMPILER_PACKAGE:-}
  if [ -z "$isolation_compiler" ]; then
    isolation_compiler="ocaml-base-compiler.$(opam exec -- ocamlc -version)"
  fi
  isolation_prefix=${AWSKIT_OPAM_ISOLATION_SWITCH_PREFIX:-awskit-release-isolation}
  isolation_switch="$isolation_prefix-$$"

  echo "Creating package isolation switch $isolation_switch with $isolation_compiler"
  run_checked opam switch create --yes --no-switch "$isolation_switch" "$isolation_compiler"
  for pinned_package in $(release_packages); do
    run_checked opam pin add --switch="$isolation_switch" --yes --no-action \
      --kind=path "$pinned_package" .
  done

  isolation_base_packages=$(
    OPAMCOLOR=never opam list --switch="$isolation_switch" --installed --short
  )

  cleanup_isolation_packages() {
    installed_packages=$(
      OPAMCOLOR=never opam list --switch="$isolation_switch" --installed --short
    )
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
      run_checked opam remove --switch="$isolation_switch" --yes --auto-remove \
        $remove_packages
    fi
  }

  for package in $(release_packages); do
    cleanup_isolation_packages
    echo "Checking isolated opam metadata for $package with $isolation_compiler"
    run_checked opam install --switch="$isolation_switch" --yes --deps-only \
      --with-test "$package"
    run_checked opam exec --switch="$isolation_switch" -- dune build -p "$package" \
      @install @runtest
  done

  cleanup_isolation_packages
  cleanup_isolation_switch
}

check_current_doc_warnings() {
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
}

check_release_archive() {
  awskit_require_release_version
  release_opam_switch=$(opam switch show)

  check_clean_worktree
  check_current_doc_warnings
  run_checked opam exec -- dune-release check -V "$AWSKIT_RELEASE_VERSION"
  run_checked opam exec -- dune-release distrib -V "$AWSKIT_RELEASE_VERSION"

  dist_archive="_build/awskit-$AWSKIT_RELEASE_VERSION.tbz"
  dist_dir=$(mktemp -d "${TMPDIR:-/tmp}/awskit-dist-doc.XXXXXX")
  dist_log=$(mktemp "${TMPDIR:-/tmp}/awskit-dist-doc.XXXXXX")
  tar -xjf "$dist_archive" -C "$dist_dir"
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
}

check_release() {
  check_package_metadata
  check_package_isolation
  check_docs_examples
  check_release_archive

  start_report
  report_no_network
  run_checked opam install --yes --with-test --deps-only $AWSKIT_MINIO_OPAM_PACKAGES
  run_minio no
  finish_report
}

cleanup() {
  stop_minio
  cleanup_isolation_switch
}

trap cleanup EXIT
trap 'cleanup; exit 130' INT
trap 'cleanup; exit 143' TERM

report_stress() {
  start_report
  dune_build_force_with_qcheck_count "$stress_check_qcheck_count" \
    "AWSKIT_QCHECK_COUNT=$stress_check_qcheck_count opam exec -- dune build --force @check-stress" \
    @check-stress
  report_docs_examples
  run_minio yes
}

case "$check" in
  package)
    check_package
    ;;
  package-metadata)
    check_package_metadata
    ;;
  package-isolation)
    check_package_isolation
    ;;
  docs-examples)
    check_docs_examples
    ;;
  no-network)
    check_no_network
    ;;
  minio)
    check_minio
    ;;
  stress)
    check_stress
    ;;
  release-archive)
    check_release_archive
    ;;
  release)
    check_release
    ;;
esac

exit 0
