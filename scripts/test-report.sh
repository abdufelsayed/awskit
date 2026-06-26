#!/bin/sh

set -u

usage() {
  cat <<'USAGE'
Usage: scripts/test-report.sh [workflow] [--log-dir DIR] [--label LABEL]

Workflows:
  quick        Format, whitespace, and no-network correctness.
  integration  Bounded MinIO integration with service lifecycle.
  full         Broad workflow plus bounded MinIO integration.
  stress       Full workflow with higher generated workload pressure.

Options:
  --log-dir DIR  Write reports under DIR instead of .logs.
  --label LABEL  Use LABEL in the report filename.
  -h, --help     Show this help.

Environment:
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

workflow=quick
log_dir=.logs
label=

while [ "$#" -gt 0 ]; do
  case "$1" in
    quick | integration | full | stress)
      workflow=$1
      shift
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

if ! root=$(git rev-parse --show-toplevel 2>/dev/null); then
  echo "scripts/test-report.sh must run inside a git checkout." >&2
  exit 2
fi

cd "$root"

stress_check_qcheck_count=
case "$workflow" in
  stress)
    if [ -n "${AWSKIT_QCHECK_COUNT:-}" ]; then
      stress_check_qcheck_count=$AWSKIT_QCHECK_COUNT
    else
      stress_check_qcheck_count=${AWSKIT_STRESS_QCHECK_COUNT:-2000}
    fi
    ;;
esac

mkdir -p "$log_dir"

slug_source=${label:-$workflow}
slug=$(printf "%s" "$slug_source" | tr -c "A-Za-z0-9_.-" "-" | sed 's/--*/-/g; s/^-//; s/-$//')
if [ -z "$slug" ]; then
  slug=$workflow
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
    echo "workflow=$workflow"
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

run_format_and_diff() {
  run_cmd "opam exec -- dune fmt --root $root" opam exec -- dune fmt --root "$root"
  run_cmd "git diff --check" git diff --check
}

run_quick() {
  run_format_and_diff
  dune_build_force "opam exec -- dune build --force @check-quick" @check-quick
}

run_docs_examples() {
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

cleanup() {
  stop_minio
}

trap cleanup EXIT
trap 'cleanup; exit 130' INT
trap 'cleanup; exit 143' TERM

run_full_workflow() {
  run_format_and_diff
  dune_build_force "opam exec -- dune build --force @check-quick" @check-quick
  run_docs_examples
  run_minio no
}

run_stress_workflow() {
  run_format_and_diff
  dune_build_force_with_qcheck_count "$stress_check_qcheck_count" \
    "AWSKIT_QCHECK_COUNT=$stress_check_qcheck_count opam exec -- dune build --force @check-stress" \
    @check-stress
  run_docs_examples
  run_minio yes
}

run_metadata

case "$workflow" in
  quick)
    run_quick
    ;;
  integration)
    run_minio no
    ;;
  full)
    run_full_workflow
    ;;
  stress)
    run_stress_workflow
    ;;
esac

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

exit 0
