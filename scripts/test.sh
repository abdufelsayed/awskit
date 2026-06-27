#!/bin/sh

set -u

usage() {
  cat <<'USAGE'
Usage: scripts/test.sh SUITE [--log-dir DIR] [--label LABEL]

Suites:
  quick        Run @correctness tests.
  integration  Run @integration tests with script-managed MinIO.
  stress       Run @stress tests.

Options:
  --log-dir DIR  Write logs under DIR instead of .logs.
  --label LABEL  Use LABEL in the log filename.
  -h, --help     Show this help.
USAGE
}

suite=
log_dir=.logs
label=

while [ "$#" -gt 0 ]; do
  case "$1" in
    quick | integration | stress)
      if [ -n "$suite" ]; then
        echo "Only one SUITE may be provided." >&2
        exit 2
      fi
      suite=$1
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

if [ -z "$suite" ]; then
  echo "Missing SUITE." >&2
  usage >&2
  exit 2
fi

if ! root=$(git rev-parse --show-toplevel 2>/dev/null); then
  echo "scripts/test.sh must run inside a git checkout." >&2
  exit 2
fi

cd "$root"

case "$suite" in
  quick)
    dune_alias=correctness
    needs_minio=0
    ;;
  integration)
    dune_alias=integration
    needs_minio=1
    ;;
  stress)
    dune_alias=stress
    needs_minio=0
    ;;
esac

slug_source=${label:-$suite}
slug=$(printf "%s" "$slug_source" | tr -c "A-Za-z0-9_.-" "-" | sed 's/--*/-/g; s/^-//; s/-$//')
if [ -z "$slug" ]; then
  slug=$suite
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
  print_command "$@" | tee -a "$report"
  : >"$marker"
  "$@" >"$tmp" 2>&1
  status=$?
  append_file "$tmp"
  rm -f "$tmp"
  append_line "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] EXIT $description status=$status"

  if [ "$status" -ne 0 ]; then
    append_recent_alcotest_outputs "$marker"
    record_failure
  fi
  rm -f "$marker"
  return "$status"
}

write_metadata() {
  mkdir -p "$log_dir"
  {
    echo "# Awskit test report"
    echo "started_utc=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "suite=$suite"
    echo "alias=@$dune_alias"
    echo "root=$root"
    echo "branch=$(git branch --show-current 2>/dev/null || true)"
    echo "head=$(git rev-parse --short HEAD 2>/dev/null || true)"
    echo "log_path=$(display_path "$report")"
    echo "AWSKIT_QCHECK_COUNT=${AWSKIT_QCHECK_COUNT:-}"
    echo "QCHECK_SEED=${QCHECK_SEED:-}"
    echo "AWSKIT_INTEGRATION_PROFILE=${AWSKIT_INTEGRATION_PROFILE:-}"
  } | tee -a "$report"

  run_cmd "git status --short --branch" git status --short --branch
  run_cmd "opam exec -- dune --version" opam exec -- dune --version
  run_cmd "opam exec -- ocamlc -version" opam exec -- ocamlc -version
}

configure_minio() {
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
}

compose_project_has_containers() {
  compose_container_ids=$(docker compose ps -a -q 2>/dev/null || true)
  [ -n "$compose_container_ids" ]
}

dump_minio_logs() {
  run_cmd "docker compose ps -a" docker compose ps -a
  run_cmd "docker compose logs minio" docker compose logs minio
  run_cmd "docker compose logs minio-setup" docker compose logs minio-setup
}

start_minio() {
  if ! command -v docker >/dev/null 2>&1; then
    append_line ""
    append_line "docker CLI not found; MinIO-backed suite was not run."
    record_failure
    return 1
  fi

  if compose_project_has_containers; then
    append_line ""
    append_line "Existing Docker Compose resources for this checkout are already present."
    append_line "Refusing to share a script-managed MinIO lifecycle; run docker compose down -v or use a separate checkout."
    dump_minio_logs
    record_failure
    return 1
  fi

  minio_started=1
  if ! run_cmd "docker compose up -d" docker compose up -d; then
    minio_failed=1
    append_line "docker compose up failed; MinIO-backed suite was not run."
    dump_minio_logs
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

finish() {
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

cleanup() {
  stop_minio
}

trap cleanup EXIT
trap 'cleanup; exit 130' INT
trap 'cleanup; exit 143' TERM

write_metadata

if [ "$needs_minio" = "1" ]; then
  if start_minio; then
    configure_minio
    if ! run_cmd "opam exec -- dune build --force @$dune_alias" \
      opam exec -- dune build --root "$root" --force "@$dune_alias"
    then
      minio_failed=1
    fi
    if [ "$minio_failed" = "1" ]; then
      dump_minio_logs
    fi
  fi
else
  run_cmd "opam exec -- dune build --force @$dune_alias" \
    opam exec -- dune build --root "$root" --force "@$dune_alias"
fi

finish
