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
  AWSKIT_QCHECK_COUNT       Overrides generated workload counts.
  AWSKIT_STRESS_QCHECK_COUNT
                            Default count used by the stress workflow when
                            AWSKIT_QCHECK_COUNT is unset. Defaults to 2000.
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

case "$workflow" in
  stress)
    if [ -z "${AWSKIT_QCHECK_COUNT:-}" ]; then
      AWSKIT_QCHECK_COUNT=${AWSKIT_STRESS_QCHECK_COUNT:-2000}
      export AWSKIT_QCHECK_COUNT
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

run_cmd() {
  description=$1
  shift
  tmp="$report.tmp.$$"

  append_line ""
  append_line "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] START $description"
  print_command "$@" >>"$report"

  "$@" >"$tmp" 2>&1
  cmd_status=$?
  append_file "$tmp"
  rm -f "$tmp"

  append_line "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] EXIT $description status=$cmd_status"
  if [ "$cmd_status" -ne 0 ]; then
    record_failure
  fi

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
    run_cmd "docker compose logs" docker compose logs
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

  if ! dune_build_force "opam exec -- dune build --force @check-integration" @check-integration; then
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

  if ! run_cmd \
    "AWSKIT_INTEGRATION_PROFILE=expensive opam exec -- dune build --force @s3-minio" \
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
  dune_build_force "opam exec -- dune build --force @check-stress" @check-stress
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

ln -sf "$(basename "$report")" "$latest"
append_line "latest_report=$(display_path "$latest")"

if [ "$failed" -ne 0 ]; then
  exit 1
fi

exit 0
