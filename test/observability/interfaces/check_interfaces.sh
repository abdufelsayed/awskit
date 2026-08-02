set -eu

ocamlfind_bin=$1
positive_source=$2
metric_source=$3
arbitrary_diagnostic_source=$4
sensitive_diagnostic_source=$5
core_private_source=$6
core_private_child_source=$7
s3_execution_source=$8
s3_observation_source=$9
s3_execution_child_source=${10}
s3_observation_child_source=${11}
lwt_private_source=${12}
eio_private_source=${13}
runtime_engine_source=${14}
awskit_lib=${15}
awskit_s3_lib=${16}
awskit_lwt_lib=${17}
awskit_eio_lib=${18}

awskit_lib=$(realpath "$awskit_lib")
awskit_s3_lib=$(realpath "$awskit_s3_lib")
awskit_lwt_lib=$(realpath "$awskit_lwt_lib")
awskit_eio_lib=$(realpath "$awskit_eio_lib")
awskit_include=$(dirname "$awskit_lib")/.awskit.objs/byte
awskit_s3_include=$(dirname "$awskit_s3_lib")/.awskit_s3.objs/byte
awskit_lwt_include=$(dirname "$awskit_lwt_lib")/.awskit_lwt.objs/byte
awskit_eio_include=$(dirname "$awskit_eio_lib")/.awskit_eio.objs/byte

compile() {
  source=$1
  output=$2
  package=$3
  case "$package" in
    awskit)
      includes="-I $awskit_include" ;;
    awskit-s3)
      includes="-I $awskit_s3_include -I $awskit_include" ;;
    awskit-lwt)
      includes="-I $awskit_lwt_include -I $awskit_include" ;;
    awskit-eio)
      includes="-I $awskit_eio_include -I $awskit_include" ;;
    *)
      includes="" ;;
  esac
  # shellcheck disable=SC2086
  "$ocamlfind_bin" ocamlc $includes -package "$package" -c -impl "$source" -o "$output"
}

compile "$positive_source" public_projection.cmo awskit

if compile "$metric_source" metric_diagnostics.cmo awskit 2>metric_diagnostics.error; then
  echo "metric observations unexpectedly exposed diagnostics" >&2
  exit 1
fi

if ! grep -q "Unbound value.*Metric.Observation.diagnostics" metric_diagnostics.error; then
  cat metric_diagnostics.error >&2
  echo "metric boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$arbitrary_diagnostic_source" arbitrary_diagnostic.cmo awskit 2>arbitrary_diagnostic.error; then
  echo "service authors unexpectedly constructed an arbitrary diagnostic" >&2
  exit 1
fi

if ! grep -q "Unbound value.*Diagnostic.public_string" arbitrary_diagnostic.error; then
  cat arbitrary_diagnostic.error >&2
  echo "arbitrary diagnostic boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$sensitive_diagnostic_source" sensitive_diagnostic.cmo awskit 2>sensitive_diagnostic.error; then
  echo "service authors unexpectedly constructed a sensitive diagnostic" >&2
  exit 1
fi

if ! grep -q "Unbound value.*Diagnostic.sensitive_string" sensitive_diagnostic.error; then
  cat sensitive_diagnostic.error >&2
  echo "sensitive diagnostic boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$core_private_source" core_private.cmo awskit 2>core_private.error; then
  echo "core lifecycle internals unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit.Observability_core" core_private.error; then
  cat core_private.error >&2
  echo "core private-module boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$core_private_child_source" core_private_child.cmo awskit 2>core_private_child.error; then
  echo "core observation children unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit.Observability_core" core_private_child.error; then
  cat core_private_child.error >&2
  echo "core private-child boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$s3_execution_source" s3_execution.cmo awskit-s3 2>s3_execution.error; then
  echo "S3 execution internals unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit_s3.Execution" s3_execution.error; then
  cat s3_execution.error >&2
  echo "S3 execution boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$s3_observation_source" s3_observation.cmo awskit-s3 2>s3_observation.error; then
  echo "S3 observation internals unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit_s3.Observation" s3_observation.error; then
  cat s3_observation.error >&2
  echo "S3 observation boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$s3_execution_child_source" s3_execution_child.cmo awskit-s3 2>s3_execution_child.error; then
  echo "S3 execution children unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit_s3.Execution_attempt" s3_execution_child.error; then
  cat s3_execution_child.error >&2
  echo "S3 execution-child boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$s3_observation_child_source" s3_observation_child.cmo awskit-s3 2>s3_observation_child.error; then
  echo "S3 observation children unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit_s3.Observation_attempt" s3_observation_child.error; then
  cat s3_observation_child.error >&2
  echo "S3 observation-child boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$lwt_private_source" lwt_private.cmo awskit-lwt 2>lwt_private.error; then
  echo "Lwt observer internals unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit_lwt.Observer" lwt_private.error; then
  cat lwt_private.error >&2
  echo "Lwt private-module boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$eio_private_source" eio_private.cmo awskit-eio 2>eio_private.error; then
  echo "Eio observer internals unexpectedly crossed the public boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Awskit_eio.Observer" eio_private.error; then
  cat eio_private.error >&2
  echo "Eio private-module boundary failed for an unexpected reason" >&2
  exit 1
fi

if compile "$runtime_engine_source" runtime_engine.cmo awskit 2>runtime_engine.error; then
  echo "runtime engine unexpectedly crossed the sealed boundary" >&2
  exit 1
fi

if ! grep -q "Unbound module.*Engine" runtime_engine.error; then
  cat runtime_engine.error >&2
  echo "runtime engine boundary failed for an unexpected reason" >&2
  exit 1
fi
