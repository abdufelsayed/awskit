set -eu

awskit_manifest=$1
awskit_lwt_manifest=$2
awskit_eio_manifest=$3
awskit_s3_manifest=$4

manifest_path() {
  path=$1
  if [ -f "$path" ]; then
    return 0
  else
    printf 'install manifest not found: %s\n' "$path" >&2
    exit 1
  fi
}

install_root() {
  manifest=$1
  entry=$(sed -n 's/^[[:space:]]*"\([^"]*\/lib\/[^\"]*\)".*/\1/p' "$manifest" | head -n 1)
  if [ -z "$entry" ]; then
    printf 'install manifest has no library files: %s\n' "$manifest" >&2
    exit 1
  fi
  if [ ! -e "$entry" ]; then
    manifest_dir=$(dirname "$manifest")
    if [ -e "$manifest_dir/$entry" ]; then
      entry="$manifest_dir/$entry"
    elif [ "${entry#_build/}" != "$entry" ] && [ -e "../${entry#_build/}" ]; then
      entry="../${entry#_build/}"
    fi
  fi
  root=$(dirname "$entry")
  if [ "$(basename "$root")" = .private ]; then
    root=$(dirname "$root")
  fi
  realpath "$root"
}

assert_package_name() {
  manifest=$1
  expected=$2
  actual=$(basename "$manifest")
  actual=${actual%.install}
  if [ "$actual" != "$expected" ]; then
    printf 'unexpected installed package name in %s: %s\n' "$manifest" "$actual" >&2
    exit 1
  fi
}

assert_dune_package_name() {
  root=$1
  expected=$2
  package="$root/dune-package"
  actual=$(sed -n 's/^(name \([^)]*\)).*/\1/p' "$package" | head -n 1)
  if [ "$actual" != "$expected" ]; then
    printf 'unexpected dune package name in %s: %s\n' "$package" "$actual" >&2
    exit 1
  fi
}

assert_no_private_interfaces() {
  manifest=$1
  if grep -Eq '/observer\.mli"|/execution/[^" ]*\.mli"|/observation/[^" ]*\.mli"|/observability_core/[^" ]*\.mli"|/observability_core\.mli"' "$manifest"; then
    printf 'private observability interface is installed by %s\n' "$manifest" >&2
    grep -En '/observer\.mli"|/execution/[^" ]*\.mli"|/observation/[^" ]*\.mli"|/observability_core/[^" ]*\.mli"|/observability_core\.mli"' "$manifest" >&2
    exit 1
  fi
}

assert_private_cmis_are_hidden() {
  manifest=$1
  while IFS= read -r line; do
    case "$line" in
      *awskit__Observability_core.cmi*|*awskit_lwt__Observer.cmi*|*awskit_eio__Observer.cmi*|*awskit_s3__Execution*.cmi*|*awskit_s3__Observation*.cmi*)
        case "$line" in
          *'/.private/'*) ;;
          *)
            printf 'private CMI is installed on a public path by %s:\n%s\n' "$manifest" "$line" >&2
            exit 1
            ;;
        esac
        ;;
    esac
  done <"$manifest"
}

private_path_pattern='Observability_core|Awskit_s3\.(Execution|Observation)|Awskit_lwt\.Observer|Awskit_eio\.Observer|awskit_(s3|lwt|eio)__'

assert_no_private_paths_in_interfaces() {
  root=$1
  for interface in "$root"/*.mli; do
    [ -f "$interface" ] || continue
    if grep -nE "$private_path_pattern" "$interface"; then
      printf 'private path leaked into installed interface: %s\n' "$interface" >&2
      exit 1
    fi
  done
}

assert_no_private_paths_in_odoc() {
  root=$1
  install_prefix=$(dirname "$(dirname "$root")")
  package=$(basename "$root")
  docs="$install_prefix/doc/$package"
  [ -d "$docs" ] || return 0
  while IFS= read -r document; do
    if grep -nE "$private_path_pattern" "$document"; then
      printf 'private path leaked into installed odoc source: %s\n' "$document" >&2
      exit 1
    fi
  done < <(find "$docs" -type f \( -name '*.mld' -o -name '*.html' \) -print)
}

expect_private_module_unbound() {
  root=$1
  module_name=$2
  tmp=$(mktemp -d "${TMPDIR:-/tmp}/awskit-interface.XXXXXX")
  trap 'rm -rf "$tmp"' EXIT HUP INT TERM
  printf 'module Hidden = %s\n' "$module_name" >"$tmp/probe.ml"
  if ocamlc -I "$root" -c -impl "$tmp/probe.ml" -o "$tmp/probe.cmo" \
    2>"$tmp/probe.error"; then
    printf 'private CMI is importable from the public install path: %s\n' "$module_name" >&2
    exit 1
  fi
  if ! grep -q "Unbound module.*$module_name" "$tmp/probe.error"; then
    cat "$tmp/probe.error" >&2
    printf 'private CMI boundary failed for an unexpected reason: %s\n' "$module_name" >&2
    exit 1
  fi
  rm -rf "$tmp"
  trap - EXIT HUP INT TERM
}

manifest_path "$awskit_manifest"
manifest_path "$awskit_lwt_manifest"
manifest_path "$awskit_eio_manifest"
manifest_path "$awskit_s3_manifest"

assert_package_name "$awskit_manifest" awskit
assert_package_name "$awskit_lwt_manifest" awskit-lwt
assert_package_name "$awskit_eio_manifest" awskit-eio
assert_package_name "$awskit_s3_manifest" awskit-s3

for manifest in "$awskit_manifest" "$awskit_lwt_manifest" "$awskit_eio_manifest" "$awskit_s3_manifest"; do
  assert_no_private_interfaces "$manifest"
  assert_private_cmis_are_hidden "$manifest"
done

awskit_root=$(install_root "$awskit_manifest")
awskit_lwt_root=$(install_root "$awskit_lwt_manifest")
awskit_eio_root=$(install_root "$awskit_eio_manifest")
awskit_s3_root=$(install_root "$awskit_s3_manifest")

assert_dune_package_name "$awskit_root" awskit
assert_dune_package_name "$awskit_lwt_root" awskit-lwt
assert_dune_package_name "$awskit_eio_root" awskit-eio
assert_dune_package_name "$awskit_s3_root" awskit-s3

for root in "$awskit_root" "$awskit_lwt_root" "$awskit_eio_root" "$awskit_s3_root"; do
  assert_no_private_paths_in_interfaces "$root"
  assert_no_private_paths_in_odoc "$root"
done

expect_private_module_unbound "$awskit_root" Awskit__Observability_core
expect_private_module_unbound "$awskit_lwt_root" Awskit_lwt__Observer
expect_private_module_unbound "$awskit_eio_root" Awskit_eio__Observer
expect_private_module_unbound "$awskit_s3_root" Awskit_s3__Execution
expect_private_module_unbound "$awskit_s3_root" Awskit_s3__Observation
