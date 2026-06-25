# Test Identity Registry

This directory mirrors package ownership. Keep new tests close to the package,
runtime adapter, or evidence layer they exercise:

- `test/awskit`: runtime-neutral core behavior.
- `test/awskit/eio` and `test/awskit/lwt`: runtime adapter behavior.
- `test/awskit-s3`: runtime-neutral S3 behavior.
- `test/awskit-s3/protocol`: protocol fixtures, protocol PBT, and fuzz replay.
- `test/awskit-s3/sim`: simulator contracts.
- `test/awskit-s3/lwt/unix`: Docker-backed MinIO contract coverage.
- `test/support` and package-local `support`: private test libraries only.

## Naming Layers

Dune aliases are command IDs and stay kebab-case:

- `@check-fast`
- `@check-protocol`
- `@protocol-pbt`
- `@protocol-fixtures`
- `@fuzz-replay`
- `@runtime-conformance`
- `@simulator-contract`
- `@minio-contract`

Alcotest suite IDs are selector IDs and use colon scopes:

```text
<evidence>:<subject>:<area>[:<detail>]
```

Use lowercase ASCII and kebab-case inside each segment.

Evidence segments:

- `unit`: deterministic unit-level behavior.
- `integration`: local process or adapter integration.
- `contract`: reusable behavioral contract suites.
- `pbt`: property-based tests.
- `fixture`: golden fixture comparison.
- `replay`: deterministic replay of reduced fuzz findings.

Subject segments are package, runtime, or backend names such as `awskit`,
`awskit-eio`, `awskit-lwt`, `awskit-s3`, `awskit-s3-sim`, and `minio`.

Examples:

- `pbt:awskit:signing:canonical-query`
- `pbt:awskit-s3:domain:bucket`
- `pbt:awskit-s3:protocol`
- `contract:runtime:conformance`
- `contract:awskit-eio:runtime-http`
- `contract:awskit-lwt:runtime-http`
- `contract:awskit-s3-sim:bucket`
- `contract:minio:bucket`
- `fixture:awskit-s3:protocol`
- `replay:awskit-s3:fuzz`

QCheck property names may be readable sentences because the enclosing Alcotest
suite provides the stable scoped ID.

PBT suites use fresh QCheck seeds by default. Do not add fixed
`Random.State.make` seeds to ordinary checked-in aliases. QCheck prints the
run seed; replay a failure with:

```sh
QCHECK_SEED=<seed> opam exec -- dune build <alias>
```

## Agent Rules

- Add new suite IDs using the colon-scoped form.
- Update any Dune rule that selects a renamed suite ID.
- Keep Dune aliases kebab-case and document new maintained aliases in
  `docs/testing.md`.
- Let QCheck choose fresh seeds by default; use `QCHECK_SEED` only to replay a
  failing case.
- Put shared test helpers in private test libraries; do not expose them through
  production packages.
- When touching a legacy unscoped suite ID, normalize it instead of adding a
  nearby new spelling.
