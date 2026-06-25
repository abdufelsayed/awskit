# Tests

This tree is being rebuilt around workload-based correctness checks.

Implementation passes should add focused private test libraries, workload
models, and package-owned runners under this directory. Use concise names that
describe behavior, such as `runtime_http_workload`, `s3_model`,
`s3_workload`, `protocol_wire`, and `transfer_fault_workload`.

Runtime HTTP adapter work runs through package-owned aliases:

- `@test/awskit/eio/runtime-http-workload`
- `@test/awskit/lwt/runtime-http-workload`
- `@runtime-http-workload`

The suite IDs are `workload:awskit-eio:runtime-http` and
`workload:awskit-lwt:runtime-http`.

S3 workload support lives in `test/awskit-s3/support`. The private
`awskit_s3_workload` library provides reusable command generation, pure model
state, replay text helpers, and a target functor for package-owned runners.

The simulator runner lives in `test/awskit-s3/sim` and runs the shared S3 state
workload without network access:

- `@test/awskit-s3/sim/s3-sim-workload`
- `@s3-sim-workload`

The suite ID is `workload:awskit-s3-sim:s3-state`.

During the migration, the previous test tree may exist locally as `test.o/`.
That directory is historical reference material only. Do not stage it, do not
add ignore rules for it, and do not use it as replacement evidence for tracked
tests.

Test-construction passes may expose product bugs. Record the focused failing
alias and reduced input, but keep production fixes in separate fix passes.
