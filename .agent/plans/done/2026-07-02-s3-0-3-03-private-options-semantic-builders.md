# S3 0.3 Private Options And Semantic Builders

Status: done
Issue: none
Goal: Make non-presigned S3 request option records private and replace record-update bypasses with semantic builders/updaters.
Risk: high
Verification: dune build @packages/awskit-s3/all plus the no-match record-update audit below

## Context

Depends on: plans 01 and 02.

OCaml private records in `.mli` files block construction and record update
outside the defining module. Sibling modules and Dune-private simulator modules
still compile against the public `.cmi`, so they need semantic APIs too.

This plan covers object, bucket, and multipart option records. Presigned option
records are handled in plan 04.

## Files Likely To Change

- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/object.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/object.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/bucket.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/bucket.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/multipart.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/multipart.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/object_request.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/multipart_request.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/eio/transfer.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/lwt/unix/transfer.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/sim/**/*.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/**`

## Acceptance Criteria

- Public `Object.*.options`, `Bucket.*.options`, and `Multipart.*.options`
  records are `private` in `.mli` files.
- Result records remain constructible/readable where appropriate; do not make
  response/result records private as part of this work.
- No package, simulator, test, or example code updates request option records
  outside their defining module.
- No package, simulator, test, or example code constructs request option
  records directly outside their defining module.
- Pagination uses public-worthy operation helpers such as
  `Object.List.next_page_options`, `Object.Versions.next_page_options`, and
  `Multipart.List_parts.next_page_options`.
- Transfer code uses a semantic helper for ranged download option adjustment,
  not generic `with_range`/`set_version_id` mutators.
- Transfer code derives `Object.Head.options` from `Object.Get.options`
  through an owning-module helper, not direct record construction.
- The multipart validation test no longer updates
  `Multipart.Complete.default_options` directly.

## Tasks

- [x] Run the current record-update/direct-construction audit:
  ```bash
  rg -n "default_options with|with (range|version_id|preconditions|continuation_token|key_marker|version_id_marker|part_number_marker|multipart_object_size)" packages/awskit-s3 examples test
  rg -n "options_for_page|: .*\\.options = \\{|Head\\.options" packages/awskit-s3/object_request.ml packages/awskit-s3/multipart_request.ml packages/awskit-s3/eio packages/awskit-s3/lwt/unix packages/awskit-s3/sim
  ```
- [x] Add operation-owned pagination helpers in the defining modules:
  - `Object.List` for continuation token/start-after behavior.
  - `Object.Versions` for key/version markers.
  - `Multipart.List_parts` for part-number markers.
- [x] Replace pagination record updates in request modules and simulator
  modules with those helpers.
- [x] Add an owning-module helper for deriving `Object.Head.options` from
  `Object.Get.options`.
- [x] Add a narrow transfer helper in `Object.Get` or a closely related owning
  module for HEAD-derived ranged downloads. It should apply the version/ETag
  stabilization and range in one semantic operation.
- [x] Replace Eio and Lwt transfer record updates with the transfer helper.
- [x] Replace test direct record updates with builders or semantic helpers.
- [x] Change public object, bucket, and multipart request option records to
  `type options = private { ... }`.
- [x] Update `.mli` comments to show the intended builder/helper path for each
  private option record.

## Completion Notes

- Added semantic pagination helpers on `Object.List`, `Object.Versions`, and
  `Multipart.List_parts`.
- Added `Object.Head.of_get_options` and
  `Object.Get.ranged_download_options` for transfer option derivation.
- Made non-presigned object, bucket, and multipart request option records
  private in public interfaces.
- Replaced non-presigned request-option record updates in request modules,
  simulator pagination, transfer adapters, and multipart validation coverage.
- Verified with `dune build @packages/awskit-s3/all`,
  `dune build @test/awskit-s3/runtest`, `dune build @fmt`, and the planned
  grep audits.

## Exact Verification Commands

```bash
dune build @packages/awskit-s3/all
```

```bash
dune build @test/awskit-s3/runtest
```

```bash
rg -n "type options = \\{" packages/awskit-s3/object.mli packages/awskit-s3/bucket.mli packages/awskit-s3/multipart.mli
```

Expected signal: no non-private request option declarations.

```bash
rg -n "default_options with|with (range|version_id|preconditions|continuation_token|key_marker|version_id_marker|part_number_marker|multipart_object_size)" packages/awskit-s3 examples test
```

Expected signal: no non-presigned request-option record update bypasses.
Presigned matches are allowed here only when clearly deferred to plan 04.
Explain any other match that is not an operation option record.

```bash
rg -n "options_for_page|: .*\\.options = \\{|Head\\.options" packages/awskit-s3/object_request.ml packages/awskit-s3/multipart_request.ml packages/awskit-s3/eio packages/awskit-s3/lwt/unix packages/awskit-s3/sim
```

Expected signal: no local pagination helper bypasses or direct request-option
record construction outside defining modules.

## Rollback Notes

- If a helper would be accidental public API, do not publish it. Redesign the
  operation-specific builder or temporarily leave that one record non-private
  until a stable public helper exists.
- Do not use generic public mutators just to recover record update syntax.
