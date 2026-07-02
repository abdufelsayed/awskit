# S3 0.3 Public API Clean Break

Status: done
Issue: none
Goal: Remove old public S3 request API names and settle typed public upload-constructor exceptions.
Risk: high
Verification: dune build @packages/awskit-s3/all plus the no-match audits below

## Context

Depends on: current dirty branch baseline only.

This plan makes the public API internally consistent for `0.3.0`. It does not
preserve old names. Any broken implementation, simulator, example, or test call
site must move to the new API.

## Files Likely To Change

- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/object.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/object.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/object_request.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/object_delete_xml.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/awskit_s3.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/awskit_s3.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/sim/**/*.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/sim/**/*.mli`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/**`
- `/Users/abdllahdev/dev/awskit/examples/**`

## Acceptance Criteria

- `Object.Delete_many` is removed from public interfaces.
- The replacement module is `Object.Delete_objects`.
- Runtime/root operation function names remain `delete_objects`.
- No compatibility alias or deprecated public `Delete_many` bridge exists.
- `Object.Delete_objects.object_` has `object_exn` if `object_` remains
  result-returning.
- Public S3 `.mli` files have no typed request inputs owned by this plan.
  Endpoint/region typed seams are handled in plan 02, and presigned typed seams
  are handled in plan 04.
- `Multipart.Upload.created` is either hidden, made string-facing, or
  documented as a deliberate simulator/runtime support constructor.

## Tasks

- [x] Audit the current public typed request surface and record which matches
  are owned by later plans:
  ```bash
  rg -n "(bucket|key|upload_id|version_id|expected_bucket_owner|content_type|content_disposition|content_encoding|cache_control|region|endpoint|signing_region):(Bucket_name|Object_key|Upload_id|Object\\.Version_id|Version_id|Account_id|Content_type|Header_value|Awskit\\.Region|Awskit\\.Endpoint)" packages/awskit-s3 --glob '*.mli'
  ```
- [x] Rename `Object.Delete_many` to `Object.Delete_objects` in `object.ml`
  and `object.mli`.
- [x] Update `object_request.ml`, `object_delete_xml.ml`, simulator modules,
  root APIs, examples, and tests to use `Object.Delete_objects`.
- [x] Add `Object.Delete_objects.object_exn` if `object_` continues to return
  `(object_, Awskit.Error.t) result`.
- [x] Remove all `Delete_many` compatibility aliases. Do not add deprecations.
- [x] Decide `Multipart.Upload.created`:
  - hide it from the public interface, or
  - make it string-facing, or
  - document it as an intentional runtime/simulator typed support API.
- [x] Re-run the typed public request-input audit and explain any remaining
  matches as either fixed here, owned by plan 02, owned by plan 04, or a
  deliberate non-request-input exception.

## Completion Notes

- `Delete_many` has no remaining code, example, or test matches.
- `Multipart.Upload.created` is now string-facing and result-returning, with
  `created_exn` for trusted internal/test bridges.
- Remaining typed public request matches are owned by plan 02
  (`Endpoint_config`, `Bucket.Create`, `Endpoint_resolver`) and plan 04
  (`Presigned.*_with_endpoint_config`).
- Verification run:
  - `dune build @packages/awskit-s3/all`
  - `dune build @fmt`
  - `dune build @test/awskit-s3/runtest`

## Exact Verification Commands

```bash
dune build @packages/awskit-s3/all
```

```bash
rg -n "Object\\.Delete_many|module Delete_many|Delete_many\\." packages/awskit-s3 examples test
```

Expected signal: no matches, unless `CHANGES.md` is being updated in plan 07.

```bash
rg -n "(bucket|key|upload_id|version_id|expected_bucket_owner|content_type|content_disposition|content_encoding|cache_control|region|endpoint|signing_region):(Bucket_name|Object_key|Upload_id|Object\\.Version_id|Version_id|Account_id|Content_type|Header_value|Awskit\\.Region|Awskit\\.Endpoint)" packages/awskit-s3 --glob '*.mli'
```

Expected signal: only documented non-request-input exceptions or matches owned
by plans 02 and 04.

## Rollback Notes

- If the rename is too large for one commit, split by library/runtime/test
  groups. The final release must still contain no public `Delete_many`.
- Do not restore compatibility aliases to make intermediate builds easier.
