# S3 0.3 Public Seam Tests

Status: ready
Issue: none
Goal: Add focused tests proving the new string-facing S3 public API validates early and avoids transport on invalid input.
Risk: high
Verification: dune build @test/awskit-s3/runtest && dune build @correctness

## Context

Depends on: plans 01 through 04.

Existing domain tests prove validators work, but the breaking release also
needs evidence that public operations and builders call those validators at the
right seam.

## Files Likely To Change

- `/Users/abdllahdev/dev/awskit/test/awskit-s3/protocol/test_protocol_pbt.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/protocol/test_protocol_fixtures.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/protocol/test_domain_pbt.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/sim/**/*.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/lwt/unix/test_s3_minio_workload.ml`
- `/Users/abdllahdev/dev/awskit/test/support/**`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/sim/**/*.ml`

## Acceptance Criteria

- Invalid public `bucket:string` and `key:string` operation calls return
  structured validation errors before transport.
- Invalid upload ids, version ids, expected owners, content types/header
  values, part numbers, endpoints, regions, and signing regions are covered at
  their public seams.
- Listing and pagination request inputs are covered at their public seams:
  prefixes, delimiters, max key/page bounds, key markers, version-id markers,
  start-after keys, continuation tokens, max part counts, and part-number
  markers.
- Public object-copy bucket/key inputs are covered, including invalid source
  and destination names that should fail before transport.
- Recording-runtime or simulator tests prove invalid operation input sends zero
  requests.
- Presigned builder tests cover success, invalid string inputs, invalid header
  names/values, duplicate header names, and case-duplicate header names.
- Endpoint-config tests cover valid HTTPS, valid local plaintext, invalid
  endpoint strings, invalid signing regions, and `unsafe_plaintext` result
  behavior.
- Simulator, Lwt, and Eio-facing entrypoints have smoke or validation coverage
  for the migrated string APIs.
- Domain validator tests remain; do not remove them just because callers mostly
  use strings now.

## Tasks

- [ ] Add helper assertions for validation error field/path checks.
- [ ] Add or reuse recording-runtime helpers that count attempted requests.
- [ ] Test invalid object operation bucket/key input with zero transport.
- [ ] Test invalid object-copy source/destination bucket and key input with
  zero transport.
- [ ] Test object listing/version-listing builders for invalid pagination,
  delimiter, marker, and bound inputs.
- [ ] Test invalid bucket operation input with zero transport.
- [ ] Test invalid multipart operation input, including part number and upload
  id validation.
- [ ] Test multipart list-parts builders for invalid max-parts and
  part-number-marker inputs.
- [ ] Test `Bucket.Create.options ?region:string` success/failure.
- [ ] Test endpoint-config string constructors success/failure.
- [ ] Test presigned builders for content type, response overrides, owners,
  version ids, and extra signed headers.
- [ ] Update simulator and MinIO workload tests to use `Object.Delete_objects`
  and all new builders.
- [ ] Add Eio/Lwt smoke coverage where examples are currently the only proof.

## Exact Verification Commands

```bash
dune build @test/awskit-s3/runtest
```

```bash
dune build @s3-domain-laws @s3-protocol-laws @s3-protocol-fixtures @s3-protocol-replay
```

```bash
dune build @s3-simulator @s3-transfer-faults
```

```bash
dune build @correctness
```

If local MinIO is available:

```bash
scripts/test.sh integration --label s3-0-3-public-seam
```

## Rollback Notes

- If a broad alias fails, reduce to the focused test that proves the changed
  seam first, then restore the broader gate before release readiness.
- Do not weaken validation to satisfy old tests; update tests to the new API.
