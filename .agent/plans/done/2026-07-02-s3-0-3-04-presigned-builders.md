# S3 0.3 Presigned Builders

Status: done
Issue: none
Goal: Make presigned request options builder-only/private and string-facing, including endpoint-config presign functions.
Risk: high
Verification: dune build @packages/awskit-s3/all && dune build @s3-protocol-laws @s3-protocol-fixtures

## Context

Depends on: plans 01 through 03.

Presigned URLs are security-sensitive bearer artifacts. Invalid signed headers,
response overrides, owners, content types, or version ids should fail at option
construction where possible, before URL generation.

## Files Likely To Change

- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/presigned.mli`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/presigned.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/presigned_request.ml`
- `/Users/abdllahdev/dev/awskit/packages/awskit-s3/sim/simulator_presigned.ml`
- `/Users/abdllahdev/dev/awskit/examples/eio/presign.ml`
- `/Users/abdllahdev/dev/awskit/examples/lwt/presign.ml`
- `/Users/abdllahdev/dev/awskit/examples/sim/presign_summary.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/protocol/test_protocol_pbt.ml`
- `/Users/abdllahdev/dev/awskit/test/awskit-s3/protocol/test_protocol_fixtures.ml`

## Acceptance Criteria

- `Presigned.Put_object`, `Get_object`, `Head_object`, `Upload_part`, and
  `Delete_object` each expose `options` and `options_exn`.
- Presigned option records are private in `presigned.mli`.
- Public builder inputs are string-facing for content types, response override
  headers, version ids, expected owners, and extra signed headers.
- `?expires_in:Ptime.Span.t` remains typed because it is not a raw string
  domain value.
- Invalid `expires_in` bounds fail in `options` before credentials, endpoint
  resolution, or URL generation.
- Existing typed security/domain values that are not raw request strings remain
  typed, such as checksums and encryption/customer-key values.
- `extra_signed_headers` are validated in builders for invalid names/values and
  duplicate or case-duplicate names.
- Duplicate signed headers are rejected against the complete signed-header set,
  including `host`, operation headers, content type, owner, checksum, and
  encryption headers.
- `Presigned.*_with_endpoint_config` functions accept `region:string`.
- `Presigned.endpoint_config` and endpoint-config aliases use
  `Endpoint_config.t`, not `Endpoint_resolver.t`, as the user-facing type.
- Presign examples and tests no longer use `{ default_options with ... }`.

## Tasks

- [x] Add `options` and `options_exn` to every presigned operation module.
- [x] Use existing content/header/account/version validation helpers inside
  the builders.
- [x] Validate `expires_in` in builders.
- [x] Move `extra_signed_headers` validation to builder time while keeping URL
  generation defensive; validate duplicates against all generated signed
  headers, not only within the extra header list.
- [x] Change `_with_endpoint_config` region arguments from `Awskit.Region.t` to
  `string`; parse at that public boundary.
- [x] Update simulator presign wrappers that currently pass typed runtime
  regions into `_with_endpoint_config`.
- [x] Keep a private typed helper only inside `presigned.ml` if the
  implementation benefits from it.
- [x] Make presigned option records private after builders exist.
- [x] Update presign examples, fixtures, and property tests to use the builders.
- [x] Add or update tests that assert invalid presigned options fail before URL
  generation.

## Exact Verification Commands

```bash
dune build @packages/awskit-s3/all
```

```bash
dune build @s3-protocol-laws @s3-protocol-fixtures
```

```bash
rg -n "Presigned\\..*default_options with|default_options with" examples test/awskit-s3/protocol packages/awskit-s3/presigned.mli
```

Expected signal: no presigned default-options record updates.

```bash
rg -n "region:Awskit\\.Region\\.t|type endpoint_config = Endpoint_resolver\\.t|type options = \\{" packages/awskit-s3/presigned.mli
```

Expected signal: no typed region, resolver alias, or non-private presigned
option records.

```bash
rg -n "Content_type\\.t|Header_value\\.t|Object\\.Version_id\\.t|Account_id\\.t" packages/awskit-s3/presigned.mli
```

Expected signal: no public raw request-string inputs exposed as typed
presigned option fields or builder arguments. Remaining typed security values
such as checksum and encryption/customer-key values must be justified.

## Rollback Notes

- Do not leave presigned records concrete just to keep examples compiling.
- Do not defer header validation to URL generation if the builder has enough
  information to reject the input.

## Completion Notes

- Added builder-only/private presigned options with string-facing public inputs
  for raw request string domains.
- Moved `expires_in`, extra signed header, and duplicate complete signed-header
  validation into builders while keeping URL generation defensive.
- Changed public `_with_endpoint_config` region inputs to `string` with private
  typed helpers inside `presigned.ml`.
- Updated examples, simulator/runtime wrappers, fixtures, and protocol
  properties to use builders.
- Verified with `dune build @packages/awskit-s3/all`,
  `dune build @s3-protocol-laws @s3-protocol-fixtures`, `dune build @fmt`, and
  the required `rg` checks.
