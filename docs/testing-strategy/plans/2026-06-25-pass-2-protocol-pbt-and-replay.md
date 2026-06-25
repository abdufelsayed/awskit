# Protocol PBT And Replay Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Expand deterministic no-network protocol evidence for parsers,
validators, canonicalization, endpoint rejection, XML decoding, and minimized
replay cases.

**Architecture:** Place properties beside the package they exercise:
`test/awskit` for signing and core request behavior, `test/awskit-s3` for S3
domain and XML behavior, and `test/awskit-s3/protocol` for protocol fixtures,
range parsing, endpoint rejection, and fuzz replay.

**Tech Stack:** OCaml, Dune aliases, Alcotest, QCheck, Awskit test harnesses,
protocol fixtures, fuzz-replay sidecars.

---

## Related Draft

For deeper rationale, target areas, replay-corpus rules, and fuzzing discipline,
read:

- `docs/testing-strategy/drafts/2026-06-25-protocol-fuzz-replay.md`

## No-Fix Rule

This is a test-construction pass. Do not edit production protocol
implementations. If a property exposes a parser, signing, endpoint, or decoder
bug, keep the test and record the failure.

Read-only production files for this pass:

- `packages/awskit/request.ml`
- `packages/awskit/signing.ml`
- `packages/awskit/endpoint.ml`
- `packages/awskit-s3/response.ml`
- `packages/awskit-s3/range.ml`
- `packages/awskit-s3/s3_metadata_headers.ml`
- `packages/awskit-s3/metadata.ml`
- `packages/awskit-s3/*_xml.ml`
- `packages/awskit-s3/s3_parse.ml`
- `packages/awskit-s3/s3_error.ml`

## Files

- Modify `test/awskit/test_signing.ml`
- Modify `test/awskit/dune` only when adding a new suite group
- Modify `test/awskit-s3/protocol/test_protocol_pbt.ml`
- Modify `test/awskit-s3/test_paginator.ml`
- Modify `test/awskit-s3/test_bucket_xml.ml`
- Modify `test/awskit-s3/dune` only when adding a new suite group
- Modify `test/awskit-s3/protocol/test_fuzz_replay.ml` only for committed
  minimized replay cases
- Add files under `test/awskit-s3/fixtures/protocol/fuzz-replay/**` only for
  deterministic minimized failures

## Task 1: Content-Range Properties

- [ ] Add properties in `test/awskit-s3/protocol/test_protocol_pbt.ml`.

- [ ] Generate valid `Range.Content_range.t` values where:

  - `start >= 0`
  - `finish >= start`
  - `complete_length = None` or `complete_length > finish`

- [ ] Assert valid round-trips:

  ```ocaml
  value |> Range.Content_range.to_header |> Range.Content_range.of_header
  ```

  Expected: `Ok value`.

- [ ] Generate invalid headers for:

  - Missing `bytes` range-unit.
  - Non-decimal start.
  - Non-decimal finish.
  - Non-decimal complete length.
  - `finish < start`.
  - `finish >= complete_length`.
  - Extra `/` or `-` separators.

- [ ] Assert invalid headers return decode errors through public error
  classification, not exact human printer text.

## Task 2: Endpoint Rejection Properties

- [ ] Expand `prop_endpoint_rejects_url_parts` in
  `test/awskit-s3/protocol/test_protocol_pbt.ml`.

- [ ] Generate endpoint strings with:

  - URL fragments.
  - Bad ports.
  - Empty hosts.
  - Malformed IPv6 brackets.
  - Unsupported schemes.
  - Userinfo.
  - Paths.
  - Queries.
  - Control characters.

- [ ] Assert `Awskit.Endpoint.of_string` returns `Error _` for each generated
  rejected endpoint.

## Task 3: Canonical Query Properties

- [ ] Expand canonical query properties in `test/awskit/test_signing.ml`.

- [ ] Generate query parameters with:

  - Duplicate names.
  - Empty value lists.
  - Empty string values.
  - Percent-looking values such as `%2F` and `%zz`.
  - Slash values.
  - Spaces and tabs.

- [ ] Assert `Awskit.Signing.canonical_query_params` preserves the expanded
  pair count.

- [ ] Assert the output is sorted by encoded key, then encoded value.

- [ ] Assert slash and percent characters are encoded according to
  `Awskit.Signing.uri_encode`.

## Task 4: Header Validation And Signing Properties

- [ ] Add generated header validation cases in `test/awskit/test_signing.ml`.

- [ ] Cover duplicate header names, uppercase and lowercase header names,
  leading or trailing whitespace, newline, and carriage return.

- [ ] Assert newline and carriage-return values are rejected through public
  request or signing APIs.

- [ ] Assert accepted whitespace cases produce deterministic signatures when
  the normalized header set is equivalent.

## Task 5: ListObjectsV2 XML Decoder Properties

- [ ] Add generated decoder coverage in `test/awskit-s3/test_paginator.ml`.

- [ ] Reach decoder behavior through `Recording_s3.Object.List` public or test
  harness APIs.

- [ ] Generate small ListObjectsV2 XML documents with:

  - Zero to five keys.
  - Optional `IsTruncated`.
  - Optional `NextContinuationToken`.
  - Unknown child elements.

- [ ] Assert unknown elements are tolerated when known required fields remain
  valid.

- [ ] Generate malformed known fields such as non-boolean `IsTruncated` and
  assert decode errors.

## Task 6: Tagging XML Decoder Properties

- [ ] Add generated Tagging XML decoder coverage in
  `test/awskit-s3/test_bucket_xml.ml`.

- [ ] Generate zero to ten valid tags with short printable keys and values.

- [ ] Assert valid generated XML decodes through public or test harness APIs.

- [ ] Generate malformed known fields such as empty tag keys and missing
  `Value` elements where the supported contract rejects them.

- [ ] Assert malformed known fields return decode or validation errors through
  structured classification.

## Task 7: Replay Corpus Discipline

- [ ] Add new fuzz replay fixtures only when a failure has already been
  minimized.

- [ ] Put endpoint cases under:

  ```text
  test/awskit-s3/fixtures/protocol/fuzz-replay/endpoint/
  ```

- [ ] Put header cases under:

  ```text
  test/awskit-s3/fixtures/protocol/fuzz-replay/headers/
  ```

- [ ] Put XML cases under:

  ```text
  test/awskit-s3/fixtures/protocol/fuzz-replay/xml/
  ```

- [ ] Each input must have a `.expected` sidecar with stable output such as
  error kind, field, service status, retry class, or normalized success summary.

- [ ] Do not commit large random corpora, provider dumps, secrets, live AWS
  responses, or MinIO-only quirks.

## Task 8: Validation

- [ ] Run property evidence:

  ```sh
  opam exec -- dune build @protocol-pbt
  ```

- [ ] Run fixture evidence:

  ```sh
  opam exec -- dune build @protocol-fixtures
  ```

- [ ] Run replay evidence:

  ```sh
  opam exec -- dune build @fuzz-replay
  ```

- [ ] Run whitespace validation:

  ```sh
  git diff --check
  ```

- [ ] If a property or replay case fails because of product behavior, write
  failure evidence and do not edit production code.

## Backlog

- Golden signing fixtures for valid canonicalization bugs.
- Replay families for reduced response classification failures.
- Generated pagination summaries for more ListObjectsV2 token combinations.
- Offline mutation fuzzing outside default Dune aliases.
