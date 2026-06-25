# Protocol Fuzz And Replay Draft

Draft status: reviewed by the original protocol explorer and revised from the
formatting draft.

## Purpose

Build a repeatable protocol evidence layer for Awskit's owned S3 behavior,
turning parser, validator, signing, response, and replayability failures into
deterministic tests. The layer should protect Awskit's SDK contract without
treating MinIO as the source of truth or expanding support to arbitrary
S3-compatible providers.

## Current Gap

Awskit already has protocol PBT, fixtures, and replay aliases. The path for
adding minimized failures exists, but it is minimal: `docs/testing.md` and
`test/awskit-s3/fixtures/protocol/fuzz-replay/README.md` describe minimized
replay cases with `.expected` sidecars.

Current replay coverage is narrow: endpoint validation, header validation, and
one XML tagging malformed case. Existing protocol PBT and fixtures cover useful
ground, but malformed XML families, header boundary values, endpoint rejection,
and canonicalization edge cases can be expanded without network dependencies.

The simulator is available for semantic state modeling, but it is not a wire
authority. MinIO is useful for local S3-compatible contract checks, but protocol
fuzz findings should reduce to Awskit-owned deterministic evidence before they
count.

## Proposed Test Layer

Keep the existing three-part protocol layer:

- Property tests: `@protocol-pbt`.
- Golden fixtures: `@protocol-fixtures`.
- Fuzz replay: `@fuzz-replay`.

Clarify that `@protocol-pbt` is recursive and includes signing PBT under
`test/awskit`, S3 domain/paginator/transfer PBT under `test/awskit-s3`, and
protocol PBT under `test/awskit-s3/protocol`.

Add only reduced, deterministic replay cases to the committed corpus. Offline
fuzzing should remain opt-in and outside default CI.

## Target Areas

Endpoint parsing and resolution:

- `packages/awskit/endpoint.ml`.
- `packages/awskit-s3/endpoint_resolver.ml`.
- Replay cases under `fuzz-replay/endpoint`.

Header validation, response parsing, and canonicalization:

- `packages/awskit/request.ml`.
- `packages/awskit/signing.ml`.
- `packages/awskit-s3/response.ml`.
- `packages/awskit-s3/range.ml`.
- `packages/awskit-s3/s3_metadata_headers.ml`.
- `packages/awskit-s3/metadata.ml`.
- Replay cases under `fuzz-replay/headers` when a reduced failure exists.

XML decode/encode boundaries:

- `packages/awskit-s3/*_xml.ml`.
- `packages/awskit-s3/s3_parse.ml`.
- `packages/awskit-s3/s3_error.ml`.
- Replay cases under `fuzz-replay/xml`.

XML policy:

- Unknown elements should generally be tolerated where AWS compatibility
  requires forward compatibility.
- Malformed known fields should fail as decode errors.
- Public/test APIs such as `Recording_s3` should exercise private parser
  behavior without exposing private modules.

Signing and presign artifacts:

- Canonical query ordering.
- Signed headers.
- Redaction-safe URLs.
- Existing fixtures under `fixtures/protocol/signing` and
  `fixtures/protocol/presign`.

Pagination and response summaries:

- Truncated pages.
- Missing or invalid next tokens.
- Empty prefixes.
- `max_pages`.
- Generated ordered pages.

Response status/body rules are a later, careful target. Socket framing behavior
for `HEAD`, `204`, and `304` belongs in runtime HTTP contract tests. Operation
classification can be tested at the S3 layer when there is a concrete reduced
failure, but it should not be the first protocol replay milestone.

## Files And Aliases

Existing anchors:

- `test/awskit-s3/protocol/dune`.
- `test/awskit-s3/protocol/test_protocol_pbt.ml`.
- `test/awskit-s3/protocol/test_protocol_fixtures.ml`.
- `test/awskit-s3/protocol/test_fuzz_replay.ml`.
- `test/awskit-s3/protocol/fixture_diff.ml`.
- `test/awskit-s3/protocol/s3_wire_matrix.md`.
- `test/awskit-s3/fixtures/protocol/README.md`.
- `test/awskit-s3/fixtures/protocol/fuzz-replay/README.md`.
- `test/awskit-s3/support/awskit_s3_test.ml`.
- `test/awskit-s3/support/s3_contract.ml`.

Relevant aliases:

- `opam exec -- dune build @fuzz-replay`.
- `opam exec -- dune build @protocol-pbt`.
- `opam exec -- dune build @protocol-fixtures`.
- `opam exec -- dune build @check-protocol`.

## First Milestone

Improve small structured PBT before adding new replay families:

- Add `Content-Range` properties: valid `to_header |> of_header` round-trip and
  invalid boundary families.
- Add canonical query/header normalization properties in `test/awskit`:
  duplicate names, tabs/spaces, empty query values, percent cases, slash cases.
- Add generated XML decoder properties for ListObjectsV2 and Tagging through
  public/test APIs.
- Expand endpoint rejection PBT to fragments, bad ports, empty hosts, malformed
  IPv6, unsupported schemes, userinfo, paths, queries, and control characters.

Validation for the milestone:

- `opam exec -- dune build @protocol-pbt`.
- `opam exec -- dune build @protocol-fixtures`.
- `opam exec -- dune build @fuzz-replay`.
- `opam exec -- dune build @check-protocol` for broader shared changes.

## Replay Corpus Rules

Each committed replay case must be minimized, deterministic, and tied to one
Awskit-owned invariant. Inputs should have sidecar `.expected` files with
stable output such as error kind, field, status, retry class, or normalized
success summary.

Do not commit large random corpora, provider dumps, secrets, live AWS
responses, or MinIO-only quirks. MinIO findings must be reduced to an Awskit
protocol invariant before entering `fuzz-replay`.

Organize by protocol domain:

- `fuzz-replay/endpoint`.
- `fuzz-replay/headers`.
- `fuzz-replay/xml`.
- Optional later: `fuzz-replay/response`.
- Optional later: `fuzz-replay/signing`.

Valid-output canonicalization bugs may belong as golden signing or presign
fixtures instead of `fuzz-replay`. Focused deterministic unit tests are also
acceptable when a fixture would be too indirect.

Sidecars should track public classification, not incidental printer wording.
Response checksum values are opaque unless Awskit explicitly claims validation;
do not imply base64 validation as a fuzz oracle until that contract exists.

## Generators And Shrinking

Use small structured QCheck generators in PR. Avoid raw byte fuzzing in default
CI.

Generator guidance:

- Fixed seeds.
- Useful `print` functions.
- Low rejection rates.
- XML mini-ASTs or compact string builders.
- Shrinkers that remove XML children, shorten lists, and reduce values to
  boundary cases such as empty string, zero, negative-looking text, non-integer
  text, max-plus-one, and control characters.

## Offline Fuzzing Discipline

Offline fuzzing is a discovery tool, not a release gate. Keep it no-network,
deterministic by seed, bounded by time and case count, and runnable outside
default Dune aliases. Prefer QCheck-style generators and small mutation drivers
before adding new fuzzing dependencies.

A fuzz failure should become committed deterministic evidence before the bug is
called fixed. That evidence can be a replay fixture, a golden fixture, or a
focused deterministic unit/property test, depending on the failure.

Do not add `@fuzz-offline` yet. If an opt-in alias exists later, keep it outside
`@check-*` aliases.

## Risks And Open Questions

- Response replay can blur protocol behavior with runtime HTTP framing. Keep
  `HEAD`, `204`, and `304` socket handling in runtime tests.
- Sidecars can become too brittle if they assert full printer output.
- Fuzzing can accidentally expand scope into unsupported S3 variants such as
  directory buckets, access points, or provider-specific behavior.
- MinIO differences must be named as capability differences, not weakened into
  global assertions.
- Private parser modules should stay private; tests should reach them through
  public or test harness APIs.
- Open question: should a future response replay family use a compact `.case`
  format, or stay as one OCaml table in `test_fuzz_replay.ml` with fixture
  payloads only?
