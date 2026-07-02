# S3 0.3 Public API Cleanup

Status: superseded
Issue: none
Goal: Finish the breaking awskit-s3 0.3 public API cleanup so request inputs are string-facing, internals remain typed, and request option construction is validated.
Risk: high
Verification: dune build @fmt && dune build && dune runtest && dune build @examples @doc && dune build @opam && git diff --check

Superseded by the ordered 0.3 plan suite:

- `.agent/plans/ready/2026-07-02-s3-0-3-00-plan-suite.md`
- `.agent/plans/done/2026-07-02-s3-0-3-01-public-api-clean-break.md`
- `.agent/plans/done/2026-07-02-s3-0-3-02-endpoint-config-string-boundary.md`
- `.agent/plans/done/2026-07-02-s3-0-3-03-private-options-semantic-builders.md`
- `.agent/plans/ready/2026-07-02-s3-0-3-04-presigned-builders.md`
- `.agent/plans/ready/2026-07-02-s3-0-3-05-public-seam-tests.md`
- `.agent/plans/ready/2026-07-02-s3-0-3-06-docs-examples.md`
- `.agent/plans/ready/2026-07-02-s3-0-3-07-release-readiness.md`

## Context

This plan applies to /Users/abdllahdev/dev/awskit on branch `feat/s3-public-string-inputs`.

The working tree is already dirty with a large partial migration. Treat the current branch state as the baseline. Do not revert unrelated changes or try to go back to the old typed public API.

The design rule for 0.3 is:

> Public request inputs are primitive strings/ints. Validated domain types are used internally and in response/result models. Request option values are constructed through builders, not public record construction.

Real World OCaml grounding for this change:

- The `.mli` files are the public contract and the odoc source. Any helper exposed there is a supported API, even if it exists mainly to make internal call sites compile.
- Private record types in an interface block construction and record update outside the defining module. Other modules in the same library still consume the `.cmi`, so they cannot update private option records directly.
- Labeled arguments are part of the safety story when many string/int values sit next to each other. Preserve clear labels at every public primitive boundary.
- Expected validation failures should be represented with `result`. `_exn` functions are convenience bridges and should raise the same structured `Awskit.Error.Awskit_error` path.
- Tests should exercise the public seam and failure paths rather than relying on implementation details.

Keep these boundaries:

- Domain identifiers such as `Bucket_name.t`, `Object_key.t`, `Upload_id.t`, `Object.Version_id.t`, `Account_id.t`, `Content_type.t`, `Header_value.t`, `Awskit.Region.t`, and `Awskit.Endpoint.t` remain typed internally.
- Operation functions and option builders parse raw user inputs at the public boundary.
- Internal request modules, simulator modules, response/result records, and parsed service facts may remain typed.
- Request option records should not be freely constructible or updatable by users.
- If a private option record needs to be adjusted outside its defining module, add a semantic operation-owned constructor/updater or rebuild it through the public builder. Do not expose generic mutators just to recover record-update syntax.
- Do not add generic helper layers just to avoid writing direct parsing at each real boundary.
- Do not hide response/result records merely because request options become private.
- This is a breaking 0.3 release. Do not preserve backward compatibility for old S3 request-input shapes or the old `Object.Delete_many` module name.

The public API cleanup is a breaking change and belongs in 0.3.0, not 0.2.1.

## Files Likely To Change

- /Users/abdllahdev/dev/awskit/packages/awskit-s3/object.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/object.ml
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/bucket.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/bucket.ml
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/multipart.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/multipart.ml
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/presigned.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/presigned.ml
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/endpoint_config.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/endpoint_config.ml
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/awskit_s3.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/awskit_s3.ml
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/*_request.ml
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/eio/*.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/lwt/**/*.mli
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/sim/**/*.ml
- /Users/abdllahdev/dev/awskit/examples/**
- /Users/abdllahdev/dev/awskit/test/awskit-s3/**
- /Users/abdllahdev/dev/awskit/packages/awskit-s3/doc/*.mld
- /Users/abdllahdev/dev/awskit/dune-project

## Acceptance Criteria

- Normal object, bucket, multipart, transfer, presigned, sim, eio, and lwt public request APIs accept `bucket:string`, `key:string`, `upload_id:string`, `part_number:int`, `expected_bucket_owner:string`, `version_id:string`, `content_type:string`, and header-value strings where those values are caller request inputs.
- Public option builders parse strings into typed internals at construction time and return `(options, Awskit.Error.t) result`.
- `*_exn` builders raise `Awskit.Error.Awskit_error` with the same structured validation error path as existing constructors.
- `Object.*.options`, `Bucket.*.options`, `Multipart.*.options`, and `Presigned.*.options` are not freely constructible by users.
- Existing out-of-module option record construction/update sites are replaced before `private` is added to the interface. The build should not rely on same-library access to private records.
- Response/result models remain typed where they represent parsed service facts.
- `Bucket.Create.options` accepts `?region:string`, not `?region:Awskit.Region.t`.
- `Endpoint_config` public constructors accept `endpoint:string` and `signing_region:string` where users configure endpoints from raw input.
- If `Endpoint_config.unsafe_plaintext` parses public strings, it returns `(t, Awskit.Error.t) result`; add `unsafe_plaintext_exn` only if examples/tests need a raising bridge.
- Public root aliases use `Endpoint_config.t` for endpoint configuration; `Endpoint_resolver` is not promoted as the normal public user surface.
- `Presigned.*_with_endpoint_config` functions accept `region:string` at the public boundary, or the plan explicitly documents why they are the one typed-region exception.
- `Object.Delete_many` is renamed to `Object.Delete_objects`, and root/runtime APIs use the new module name.
- `Object.Delete_objects.object_` has an `object_exn` convenience constructor if the validated `object_` builder remains result-returning.
- Public README snippets, package docs, examples, simulator tests, protocol tests, and MinIO workload tests compile against the new API.
- `CHANGES.md` gets a concrete 0.3.0 breaking-change entry after the implementation commit/ref is known.
- Generated opam files are updated only through `dune build @opam` if package metadata changes.
- Release readiness follows `docs/release-gates.md`; the final release vehicle records local gate evidence, Required CI, public API review, support/security scope, and the live-AWS scope decision.

## Enforcement Strategy

The plan should be enforced by a mix of OCaml's type system, source audits,
and public-seam tests. Do not rely on review discipline alone.

- Compile-time enforcement:
  - Public `.mli` signatures must expose primitive request inputs and private option records.
  - Concrete option construction stays inside the defining module only.
  - Sibling modules that need adjusted options must use public semantic builders/updaters, which proves the same path is available to users.
  - Root aliases must name the intended public surface, such as `Endpoint_config.t`, so downstream users do not discover lower-level resolver types by accident.

- Source-audit enforcement:
  - Run the typed-request-input audit and record-update audit in this plan before completion.
  - Explain every remaining match as either an internal implementation surface, a response/result model, a simulator/runtime support type, or a deliberate exception.
  - Search examples and docs as well as library code, because examples are part of the public API story.
  - Include README and package `.mld` files in the public API audit; docs that teach typed request constructors are release blockers.

- Test enforcement:
  - Add success tests for the normal string-facing constructors/builders.
  - Add failure tests for invalid raw inputs at public boundaries.
  - For operation-call validation failures, assert no request reaches the recording runtime or simulator transport.
  - Add presigned tests that prove invalid or duplicate extra signed headers fail before URL generation.
  - Keep domain-type tests so internal validators remain stable even though most users no longer call them directly.

- Documentation enforcement:
  - Odoc-visible `.mli` comments should show the intended builder path for every private option record.
  - `CHANGES.md` should call out the breaking change clearly so users know typed request inputs were intentionally replaced with string-facing builders.
  - Release PR notes can discuss process and evidence; public `CHANGES.md` should describe shipped behavior and caller action only.

## Tasks

- [ ] Audit the remaining public typed request inputs.
  - Run:
    ```bash
    rg -n "\\?(bucket|key|upload_id|version_id|expected_bucket_owner|content_type|content_disposition|content_encoding|cache_control|region|endpoint|signing_region):[^\\n]*(Bucket_name|Object_key|Upload_id|Version_id|Account_id|Content_type|Header_value|Awskit\\.Region|Awskit\\.Endpoint)" packages/awskit-s3 examples test
    ```
  - Expected signal: only internal implementation surfaces, response/result records, or simulator-private helpers remain typed.

- [ ] Make request option records private in public interfaces.
  - First audit current direct construction/update sites:
    ```bash
    rg -n "default_options with|with (range|version_id|preconditions|continuation_token|key_marker|version_id_marker|part_number_marker|multipart_object_size)" packages/awskit-s3 examples test
    ```
  - Change public request option record declarations in object, bucket, multipart, and presigned `.mli` files from `type options = { ... }` to `type options = private { ... }`.
  - Do not make result records private.
  - Do not make domain identifiers concrete.
  - Keep each defining `.ml` implementation concrete so that module can construct and read its own fields normally.
  - Replace out-of-module record updates before flipping the interfaces:
    - pagination helpers in `object_request.ml`,
    - list-parts pagination in `multipart_request.ml`,
    - simulator pagination/helpers in `simulator_object.ml` and `simulator_multipart.ml`,
    - transfer option adjustment in `eio/transfer.ml` and `lwt/unix/transfer.ml`,
    - presign examples/tests that update `Presigned.*.default_options`,
    - multipart validation tests that update `Multipart.Complete.default_options`.
  - Do not dismiss simulator implementation modules as safe because they are Dune-private; they still compile against the public `awskit-s3` interface from the simulator package.
  - Because OCaml has no friend modules, any helper needed outside the defining module is public once it appears in the `.mli`. Prefer stable, operation-owned helpers such as pagination/transfer updaters over generic `set_*` mutators.
  - Update tests and examples that construct option records directly to use builders or these semantic helpers.

- [ ] Finish presigned option builders.
  - Add `options` and `options_exn` to:
    - `Presigned.Put_object`
    - `Presigned.Get_object`
    - `Presigned.Head_object`
    - `Presigned.Upload_part`
    - `Presigned.Delete_object`
  - Public builder inputs should be string-facing:
    - `?expires_in:Ptime.Span.t`
    - `?content_type:string`
    - `?response_content_type:string`
    - `?response_content_disposition:string`
    - `?version_id:string`
    - `?expected_bucket_owner:string`
    - `?extra_signed_headers:(string * string) list`
  - Keep already-typed non-string domain objects where they are not raw string inputs, such as `Object.Checksum.value`, `Encryption.Source.t`, `Encryption.Destination.t`, and `Encryption.Customer_key.t`.
  - Validate `extra_signed_headers` with existing duplicate-header checks.
  - Make `default_options` remain available but not sufficient for users to bypass validation by record update.
  - Change `Presigned.*_with_endpoint_config` public `region` arguments to `region:string` and parse them there. Keep a typed internal helper only if needed by the implementation.

- [ ] Change `Bucket.Create.options ~region` to string.
  - Public signature:
    ```ocaml
    val options :
      ?region:string -> unit -> (options, Awskit.Error.t) result
    ```
  - Parse with `Awskit.Region.of_string` inside the builder.
  - Keep `Create.options.region : Awskit.Region.t option` internally.
  - Update examples/tests that pass `Awskit.Region.t` directly.

- [ ] Clean the endpoint configuration public boundary.
  - Change `Endpoint_config.s3_compatible`, `local_plaintext`, and `unsafe_plaintext` to accept:
    - `endpoint:string`
    - `signing_region:string`
  - Parse with `Awskit.Endpoint.of_string` and `Awskit.Region.of_string` at the boundary.
  - Return `result` from any constructor that can fail because of parsing, including `unsafe_plaintext`; add `_exn` variants only as deliberate convenience bridges.
  - Keep `Endpoint_config.endpoint` and `Endpoint_config.signing_region` accessors typed because they return internal parsed facts.
  - Change `Awskit_s3.endpoint_config` and presigned endpoint-config aliases to `Endpoint_config.t`, not `Endpoint_resolver.t`, where possible.
  - Keep `Endpoint_resolver` internal unless a required runtime/functor signature needs it.

- [ ] Rename `Object.Delete_many` to `Object.Delete_objects`.
  - Rename module, references, docs, tests, and root/runtime API types.
  - Keep operation function names as `delete_objects` where they already match the AWS operation.
  - If the object descriptor builder remains validated and result-returning, add `object_exn` for parity with other validated constructors.
  - Do not add a compatibility alias or deprecation bridge. 0.3 intentionally removes the old `Delete_many` public name.

- [ ] Keep internal request construction typed and explicit.
  - Request modules such as `object_request.ml`, `bucket_request.ml`, `multipart_request.ml`, `object_tagging_request.ml`, and `presigned_request.ml` should receive typed option records and render typed values.
  - Do not pass raw string request inputs deep into request rendering.
  - Avoid adding generic `parse_opt` or catch-all conversion helpers. Small local parsing inside real public builders is preferred.

- [ ] Update docs and examples to teach the new API.
  - Update README snippets as well as package `.mld` docs.
  - Examples should pass raw strings for buckets, keys, regions, owners, version ids, content types, and endpoint config construction.
  - Presign examples should use `Presigned.*.options`/`options_exn`, not `{ default_options with ... }`.
  - `.mld` docs should explain:
    - public request inputs are string-facing,
    - domain constructors still exist for advanced/internal/result use,
    - request options must be built with `options` or `options_exn`,
    - private option records remain inspectable but are not user-constructible,
    - presigned URLs expose `reveal_url` intentionally and logging should use safe accessors.
  - `.mli` docs for private option records must show the intended construction/update path, since the interface is the public contract and the odoc source.

- [ ] Update tests through the public seam.
  - Protocol tests should assert invalid raw input fails at builders or operation calls.
  - Cover invalid bucket, key, owner, version id, content type/header value, part number, endpoint, and signing-region strings at public boundaries.
  - Cover presigned builder rejection of duplicate or invalid `extra_signed_headers`.
  - Add recording-runtime or simulator assertions that invalid public operation input sends zero transport requests.
  - Simulator/MinIO workload tests should use the same public API examples use.
  - Add smoke coverage through simulator, Lwt, and Eio-facing entrypoints for the migrated string APIs where coverage is otherwise only indirect.
  - Keep domain tests for `Bucket_name`, `Object_key`, `Upload_id`, `Version_id`, `Account_id`, `Content_type`, and `Endpoint` because these still define internal validation behavior.
  - Add or update tests that prove private option records cannot be bypassed only indirectly through compile-time public API use; do not add brittle type-error tests unless the repo already has a pattern for them.
  - Build examples and docs as part of verification so accidental public record-update examples are caught.

- [ ] Update release metadata for 0.3.0 only where the repo requires it.
  - `dune-project` has `generate_opam_files true`; edit `dune-project`, not generated opam files, unless generated files are being promoted.
  - Do not add exact dependency pins.
  - Add a `CHANGES.md` 0.3.0 breaking-change entry once the implementation commit/ref is known. The changelog is the canonical release ledger.
  - Version identity comes from the release branch/tag/dune-release `--pkg-version`; do not invent a source version bump unless the repo adds one.
  - If package metadata changes, run `opam exec -- dune build @opam` and inspect the generated opam diffs.

- [ ] Run or explicitly defer release-gate evidence.
  - For the implementation PR, run the narrow and full local checks below.
  - Before a production-ready release PR, follow `docs/release-gates.md`, including:
    ```bash
    opam install --yes --with-test --with-doc --with-dev-setup --deps-only .
    opam exec -- dune build @opam
    opam lint ./*.opam
    opam exec -- dune fmt
    git diff --check
    git diff --exit-code
    scripts/test.sh quick --label release-correctness
    opam install --yes eio_main tls-eio tls ca-certs domain-name mirage-crypto-rng
    opam exec -- dune build @examples @doc
    scripts/test.sh integration --label release-integration
    ```
  - The clean release branch also needs `dune-release check/distrib` and archive doc-build evidence from `docs/release-gates.md`.
  - Package matrix evidence must cover the S3 package family: `awskit-s3`, `awskit-s3-sim`, `awskit-s3-lwt`, `awskit-s3-lwt-unix`, and `awskit-s3-eio`, plus their same-version core/runtime dependencies.
  - Confirm `SUPPORT.md`, `SECURITY.md`, and `docs/security-threat-model.md` still match the release scope, especially endpoint policy, diagnostics, and presigned bearer URL docs.
  - Live AWS remains outside the release gate unless `SUPPORT.md` is changed to promise live AWS coverage.

- [ ] Decide and document public typed exceptions.
  - `Multipart.Upload.created` remains a public typed constructor only if it is intentionally documented as simulator/runtime support. Otherwise hide it or make it string-facing in the same 0.3 breaking window.
  - `Endpoint_resolver` remains public only if custom runtime signatures require it; it should not be the normal user endpoint configuration alias.

## Exact Verification Commands

Run narrow checks while implementing:

```bash
dune build @fmt
```

```bash
dune build @packages/awskit-s3/all
```

```bash
dune build @test/awskit-s3/runtest
```

Run full release-facing checks before completion:

```bash
dune build
```

```bash
dune runtest
```

```bash
dune build @examples @doc
```

```bash
dune build @opam
```

```bash
git diff --check
```

Run this audit before completion and explain any remaining matches:

```bash
rg -n "default_options with|with (range|version_id|preconditions|continuation_token|key_marker|version_id_marker|part_number_marker|multipart_object_size)" packages/awskit-s3 examples test
```

Run this docs/examples audit before completion and explain any remaining typed request-input teaching examples:

```bash
rg -n "Bucket_name\\.of_string|Object_key\\.of_string|Awskit\\.Region\\.of_string|Awskit\\.Endpoint\\.(of_string|http|https)|default_options with|Object\\.Delete_many" README.md packages examples test --glob '*.md' --glob '*.mld' --glob '*.ml' --glob '*.mli'
```

If opam files are regenerated, inspect the diffs before keeping them.

## Decisions For This Plan

- Use `private` request option records for 0.3 instead of fully abstract
  options. This blocks invalid public construction while keeping option values
  inspectable and minimizing churn across request renderers and tests.
- Do not treat private records as a same-library internal-only mechanism.
  The `.mli` controls other modules in the package too, so helper design must
  be intentional and public-API-worthy.
- Keep `Endpoint_config` as the public configuration surface. Minimize
  `Endpoint_resolver` exposure; make it private only if the runtime functor and
  adapters can still express their endpoint contract cleanly.
- Remove `Object.Delete_many` rather than adding a compatibility alias. This is
  a breaking cleanup release.
- Do not add compatibility aliases or deprecation bridges for old typed request
  input shapes. The 0.3 release should be internally consistent, not
  backward-compatible.

## Rollback Notes

- If making all option records private causes too much public test churn, keep the private-record change for the highest-risk request builders first: object writes/copies/deletes, multipart, presigned, and endpoint config.
- If private records require unstable helper APIs, defer that particular record's privacy rather than publishing accidental internal helpers.
- If hiding `Endpoint_resolver` breaks the runtime functor shape too widely, leave the module public for 0.3 but stop exposing it as the normal endpoint configuration alias.
- If `Delete_many` rename causes unrelated churn, split it into a separate 0.3 cleanup commit, but do not keep the old public module name in the final release.
- Do not rollback the string-facing boundary change unless the whole 0.3 public API cleanup is abandoned.
