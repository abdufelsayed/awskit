# OCaml Development Guide

This guide defines how Awskit code should be shaped. It is written for agents
and maintainers who are changing public APIs, runtime adapters, parsers,
simulators, tests, examples, or package metadata.

## Public Interfaces First

Design the public `.mli` before filling in the `.ml` when a change affects a
package boundary or user-facing API. The interface should make the supported
workflow obvious at the call site.

- Prefer labeled arguments for values that are easy to confuse, especially S3
  bucket names, keys, regions, endpoints, content lengths, ranges, and owner
  guards.
- Keep public names longer and more descriptive than local helper names.
- Preserve the existing module pattern: a module's primary type is named `t`,
  and functions that operate on `t` take it as the first argument unless an
  established Awskit API shape says otherwise.
- Add new public helpers where users naturally look for them, such as
  `Object`, `Bucket`, `Multipart`, `Presigned`, `Transfer`, or the adapter
  entrypoint modules.
- Keep implementation modules private unless callers need a stable public
  contract.

Opaque types are the default for values with invariants. Keep types such as
ETags, version ids, ranges, credentials, endpoints, requests, and structured
errors abstract when callers should not construct arbitrary values. Expose a
concrete type only when pattern matching is part of the intended public API and
the type definition itself enforces the important invariants.

Use `private` records or variants when callers need to inspect values but must
not construct invalid ones. Use ordinary records for option/result records where
callers are expected to build values directly, such as operation option records.

## Module Boundaries

Every `.ml` file is a module, so treat files as real API and dependency
boundaries. Keep modules focused on one responsibility, and avoid creating
cycles that force broad restructuring.

Use broad `open` statements sparingly:

- `open Base` is fine in implementation files that already follow that style.
- Prefer local opens such as `Result.Let_syntax` or `Int64.(...)` when the
  shorter names are useful only for one expression.
- Prefer short local module aliases inside a function or small block when a
  long module path would obscure the logic.
- Do not introduce top-level aliases like `module O = Object` unless the whole
  file clearly benefits from that abbreviation.

Use `include` only when the module is intentionally extending or re-exporting a
stable interface. Do not use `include` as a shortcut for copying names into a
namespace unless the resulting public surface is deliberate.

## Records And Variants

Records represent data that exists together. Variants represent alternatives.
Use that distinction to make invalid states hard to express.

- Put shared fields into a shared record when several cases carry the same
  context.
- Use variants for mutually exclusive states, strategies, classifications, and
  protocol outcomes.
- Prefer ordinary variants for closed Awskit domains.
- Use polymorphic variants only where their flexibility is part of the API,
  such as small strategy tags or adapter-local extensibility.
- Avoid catch-all variant cases in code that should be refactor-friendly.
  Spell out constructors so the compiler can flag new cases.
- Use `_` in record patterns only when ignoring future fields is intentional.
- When field names overlap across record modules, add type annotations or
  module-qualified fields instead of relying on inference order.
- Use record update syntax only when unchanged fields truly do not need to be
  reconsidered after a record grows.

For AWS wire domains, preserve forward compatibility deliberately. Closed
variants are appropriate for values owned by Awskit. Use `Unknown of string`
when AWS may add values and callers need to round-trip or inspect the original
wire value.

## Error Handling

Expected production failures should appear in the type. In Awskit that means:

- Use `('a, Awskit.Error.t) result` for validation, signing, transport,
  service, body, decode, retry, and operation failures.
- Use `option` when absence is a normal outcome, such as object lookup helpers.
- Reserve exceptions for exceptional conditions, test helpers, and explicit
  `_exn` bridge functions.
- Functions that routinely raise must end in `_exn`.
- Convert exceptions from external libraries at the boundary into
  `Awskit.Error.t` with the narrowest useful context.
- Catch exceptions around the smallest expression that can raise. Do not wrap a
  broad caller callback and accidentally swallow the caller's exception.
- When cleaning up after a callback failure, preserve the original exception or
  cancellation reason.

Attach operation context where it helps users act on an error. Prefer structured
context over prose strings when the information has fields, such as service,
operation, resource, retry attempt, status, request id, or S3 error code.

## Resource Cleanup

Any code that opens, reads, writes, uploads, downloads, starts multipart state,
or scopes response bodies must be correct when exceptions or cancellation
interrupt the happy path.

- Prefer `with_*` or scoped-consumer APIs when the runtime offers them.
- Use `Fun.protect`, `Exn.protect`, or the adapter's equivalent cleanup pattern
  when manually acquiring and releasing resources.
- Close or drain response bodies on both success and failure when ownership has
  transferred to Awskit.
- Abort fresh multipart uploads if post-create work fails before completion.
- Do not abort caller-supplied resumable uploads unless the API explicitly owns
  that lifecycle.
- Process large files and S3 bodies incrementally instead of reading the whole
  payload into memory, unless the helper explicitly promises an in-memory
  result.

Make side-effect order explicit with `let` bindings. Do not depend on
evaluation order inside a large expression when requests, writes, reads,
cleanup, logging, or cancellation can be involved.

## Runtime Adapters

Keep the core packages runtime-neutral. Lwt and Eio code belongs in adapter
packages and adapter-specific tests.

- Do not add Lwt dependencies to Eio packages or Eio dependencies to Lwt
  packages.
- Keep shared request construction, signing, XML parsing, headers, and
  transfer validation in runtime-neutral modules.
- Keep effectful HTTP execution, body streaming, file IO, and concurrency in
  adapter modules.
- Use module signatures or small adapter modules for dependency injection when
  tests, simulators, or runtime backends need swappable behavior.
- Add a functor only when the shared behavior genuinely depends on a module
  contract; simple helper functions are usually easier to review.

Avoid classes and objects for Awskit internals unless an external dependency
requires them. Modules, records, variants, and first-class functions are the
normal design tools in this repository.

## Wire Parsing And Serialization

Use structured parsers and typed encoders for wire formats. Do not parse XML,
JSON, headers, timestamps, or checksums with ad hoc string slicing when an
existing parser or local helper can express the contract.

- For XML, preserve tolerance for unknown elements where AWS compatibility
  requires it.
- Keep strict validation for known malformed fields.
- Convert parse failures into `Awskit.Error.Decode`.
- Keep request-building validation separate from response-decoding validation.
- Add round-trip tests for serializers and focused malformed-input tests for
  decoders.
- Preserve AWS spellings in `to_string` functions and normalize only where the
  protocol allows it.

When adding generated converters such as sexp or JSON derivations, expose only
the converters that belong in the public contract. Debug converters are useful,
but they should not force users to depend on implementation representation.

## Tests

Tests should cover behavior through the public surface whenever practical.
Implementation-level tests are acceptable when the behavior cannot be reached
through a public API without making the public API worse.

- Put the bulk of tests under `test/`, not inside production libraries.
- Keep tests deterministic and fast unless they are explicitly integration or
  contract tests.
- Use Alcotest assertions that show useful values on failure.
- Use property tests for parsers, formatters, validation boundaries, and
  round-trips when examples alone would miss important combinations.
- Use examples or trace-style tests for workflows whose behavior is easier to
  understand from a short execution story.
- Do not add tombstone tests that only prove a removed API, symbol, or option
  no longer exists.
- Let `dune build` prove deleted symbols are gone.
- For bug fixes, test the original symptom and the supported behavior that now
  works.
- For resource fixes, test both the normal path and the callback/IO failure path
  when the failure mode is observable.

Choose the narrowest test that proves the change, then run broader suites when
the change affects shared behavior, package boundaries, runtime adapters,
release validation, or public documentation.

## Performance And Memory

Start with clear types and correct behavior. Optimize after identifying the
hot path or resource bound.

- Prefer streaming and folds for large bodies, large files, and paginated S3
  results.
- Avoid building intermediate lists in tight transfer paths when an incremental
  loop is clearer and avoids avoidable allocation.
- Prefer ordinary variants over polymorphic variants in closed hot-path data.
- Avoid unnecessary tuple wrapping in variants that carry multiple fields.
- Do not use `Obj` in production code.
- Do not add mutable state for speed unless the ownership and concurrency model
  are obvious from the surrounding code.

If a performance change is the point of the patch, include a test, benchmark,
or concrete measurement that demonstrates the relevant behavior.

## Dune And Packages

Package boundaries are part of the public design.

- Update `dune-project` first when package metadata changes.
- Regenerate opam files with `opam exec -- dune build @opam`.
- Keep dependencies in the smallest package that needs them.
- Verify OCaml 4.14 compatibility for non-Eio packages.
- Verify OCaml 5.2 compatibility for Eio packages.
- Keep documentation in `packages/*/doc` aligned with the exposed `.mli`
  surface.

When a change touches public APIs, update examples, docs, tests, and
`CHANGES.md` together after the implementation commit exists and the changelog
entry can reference the correct commit.
