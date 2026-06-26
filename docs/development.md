# Awskit Development Guide

This guide describes how to make durable Awskit changes. It is not a general
OCaml style guide; it is about keeping Awskit's public API, package boundaries,
runtime adapters, wire behavior, and maintenance story coherent.

Use it with `docs/architecture.md` for package layout and with
`docs/testing.md` for check selection.

## Start With The User-Facing Shape

Before writing implementation code, identify the public workflow the change is
meant to support.

- Put new public helpers where Awskit users naturally look for them, such as
  `Object`, `Bucket`, `Multipart`, `Presigned`, `Transfer`, or a runtime
  adapter entrypoint.
- Read the existing `.mli` before changing a module. Public interfaces are the
  SDK contract and the main documentation source for package users.
- Sketch the `.mli` first when a change affects a package boundary, runtime
  adapter, service operation, option record, result record, or domain value.
- Keep implementation-only modules private unless callers need a stable public
  contract.
- Prefer a small addition to an existing role over a new namespace when the
  workflow already belongs to that role.

When the right package or module is not clear, pause and settle the boundary
before widening the patch. Package placement is part of Awskit's API design,
not file organization housekeeping.

## Public API Shape

Awskit APIs should make correct AWS usage obvious at the call site.

- Use labeled arguments for values that are easy to confuse: bucket names,
  object keys, regions, endpoints, ranges, content lengths, account ids,
  owners, and guards.
- Keep public names descriptive. Local helper names can be shorter when their
  scope is small and obvious.
- Preserve the established module pattern: a module's primary type is `t`, and
  functions that operate on `t` take it as the first argument unless the
  surrounding API already uses a different shape.
- Use option records for operation inputs that callers build directly.
- Use result records for structured operation outputs, especially when AWS may
  add fields later.
- Keep convenience helpers and advanced helpers on the same domain vocabulary:
  the same client handle, options, result records, body ownership model, and
  `Awskit.Error.t`.

Opaque types are the default for values with invariants. Keep ETags, version
ids, ranges, credentials, endpoints, requests, checksums, and structured errors
abstract when callers should not construct arbitrary values. Use `private`
records or variants when callers need to inspect values but construction must
stay validated.

Expose a concrete variant only when pattern matching is part of the intended
public API and the constructors themselves enforce the important invariants.

## Awskit Vocabulary

Use role names consistently so users can move between packages without
relearning the SDK.

- `Runtime` owns effect, clock, sleep, random, timeout, cancellation, and
  transport execution contracts.
- `Provider` resolves values from an environment or backend, such as
  credentials and regions.
- `Transport` is the HTTP execution boundary.
- `Body` and `Reader` describe request and response body ownership.
- `Credentials`, `Endpoint`, `Retry`, and `Timeout` name their domain modules.
- `S` is the primary module type in a role namespace.
- `Make` names public functors, including custom runtime composition.

`Internal` is not a public extension role. Use private Dune modules for
implementation details, or choose a real role name when the contract is meant
to be public. Runtime, service, and simulator code that intentionally builds
shared SDK errors should use `Awskit.Error.Producer`.

## Package And Runtime Boundaries

Keep runtime-neutral behavior in runtime-neutral packages. Request
construction, signing inputs, XML, headers, checksums, pagination state,
operation options, and validation should not depend on Lwt, Eio, Unix, or a
specific HTTP backend.

Runtime adapters own effectful execution:

- HTTP execution and connection lifecycles.
- Request and response body streaming.
- File IO and local filesystem behavior.
- Sleeps, clocks, cancellation, timeouts, and runtime-specific cleanup.

Do not add Lwt dependencies to Eio packages or Eio dependencies to Lwt
packages. Add dependencies in the smallest package that needs them, and only
when they improve correctness, interoperability, or the user experience enough
to justify the public maintenance cost.

The simulator is a public package with a private implementation. Keep its root
API useful for tests and local workflows while avoiding accidental exposure of
state internals.

## Domain Modeling

Model AWS concepts so invalid states are hard to express.

- Use records for data that exists together.
- Use variants for mutually exclusive states, strategies, classifications, and
  protocol outcomes.
- Prefer ordinary variants for closed Awskit domains.
- Use polymorphic variants only where their flexibility is part of the public
  or adapter-local design.
- Avoid catch-all variant cases in code that should be refactor-friendly.
- Use `_` in record patterns only when ignoring future fields is intentional.
- Use record update syntax only when unchanged fields truly do not need to be
  reconsidered after a record grows.

For AWS-controlled wire domains, decide forward compatibility explicitly. Use
`Unknown of string` when AWS may add values and callers need to round-trip or
inspect the original wire token. Use a sendable constructor such as
`Other of string` only when S3-compatible providers plausibly define their own
wire values.

Constructors for validated values should return
`(_, Awskit.Error.t) result`. `_exn` helpers are acceptable as explicit bridges
for examples, tests, or users who want exception style, and their names must
make that behavior clear.

## Errors And Ownership

Expected production failures should appear in the type as
`('a, Awskit.Error.t) result`.

- Use `option` when absence is a normal successful outcome, such as object
  lookup helpers.
- Convert exceptions from external libraries at the boundary into
  `Awskit.Error.t` with the narrowest useful context.
- Attach operation context that helps users act: service, operation, resource,
  retry attempt, status, request id, S3 error code, limit, or field name.
- Reserve exceptions for exceptional conditions, test helpers, and explicit
  `_exn` bridge functions.
- Catch exceptions around the smallest expression that can raise.
- Preserve the original exception or cancellation reason when cleanup follows a
  callback failure.

Body and resource ownership must be clear in the type and in the implementation
order. Prefer scoped `with_*` or `consume` APIs when a runtime offers them.
Close or drain response bodies on both success and failure when ownership has
transferred to Awskit. Abort fresh multipart uploads if post-create work fails
before completion, but do not abort caller-supplied resumable uploads unless
the API explicitly owns that lifecycle.

Make side-effect order explicit with bindings. Avoid hiding request execution,
file writes, cleanup, logging, or cancellation-sensitive behavior inside a
large expression.

## Wire Behavior

Use structured parsers and typed encoders for AWS wire formats. Do not parse
XML, JSON, headers, timestamps, checksums, ranges, or metadata with ad hoc
string slicing when an existing parser or local helper can express the
contract.

- Preserve AWS spellings in `to_string` functions.
- Normalize only where the protocol allows it.
- Keep request-building validation separate from response-decoding validation.
- Convert parse failures into `Awskit.Error.Decode`.
- Preserve tolerance for unknown XML elements where AWS compatibility requires
  it.
- Keep strict validation for known malformed fields.

When changing wire behavior, update focused protocol fixtures, malformed-input
tests, round-trip tests, or simulator evidence that proves the contract. Choose
the narrowest validation that demonstrates the changed behavior.

## Streaming, Pagination, And Memory

Awskit should handle large S3 objects, large local files, and long listings
without surprising memory use.

- Prefer streaming and folds for object bodies, transfer paths, and paginated
  results.
- Avoid building intermediate lists in transfer code when an incremental loop
  or sequence expresses the workflow clearly.
- Keep response body readers scoped to their consumer.
- Do not read a whole payload into memory unless the helper explicitly promises
  an in-memory result.
- Preserve retry and replay semantics when request bodies can be sent more than
  once.

If performance is the point of the patch, include a test, benchmark, or
concrete measurement that demonstrates the relevant behavior.

## Implementation Habits

Every `.ml` file is a module and every public `.mli` is a contract. Keep files
focused on one responsibility and avoid creating cycles that force broad
restructuring.

- Follow the dialect already used by the surrounding package.
- Use broad `open` statements sparingly.
- Prefer local opens such as `Result.Let_syntax` or `Int64.(...)` when shorter
  names help only one expression.
- Prefer short local module aliases inside a function or block when a long path
  would obscure the logic.
- Avoid top-level aliases like `module O = Object` unless the whole file
  clearly benefits.
- Use `include` only when intentionally extending or re-exporting a stable
  interface.
- Add a functor only when shared behavior genuinely depends on a module
  contract.
- Avoid classes and objects for Awskit internals unless an external dependency
  requires them.
- Do not use `Obj` in production code.

When adding generated converters such as sexp or JSON derivations, expose only
the converters that belong in the public contract. Debug converters are useful,
but they should not force users to depend on implementation representation.

## Documentation And Checks

Public API changes usually need matching examples, package docs, tests, and a
`CHANGES.md` entry once the implementation commit exists and can be referenced.
Keep package documentation in `packages/*/doc` aligned with the exposed `.mli`
surface.

Use Dune and package metadata according to `docs/architecture.md`. When package
metadata changes, update `dune-project` first and regenerate opam files with:

```sh
opam exec -- dune build @opam
```

Select checks from `docs/testing.md`: start with the narrowest command that
proves the change, then broaden only when the change affects shared behavior,
public API, packaging, CI, releases, or multiple runtimes.
