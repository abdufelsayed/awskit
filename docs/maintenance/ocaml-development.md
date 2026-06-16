# OCaml Development Guide

This guide defines OCaml implementation and API style for Awskit. Repository
architecture rules live in `docs/maintenance/architecture.md`; test and
validation rules live in `docs/maintenance/testing.md`.

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
- Avoid catch-all variant cases in code that should be refactor-friendly. Spell
  out constructors so the compiler can flag new cases.
- Use `_` in record patterns only when ignoring future fields is intentional.
- When field names overlap across record modules, add type annotations or
  module-qualified fields instead of relying on inference order.
- Use record update syntax only when unchanged fields truly do not need to be
  reconsidered after a record grows.

## Error Handling

Expected failures should appear in the type.

- Reserve exceptions for exceptional conditions, test helpers, and explicit
  `_exn` bridge functions.
- Functions that routinely raise must end in `_exn`.
- Catch exceptions around the smallest expression that can raise. Do not wrap a
  broad caller callback and accidentally swallow the caller's exception.
- When cleaning up after a callback failure, preserve the original exception or
  cancellation reason.

Avoid classes and objects for Awskit internals unless an external dependency
requires them. Modules, records, variants, and first-class functions are the
normal design tools in this repository.

Use `Fun.protect`, `Exn.protect`, or the adapter's equivalent cleanup pattern
when manually acquiring and releasing resources.

## Abstraction Boundaries

Add a functor only when the shared behavior genuinely depends on a module
contract; simple helper functions are usually easier to review.

When adding generated converters such as sexp or JSON derivations, expose only
the converters that belong in the public contract. Debug converters are useful,
but they should not force users to depend on implementation representation.

## Performance And Memory

Start with clear types and correct behavior. Optimize after identifying the
hot path or resource bound.

- Prefer ordinary variants over polymorphic variants in closed hot-path data.
- Avoid unnecessary tuple wrapping in variants that carry multiple fields.
- Do not use `Obj` in production code.
- Do not add mutable state for speed unless the ownership and concurrency model
  are obvious from the surrounding code.

## Dune Files

Use Dune and package metadata according to `docs/maintenance/architecture.md`.
