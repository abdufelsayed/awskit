# Architecture

This document defines Awskit's repository-specific engineering constraints. It
is about how this codebase is organized, not general OCaml style.

## Public Surface

Package boundaries are part of the public design. Put new public helpers where
users naturally look for them, such as `Object`, `Bucket`, `Multipart`,
`Presigned`, `Transfer`, or the adapter entrypoint modules.

Keep implementation modules private unless callers need a stable public
contract. Follow `docs/maintenance/ocaml-development.md` for `.mli` shape and
OCaml API style.

When a change touches public APIs, update examples, package documentation,
tests, and `CHANGES.md` together after the implementation commit exists and the
changelog entry can reference the correct commit.

## Runtime Boundaries

Keep core packages runtime-neutral. Lwt and Eio code belongs in adapter packages
and adapter-specific tests.

- Do not add Lwt dependencies to Eio packages or Eio dependencies to Lwt
  packages.
- Keep shared request construction, signing, XML parsing, headers, and transfer
  validation in runtime-neutral modules.
- Keep effectful HTTP execution, body streaming, file IO, and concurrency in
  adapter modules.
- Use module signatures or small adapter modules for dependency injection when
  tests, simulators, or runtime backends need swappable behavior.

## Error Model

Expected production failures should appear in the type. In Awskit that means
using `('a, Awskit.Error.t) result` for validation, signing, transport,
service, body, decode, retry, and operation failures.

Use `option` when absence is a normal outcome, such as object lookup helpers.
Convert exceptions from external libraries at the boundary into
`Awskit.Error.t` with the narrowest useful context.

Attach operation context where it helps users act on an error. Prefer structured
context over prose strings when the information has fields, such as service,
operation, resource, retry attempt, status, request id, or S3 error code.

## Resource Ownership

Any code that opens, reads, writes, uploads, downloads, starts multipart state,
or scopes response bodies must be correct when exceptions or cancellation
interrupt the happy path.

- Prefer `with_*` or scoped-consumer APIs when the runtime offers them.
- Use the adapter's cleanup pattern when manually acquiring and releasing
  resources.
- Close or drain response bodies on both success and failure when ownership has
  transferred to Awskit.
- Abort fresh multipart uploads if post-create work fails before completion.
- Do not abort caller-supplied resumable uploads unless the API explicitly owns
  that lifecycle.
- Process large files and S3 bodies incrementally instead of reading the whole
  payload into memory, unless the helper explicitly promises an in-memory
  result.

Make side-effect order explicit with bindings. Do not depend on evaluation
order inside a large expression when requests, writes, reads, cleanup, logging,
or cancellation can be involved.

## Wire Formats

Use structured parsers and typed encoders for AWS wire formats. Do not parse
XML, JSON, headers, timestamps, or checksums with ad hoc string slicing when an
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

For AWS wire domains, preserve forward compatibility deliberately. Closed
variants are appropriate for values owned by Awskit. Use `Unknown of string`
when AWS may add values and callers need to round-trip or inspect the original
wire value.

## Streaming And Memory

Prefer streaming and folds for large bodies, large files, and paginated S3
results. Avoid building intermediate lists in transfer paths when an
incremental loop is clearer and avoids avoidable allocation.

If a performance change is the point of the patch, include a test, benchmark,
or concrete measurement that demonstrates the relevant behavior.

## Packages And Compatibility

Update `dune-project` first when package metadata changes, then regenerate opam
files:

```sh
opam exec -- dune build @opam
```

Keep dependencies in the smallest package that needs them.

- Verify OCaml 4.14 compatibility for non-Eio packages.
- Verify OCaml 5.2 compatibility for Eio packages.
- Keep documentation in `packages/*/doc` aligned with the exposed `.mli`
  surface.
