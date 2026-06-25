# Runtime HTTP Contract Draft

Draft status: reviewed by the original runtime explorer and revised from the
formatting draft.

## Purpose

Establish a release-gateable runtime HTTP contract for low-level body,
framing, timeout, cancellation, and cleanup behavior that S3 depends on. This
layer should catch adapter regressions like `HEAD` responses with
`Content-Length` being drained as though body bytes exist.

This is runtime evidence, not provider support. Awskit owns AWS S3 behavior.
MinIO is the local S3-compatible contract double. The simulator is the
deterministic semantic test arena. Observations from S3-compatible systems can
motivate regressions, but must not become support claims.

## Current Gap

`test/runtime-conformance/test_runtime_conformance.ml` exercises runtime laws
through `Recording_runtime`, which is useful for API laws, reader scope,
drain/error precedence, and retry/sleep capabilities. It cannot prove real
HTTP framing, socket, or Cohttp behavior.

Eio now has focused loopback coverage for a `HEAD` response with an advertised
body and no bytes. Lwt has strong fake-client coverage, but fake clients cannot
reproduce the exact keep-alive hang class caused by misleading framing.
S3-level tests can prove `HeadObject` outcomes, but they should not be
responsible for detecting EOF latches, drain hangs, or response-body scope
mistakes.

## Proposed Test Layer

Add a Runtime HTTP Contract sublayer under runtime conformance:

- Shared pure scenario catalog: method, status, headers, body behavior, and
  expected adapter result.
- Adapter-specific loopback runners for Eio and Lwt, because server lifecycle
  and scheduling are runtime-owned.
- Assertions through `Awskit.Runtime.S`: `Transport.with_response`,
  `Response_body.with_reader`, `Response_body.discard`, timeout policy, and
  error precedence.
- No Docker, AWS, MinIO, or arbitrary S3-compatible endpoint.

Keep shared support runtime-neutral. A shared catalog may live under
`test/support`, but Eio, Lwt, Cohttp, and socket plumbing should remain in the
adapter-specific test directories.

## Files And Aliases

Likely shared support:

- `test/support/runtime_http_contract.ml`
- `test/support/dune`

Likely adapter runners:

- `test/awskit/eio/http_contract_eio.ml`
- `test/awskit/eio/test_awskit_eio.ml`
- `test/awskit/eio/dune`
- `test/awskit/lwt/http_contract_lwt.ml`
- `test/awskit/lwt/test_awskit_lwt.ml`
- `test/awskit/lwt/dune`

Relevant implementation contracts:

- `packages/awskit/runtime.mli`
- `packages/awskit/eio/runtime.ml`
- `packages/awskit/lwt/runtime.ml`

Focused aliases:

- `opam exec -- dune build @test/awskit/eio/runtime-http-contract`
- `opam exec -- dune build @test/awskit/lwt/runtime-http-contract`

Broader gates:

- `opam exec -- dune build @runtime-conformance`
- `opam exec -- dune build @check-protocol`

## First Milestone

Create the shared pure scenario catalog and run the initial matrix for both Eio
and Lwt, not only the runtime that recently regressed.

Initial matrix:

- `HEAD 200` with `Content-Length: 5`, no body, keep-alive held open: read
  returns empty payload and drain is a no-op.
- `HEAD 404` or `HEAD 403` with `Content-Length: 5`, no body, keep-alive held
  open: early/error response paths still treat the body as empty.
- `GET 204` with `Content-Length: 5`, no body, keep-alive held open:
  read/discard completes without waiting for drain timeout.
- `GET 304` with `Content-Length: 5`, no body, keep-alive held open: same
  bodiless behavior.
- Proper chunked response with terminating zero chunk and keep-alive: adapter
  does not read past the terminating chunk.

Wire the new Eio and Lwt focused aliases into their existing
`runtime-conformance` aliases, then confirm `@check-protocol` picks them up.

## Scenario Backlog

- Bodiless status matrix: `HEAD` for success and service-error statuses,
  `204`, `304`, and any surfaced `1xx` response the HTTP client exposes.
- Early response while streaming request body: response body remains readable,
  and producer cancellation/finalization follows runtime rules.
- Short streaming request body relative to declared `Content-Length`: body
  error, no transport hang.
- Long streaming request body relative to declared `Content-Length`: write or
  finish error, no transport hang.
- Consumer returns `Error` with unread body: consumer error wins over drain
  failure.
- Consumer raises or native cancellation fires: cleanup is attempted without
  wrapping the original exception or cancellation.
- Response read timeout: reader becomes invalid and drain cleanup does not add
  a second long wait.
- Drain byte limit: unread oversized response fails with `Awskit.Error.Body`
  and preserves the configured limit.
- Premature EOF and malformed transfer framing: classify as body/transport
  errors without hanging.
- Reader scope escape: using a reader after `with_reader` remains a body error.

## CI Discipline

Keep runtime HTTP contract local, deterministic, and fast. Prefer explicit
promises, latches, and server-observed events over sleeps. Use small timeouts
only as hang guards, not as the primary determinism mechanism.

Run scenarios serially with ephemeral ports and explicit server cleanup. Keep
payloads tiny except for a bounded drain-limit case. Reuse existing narrow
listener-bind skip behavior only for known sandbox bind-denied cases.

Do not add MinIO or Docker to this layer. Promote new focused aliases in
`docs/testing.md` only when the tests exist. Until then, `@runtime-conformance`
and `@check-protocol` remain the public aliases.

## Risks And Open Questions

- Lwt needs a real loopback runner with `Cohttp_lwt_unix.Client`; fake-client
  tests cannot reproduce wire-level keep-alive hangs.
- Some HTTP clients may swallow interim `1xx` responses, so the first milestone
  should not depend on surfacing them.
- Tiny timeouts can become flaky in loaded CI; use them only as outer guards.
- Bodiless response handling must avoid reading absent bytes without
  accidentally poisoning reusable connections when a noncompliant server sends
  bytes anyway.
- The contract must stay provider-neutral: no S3-compatible provider support
  promise.
