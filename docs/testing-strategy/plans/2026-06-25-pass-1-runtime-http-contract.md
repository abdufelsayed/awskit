# Runtime HTTP Contract Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add loopback HTTP contract tests that prove Eio and Lwt handle
bodiless responses, chunk framing, draining, and socket cleanup correctly.

**Architecture:** Keep the scenario catalog runtime-neutral and put all socket,
Cohttp, Eio, and Lwt plumbing in adapter-specific runners. Runtime contract is
provider-neutral: it does not use AWS, MinIO, or the S3 simulator.

**Tech Stack:** OCaml, Dune aliases, Alcotest, Cohttp-Eio, Cohttp-Lwt-Unix,
local TCP loopback servers.

---

## Related Draft

For deeper rationale, scenario backlog, and edge-case discussion, read:

- `docs/testing-strategy/drafts/2026-06-25-runtime-http-contract.md`

## No-Fix Rule

This is a test-construction pass. Do not edit production runtime
implementations. If an adapter fails a scenario, leave the test in place,
record failure evidence under `docs/testing-strategy/failures/`, and stop.

Read-only production files for this pass:

- `packages/awskit/eio/runtime.ml`
- `packages/awskit/lwt/runtime.ml`
- `packages/awskit/runtime.ml`

## Files

- Create `test/support/runtime_http_contract.ml`
- Modify `test/support/dune`
- Create `test/awskit/eio/http_contract_eio.ml`
- Modify `test/awskit/eio/test_awskit_eio.ml`
- Modify `test/awskit/eio/dune`
- Create `test/awskit/lwt/http_contract_lwt.ml`
- Modify `test/awskit/lwt/test_awskit_lwt.ml`
- Modify `test/awskit/lwt/dune`

## Task 1: Shared Scenario Catalog

- [ ] Create `test/support/runtime_http_contract.ml`.

- [ ] Define one runtime-neutral scenario record with these fields:

  - Scenario name.
  - Request method.
  - Response status.
  - Response headers.
  - Response bytes written by the server.
  - Whether the server keeps the connection open after response headers.
  - Expected response-body string.

- [ ] Add this first scenario matrix:

  - `HEAD 200` with `Content-Length: 5`, no response bytes, keep-alive held
    open, expected body `""`.
  - `HEAD 404` with `Content-Length: 5`, no response bytes, keep-alive held
    open, expected body `""`.
  - `GET 204` with `Content-Length: 5`, no response bytes, keep-alive held
    open, expected body `""`.
  - `GET 304` with `Content-Length: 5`, no response bytes, keep-alive held
    open, expected body `""`.
  - `GET 200` with chunked bytes `5\r\nhello\r\n0\r\n\r\n`, keep-alive held
    open, expected body `"hello"`.

- [ ] Export helpers that let each runner iterate over the same scenarios.

## Task 2: Eio Loopback Runner

- [ ] Create `test/awskit/eio/http_contract_eio.ml`.

- [ ] Use a local TCP listener and one client request per scenario.

- [ ] Reuse the bind-denied sandbox skip behavior from
  `test/awskit/eio/test_integration.ml`.

- [ ] Use Eio promises, latches, or server-observed events for determinism.
  Use tiny timeouts only as outer hang guards.

- [ ] For each bodiless scenario, assert both consumption forms:

  - Reading to completion returns `""`.
  - `Awskit_eio.Runtime.Response_body.discard` returns successfully.

- [ ] For the chunked scenario, assert that reading to completion returns
  `"hello"` and does not block waiting for more bytes.

- [ ] Register the Eio suite in `test/awskit/eio/test_awskit_eio.ml`.

- [ ] Add a focused alias in `test/awskit/eio/dune`:

  ```scheme
  (rule
   (alias runtime-http-contract)
   (package awskit-eio)
   (deps test_awskit_eio.exe)
   (action
    (run ./test_awskit_eio.exe test "contract:awskit-eio:runtime-http")))
  ```

- [ ] Make the existing `runtime-conformance` alias depend on or run the new
  Eio contract suite.

## Task 3: Lwt Loopback Runner

- [ ] Create `test/awskit/lwt/http_contract_lwt.ml`.

- [ ] Use a real loopback path through `Cohttp_lwt_unix.Client`. Do not use a
  fake client for these scenarios.

- [ ] Use Lwt promises or server-observed events for determinism. Use timeouts
  only as outer hang guards.

- [ ] For each bodiless scenario, assert both consumption forms:

  - Reading to completion returns `""`.
  - `Awskit_lwt.Runtime.Response_body.discard` returns successfully.

- [ ] For the chunked scenario, assert that reading to completion returns
  `"hello"` and does not block waiting for more bytes.

- [ ] Register the Lwt suite in `test/awskit/lwt/test_awskit_lwt.ml`.

- [ ] Add a focused alias in `test/awskit/lwt/dune`:

  ```scheme
  (rule
   (alias runtime-http-contract)
   (package awskit-lwt)
   (deps test_awskit_lwt.exe)
   (action
    (run ./test_awskit_lwt.exe test "contract:awskit-lwt:runtime-http")))
  ```

- [ ] Make the existing `runtime-conformance` alias depend on or run the new
  Lwt contract suite.

## Task 4: Validation

- [ ] Run the focused Eio check:

  ```sh
  opam exec -- dune build @test/awskit/eio/runtime-http-contract
  ```

- [ ] Run the focused Lwt check:

  ```sh
  opam exec -- dune build @test/awskit/lwt/runtime-http-contract
  ```

- [ ] Run the broader runtime gate:

  ```sh
  opam exec -- dune build @runtime-conformance
  ```

- [ ] Run whitespace validation:

  ```sh
  git diff --check
  ```

- [ ] If any focused test fails because of product behavior, write failure
  evidence and do not edit production code.

## Backlog

- Early response while streaming a request body.
- Short streaming request body relative to declared `Content-Length`.
- Long streaming request body relative to declared `Content-Length`.
- Drain byte-limit failures.
- Premature EOF and malformed transfer framing.
- Consumer error versus drain failure precedence through real sockets.
