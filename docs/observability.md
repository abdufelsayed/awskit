# Observability

Awskit can show you what your S3 calls are doing through the logging,
metrics, and tracing tools you already use. Every operation the SDK performs —
an S3 request, a retry, a signing step, a file transfer — has one well-defined
lifecycle, and Awskit reports it through the `Logs` library, through optional
metric and trace sinks that you supply, or not at all.

Awskit never starts background workers, exporters, or network connections for
telemetry. Reporters, samplers, queues, transport, flushing, and shutdown
belong to your application, and Awskit's packages do not depend on Metrics,
Trace, or OpenTelemetry — those integrations are optional and live in your
code.

## Quick Start: Logs

The fastest way to see what the SDK is doing is `default ()`, which enables
log reporting through `Logs`. You install a reporter and choose levels per
source, as with any `Logs`-based library:

```ocaml
let () =
  Logs.Src.set_level Awskit_s3.Observability.Sources.operation
    (Some Logs.Warning);
  Logs.Src.set_level Awskit_s3.Observability.Sources.retry
    (Some Logs.Debug);
  Logs.Src.set_level Awskit.Observability.Sources.http
    (Some Logs.Error)

let observability = Awskit_lwt.Observability.default ()

let s3 =
  Awskit_s3_lwt_unix.create ~observability ()
  |> Result.get_ok
```

`Awskit_eio.Observability.default ()` is the direct-style equivalent. If you
omit `~observability`, the built-in clients behave as if you had passed
`default ()`.

Pass `Awskit_lwt.Observability.none` or `Awskit_eio.Observability.none` to
turn observation off completely for a client: no Logs source checks, no
clock reads, and no field encoding or sink dispatch. The only residual
per-call cost is allocating the call closures themselves.

### Sources and levels

Log output is organized into stable sources, one per SDK activity:

| Source value | Logs source | What it covers |
| --- | --- | --- |
| `Awskit_s3.Observability.Sources.operation` | `awskit.s3.operation` | One caller-visible S3 operation |
| `.attempt` | `awskit.s3.attempt` | One retry iteration |
| `.signing` | `awskit.s3.signing` | One S3 request signing |
| `.retry` | `awskit.s3.retry` | A scheduled or denied retry decision |
| `.transfer` | `awskit.s3.transfer` | One high-level upload or download |
| `.artifact` | `awskit.s3.artifact` | One presigned-URL generation |
| `.artifact_signing` | `awskit.s3.artifact.signing` | The signing step of a presigned-URL generation |
| `Awskit.Observability.Sources.http` | `awskit.http` | Physical HTTP requests and body phases |
| `.credentials` | `awskit.credentials` | Credential resolution |

You do not choose levels at call sites; the SDK does, based on how the
operation ended. Routine successes are silent. Expected outcomes such as
not-found or cancellation log at `Debug`, throttling at `Warning`, and
terminal failures at `Error`. Your per-source levels then decide what is
actually emitted, so a typical production setup silences successes entirely
and keeps warnings and errors. Log messages are built lazily: nothing is
formatted for a level that is disabled.

If you read `Logs` records yourself, the structured completion or event is
attached under `Awskit.Observability.Logs_tags.operation_completion` or
`.event`. Log messages are never also counted as metrics.

## Metrics And Traces

For metrics and tracing you write a small sink and hand it to an observer,
along with a monotonic clock:

```ocaml
let monotonic_ns () = Mtime_clock.now () |> Mtime.to_uint64_ns

let observability =
  Awskit_lwt.Observability.create
    ~clock:monotonic_ns
    ~metric_sinks:[ application_metric_sink ]
    ~trace_sinks:[ application_trace_sink ]
    ()
```

A metric sink is built with `Awskit.Observability.Metric_sink.create`. Its
`observe` callback receives one observation at a time: family metadata (name,
documentation, aggregation, unit), the family's exact label values, and one
number. That is the whole contract — there is no way for arbitrary text to
reach your sink, so you can forward observations to any metrics library
without sanitizing them. Cache your reporter's instruments by
`Metric.Family.id` and build tags from `Metric.Family.labels`.

A trace sink is built with `Awskit_lwt.Observability.Trace_sink.create` (or
the Eio equivalent). Your `start` callback receives the operation's safe
start view and returns an activation whose `within` function runs the SDK
callback inside your tracing library's promise-local or fiber-local context.
You can pass trace IDs back to Awskit through
`Awskit.Observability.Correlation`, which validates them. Both sinks are
synchronous: keep the callbacks fast and non-blocking.

Two guarantees matter when writing sinks:

- If your callback raises, the failure is contained: the SDK call returns
  whatever it would have returned anyway, and the failure is counted in
  observer health (below).
- A trace `within` function has one contract: invoke the SDK callback at
  most once, and return its result unchanged — do not store, replay, map, or
  rebuild it. Pass-through wrappers of the kind the private OpenTelemetry
  contract adapter uses satisfy this by construction. If the function
  misbehaves — calling the callback twice, skipping it, substituting its
  result, or raising — the failure is contained, counted in observer health,
  and the SDK always sees the callback's real result. What Awskit cannot
  defend is a `within` that never returns: that stalls the SDK call, so treat
  liveness as your responsibility.

## What You Will See: Operations, Attempts, And Retries

One S3 call from your code is one **operation**. Each retry iteration is an
**attempt**, and attempts are siblings under the operation:

```text
S3 operation
  attempt 1
    credentials
    signing
    HTTP request
    retry decision
  backoff
  attempt 2
    credentials
    signing
    HTTP request
```

A few consequences worth knowing when reading logs or dashboards:

- The retry decision is reported on the attempt that caused it, and that
  attempt finishes before backoff starts. Attempt durations therefore never
  include sleep time.
- Credential or signing failures belong to their attempt even when no HTTP
  request was made. Validation failures before any attempt produce one
  operation completion with zero attempts — they do not invent HTTP traffic.
- Retry scheduled and denied decisions are reported as events (not log-only
  messages) because they carry policy facts: the decision, retry class,
  replayability, chosen delay, and remaining budget.
- To compute retry amplification, divide the `awskit.s3.attempts` counter
  rate by the `awskit.s3.operations` counter rate. Awskit deliberately does
  not emit a point-in-time ratio.
- Convenience helpers such as `get_string` or `exists` produce exactly one
  operation, and paginated listing produces one operation per page fetch.

High-level file transfers and presigned-URL generation follow the same model:
a transfer is one operation with byte and part summaries, and a presign is
reported as an artifact generation with its own source and metrics, never as
an HTTP request.

## Phases, Connections, And Byte Counting

The Lwt and Eio adapters measure four boundaries directly, because they own
them:

- producing a streaming request body;
- waiting for response headers;
- the caller reading the response body; and
- draining or discarding the rest of the response body.

Awskit does not report DNS, TCP, TLS, connection-pool, or time-to-first-byte
phases: the Cohttp connectors do not expose them, and Awskit does not infer
timings it cannot observe. The measured intervals may overlap.

Byte counts follow the same honesty rules:

- Per-attempt request and response byte counts describe bytes handed to or
  pulled by the connector — not socket or wire bytes.
- A static string or bytes request body stays a static connector body, so it
  has no per-attempt connector byte count; its descriptor length still
  appears as the operation's logical request size.
- An operation's logical request bytes describe your payload once, and its
  logical response bytes count only data your code successfully received.
  Error bodies, retried attempts, and cleanup drains never inflate it.

Cleanup guarantees depend on the Lwt backend you choose. `Awskit_lwt.Make
(Client)` works with any `Cohttp_lwt.S.Client`, but that interface exposes no
way to abort an in-flight call, so a client that ignores `Lwt` cancellation
cannot be stopped while waiting for headers. `Awskit_lwt.For_connector.Make`
accepts connectors that do own their calls. The ready-made `awskit-lwt-unix`
adapter opens one fresh connection per HTTP call and closes it at end of body
or on abandonment; it does not pool connections. The Eio adapter scopes every
call in its own switch, so cleanup and cancellation behave the same way on
every path.

## Metrics Reference

Every metric family has a fixed set of labels, and every label value comes
from a small declared set — no free-form strings, no optional label slots, no
placeholder values. Families that would sometimes lack a value are split
instead: HTTP status has its own family because failed requests have no
status, and credential source has its own family because a failed resolution
may have no safe source.

| Families | Aggregation | Labels |
| --- | --- | --- |
| `awskit.s3.operations`, `awskit.s3.operation.duration` | counter, histogram | `aws.operation`, `outcome` |
| `awskit.s3.logical_request_bytes`, `awskit.s3.logical_response_bytes` | histogram | `aws.operation` |
| `awskit.s3.operations_in_flight` | gauge | `aws.operation` |
| `awskit.s3.attempts`, `awskit.s3.attempt.duration` | counter, histogram | `aws.operation`, `outcome`, `request.replayability` |
| `awskit.s3.attempt_failures` | counter | `aws.operation`, `retry.class`, `request.replayability` |
| `awskit.s3.attempts_in_flight` | gauge | `aws.operation` |
| `awskit.s3.retry.decisions` | counter | `aws.operation`, `retry.decision`, `retry.class`, `request.replayability` |
| `awskit.s3.retry.delay` | histogram | `aws.operation`, `retry.class` |
| `awskit.s3.retry.remaining_budget` | histogram | `aws.operation`, `retry.decision` |
| `awskit.s3.signing.duration` | histogram | `aws.operation`, `outcome` |
| `awskit.s3.artifacts`, `awskit.s3.artifact.duration` | counter, histogram | `aws.artifact_operation`, `outcome` |
| `awskit.s3.artifacts_in_flight` | gauge | `aws.artifact_operation` |
| `awskit.s3.artifact.signings`, `awskit.s3.artifact.signing.duration` | counter, histogram | `aws.artifact_operation`, `outcome` |
| `awskit.http.attempts`, `awskit.http.attempt.duration` | counter, histogram | `http.request.method`, `outcome` |
| `awskit.http.responses` | counter | `http.request.method`, `http.response.status_class` |
| `awskit.http.connector_request_bytes`, `awskit.http.connector_response_bytes`, `awskit.http.connector_drained_bytes` | histogram | `http.request.method` |
| `awskit.http.attempts_in_flight` | gauge | `http.request.method` |
| `awskit.http.streaming_bytes_in_flight` | gauge | `direction` |
| `awskit.http.request_body.production.duration`, `awskit.http.response_headers.wait.duration`, `awskit.http.response_body.consumption.duration`, `awskit.http.response_body.drain.duration` | histogram | `http.request.method`, `outcome` |
| `awskit.http.request_body.production.bytes`, `awskit.http.response_body.consumption.bytes`, `awskit.http.response_body.drain.bytes` | histogram | `http.request.method` |
| `awskit.credentials.resolutions`, `awskit.credentials.resolution.duration` | counter, histogram | `outcome` |
| `awskit.credentials.resolved` | counter | `credentials.source` |
| `awskit.s3.transfers`, `awskit.s3.transfer.duration` | counter, histogram | `transfer.direction`, `outcome` |
| `awskit.s3.transfer.logical_bytes`, `awskit.s3.transfer.parts` | histogram | `transfer.direction` |
| `awskit.s3.transfers_in_flight` | gauge | `transfer.direction` |

Current in-flight values for the gauge families are available on demand from
the observer (`snapshot`), so a scraper can poll at whatever cadence your
metrics setup prefers; Awskit does not run a polling loop for you.

## Units And Backend Conventions

Duration measurements are int64 nanoseconds (`ns`) and byte measurements are
UCUM bytes (`By`), declared on each metric family and measurement so a sink
always knows what it received. Awskit keeps nanoseconds in its own contract
because they are lossless and match OCaml's `Mtime` conventions; conversion
to backend conventions happens once, in your sink:

- **OpenTelemetry** semantic conventions express durations in seconds.
  Convert nanosecond values to float seconds and declare unit `s` when
  building instruments, as the private OpenTelemetry contract adapter does.
- **Prometheus** expects base units encoded in metric names. Convert
  durations to seconds and expose duration families with a `_seconds`
  suffix (for example `awskit_s3_operation_duration_seconds`); byte
  families keep their values and take a `_bytes` suffix.
- The `Metrics` library records units as source metadata only, so
  nanosecond values flow through unchanged there.

## Tracing And OpenTelemetry

A trace sink receives one span-shaped view per operation. If you map spans to
OpenTelemetry, the natural mapping is: S3 operations become client spans
named `S3.<Operation>` (for example `S3.GetObject`), physical HTTP requests
become client spans named by method, and attempts, credentials, signing,
phases, transfers, and presign generations become internal spans. Retry
decisions are span events on the attempt that caused them. Caller
cancellation is not an error status; a failed HTTP response keeps its
status-derived error.

For safety, spans never carry URLs, paths, query strings, or endpoint values,
so presigned URLs, signatures, and tokens can never leak through tracing.
That also means full conformance with the OpenTelemetry HTTP semantic
conventions is intentionally out of reach: attributes that would require
those values are simply absent.

## What Can Never Appear In Telemetry

Awskit separates three kinds of values and treats them differently:

- **Labels** on metrics come only from small declared sets (operations,
  outcomes, retry classes, status classes, methods, directions). Cardinality
  stays bounded no matter what your buckets and keys are named.
- **Diagnostics** such as AWS request IDs, host IDs, and HTTP status codes
  may appear in logs and traces after validation, but never become metric
  labels.
- **Sensitive values** — credentials, authorization material, tokens,
  signatures, presigned URLs, signed query parameters, raw provider error
  bodies, and arbitrary exception text — never appear in any signal, and
  there is no public API that could promote them into one.

If a provider sends a malformed request ID (for example containing control
characters), Awskit drops it rather than forwarding it to your logs.

## Checking Sink Health

Sinks can fail without affecting SDK results, so each observer keeps a small
set of failure counters you can inspect:

```ocaml
let snapshot = Awskit_lwt.Observability.health observability

let failures = Awskit.Observability.Health.failures snapshot
```

Each failure names its sink (by the `name` you gave it), a phase
(`Enablement`, `Start`, `Finish`, `Event`, `Instrument`, or `Context`), and a
saturating count. Snapshots contain counters only — never exception text or
request data — so it is safe to export them, using only the projection kind
and phase as dimensions. A health snapshot is never itself logged or counted
through the same observer.

## Shutting Down

Observers hold no background resources, so there is no `shutdown` to call on
them. When your application exits: stop making SDK calls first, then flush
and shut down the reporter, exporter, or tracer handles you created.

## Advanced: Runtime And Service Integration

Most applications only need the modules above. Three additional interfaces
exist for authors of SDK extensions, and ordinary application code should not
need them:

- `Awskit.Observability.For_service` lets a future service package (say, an
  EC2 client) describe its own operations, metric families, and log policies
  with the same guarantees S3 gets.
- `Awskit.Observability.For_runtime` is how the Lwt and Eio packages supply
  timing, context, and cancellation behavior; you only touch it when writing
  a new runtime adapter.
- `Awskit_s3.Observability.Make` and `.For_simulator` wire those runtimes
  and the simulator into the S3 client. `Awskit_s3.Observability.For_transfer`
  observes the high-level upload and download boundaries.

These composition roles are expert surface, and they evolve faster than the
application-facing modules: while Awskit is pre-1.0, `For_service`, the
`For_runtime` functor contracts, and the `Awskit_s3.Observability`
composition roles may change between releases, with notice in the changelog.
The application-facing modules — `Sources`, `Metric_sink`, `Trace_sink`,
`Correlation`, `Health`, `Logs_tags`, and the `For_projection` views those
deliver — remain stable across releases; any change there is called out
explicitly in the changelog.

The simulator keeps its own per-connection record of operation completions,
available as `Awskit_s3_sim.observations`, which test suites can assert
against without any observer configured.
