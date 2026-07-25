# Observability

Awskit models each operation once as a typed lifecycle and projects its
completion to Logs, metrics, and traces. Applications choose projections per
client and own every process
resource around them: Logs reporters and levels, metric reporters, tracers,
meters, exporters, sampling, queues, transport, flushing, and shutdown.

The released package graph does not include Metrics, Trace, OpenTelemetry, or
an Awskit observability PPX or syntax-extension package. Private adapters under
`test/observability/projections` compile and run against those libraries to
prove the public sink contracts without turning one telemetry stack into an SDK
dependency. The repository's existing deriving and protocol PPXs are unrelated
implementation dependencies.

## Start With Logs

`default ()` creates fresh per-client observer state with the built-in Logs
projection enabled. The application still installs the reporter and selects
levels on Awskit's sources:

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

`Awskit_eio.Observability.default ()` is the direct-style equivalent. Pass
`Awskit_lwt.Observability.none` or `Awskit_eio.Observability.none` when a client
must have a hard-off fast path: no source check, payload builder, clock read,
context activation, health mutation, or sink callback runs.

The sources are stable values owned by their domains:

| Source value | Logs source | Meaning |
| --- | --- | --- |
| `Awskit_s3.Observability.Sources.operation` | `awskit.s3.operation` | One caller-visible S3 operation |
| `.attempt` | `awskit.s3.attempt` | One retry iteration |
| `.signing` | `awskit.s3.signing` | One S3 signing operation |
| `.retry` | `awskit.s3.retry` | A scheduled or denied retry decision |
| `.transfer` | `awskit.s3.transfer` | One high-level upload or download |
| `.artifact` | `awskit.s3.artifact` | One connection-bound presigned-artifact generation |
| `.artifact_signing` | `awskit.s3.artifact.signing` | The signing child of one presigned artifact |
| `Awskit.Observability.Sources.http` | `awskit.http` | Physical HTTP attempts and exposed body phases |
| `.credentials` | `awskit.credentials` | Credential resolution |

Definitions choose their own level and lazy human message from the terminal
outcome. Routine successes are normally suppressed, cancellation and expected
not-found/conflict outcomes can remain at Debug, throttling uses Warning, and
terminal failures use Error. Call sites do not choose levels or emit a second
completion message. Typed safe values are attached with
`Awskit.Observability.Logs_tags.operation_completion` or `.event`; Logs records
are never counted as metrics.

## Add Metrics Or Traces

Create sinks at the application edge and inject them into a client observer:

```ocaml
let monotonic_ns () = Mtime_clock.now () |> Mtime.to_uint64_ns

let observability =
  Awskit_lwt.Observability.create
    ~clock:monotonic_ns
    ~metric_sinks:[ application_metric_sink ]
    ~trace_sinks:[ application_trace_sink ]
    ()
```

`Awskit.Observability.Metric_sink.create` receives exact metric observations.
Its view contains family metadata, the family-specific finite labels, and one
numeric value; the type has no diagnostic accessor. A bridge should cache its
reporter instrument by `Metric.Family.id`, verify descriptors when names are
shared, and construct exactly the tags returned by `Metric.Family.labels`.
It must not match operation names to recreate a universal label bag.

`Awskit_lwt.Observability.Trace_sink.create` and
`Awskit_eio.Observability.Trace_sink.create` receive safe operation/event
views. Their activation installs the telemetry library's promise-local or
fiber-local context around the supplied callback and returns validated trace
correlation through `Awskit.Observability.Correlation`. The callbacks can only
receive `Diagnostic.Public.t`; raw diagnostic constructors and sensitive
diagnostics are outside the projection interface.

Sink exceptions are contained and never replace an SDK value, error,
exception, or native cancellation. The runtime also defends callback invocation
and result semantics when a trace wrapper invokes more than once, substitutes
a result, returns without invoking, or raises. A wrapper that does not return
can still stall the operation, even if it already invoked the callback, so
wrappers are defended for invocation and result semantics and trusted for
liveness.

Observer values own no asynchronous resources and therefore have no misleading
`shutdown` function. Stop SDK use first, then flush and shut down the handles
owned by the chosen reporter or exporter.

## Inspect Observer Health

Projection failures are observer-local and bounded. A snapshot lists configured
projection identities and non-zero counters keyed by projection plus one phase:
`Enablement`, `Start`, `Finish`, `Event`, `Instrument`, or `Context`.

```ocaml
let snapshot = Awskit_lwt.Observability.health observability

let failures = Awskit.Observability.Health.failures snapshot
```

Each failure exposes its projection's local integer ID, kind, display name,
phase, and saturating count. The display name is for operators; it is never a
metric dimension or canonical field. Health stores no exception text, request
ID, host ID, sink tag, or other unbounded failure data. If an application
exports observer health, use only the closed projection kind and phase as
dimensions.

## Operation Topology

One caller-visible S3 operation may own several physical attempts:

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

The retry decision is emitted while its causing attempt is current. That
attempt closes before backoff starts, and the next attempt is its sibling, so
attempt duration never includes sleep. Credential or signing failure still
belongs to the attempt even when no physical HTTP request is made.

A timed operation has one terminal completion, which may produce a conditional
completion log, count, duration/value distributions, and a span. Retry
scheduled or denied is a separate event because the policy decision and chosen
action are facts of their own. Operations, attempts, transfers, and streaming
bytes in flight are lifecycle-owned gauges rather than synthetic start/finish
events.

## Connector Phases, Cleanup, And Byte Accounting

The Lwt and Eio adapters bracket four boundaries they directly own:

- execution of a streaming request-body producer;
- the connector call until response headers are returned;
- caller response-body pulls; and
- runtime cleanup or explicit-discard pulls.

These intervals may overlap. Awskit does not add or subtract them to infer DNS,
TCP, TLS, pool, socket, queue, or TTFB timing, because the current Cohttp
connectors do not expose those lower boundaries.

Physical HTTP completions report bytes handed to or pulled by the configured
connector boundary, split between caller-consumed response bytes and cleanup or
explicit-discard bytes. These values are not socket or wire byte claims, and
phase completions report only their own bytes.

Native static Lwt and Eio request bodies retain their Cohttp representation, so
their per-attempt connector request count is absent even when the descriptor
supplies a logical length. Streaming request bodies report bytes handed to the
connector producer path in their production phase, while connector request
bytes count only bytes the connector actually pulls. Caller-consumed response
pulls and cleanup or explicit-discard pulls remain separate measurements.

A logical S3 completion reports request bytes once and response bytes only when
the caller-visible operation succeeds. Retry responses, service-error bodies,
failed decodes, explicit discard, and automatic cleanup drain never inflate the
logical response value. High-level transfers similarly report logical bytes
and parts while their constituent requests retain independent connector-boundary
accounting.

Cleanup guarantees depend on the selected Lwt connector contract.
`Awskit_lwt.Make (Client)` accepts any `Cohttp_lwt.S.Client`, whose interface
does not expose an in-flight request or response connection to abort. Awskit
preserves the primary SDK result, exception, or `Lwt.Canceled` and performs no
detached background drain, but it cannot promptly stop a client that ignores
Lwt cancellation while response headers are pending, or guarantee reuse of an
abandoned response connection after bounded cleanup fails.

`Awskit_lwt.For_connector.Make` is the expert alternative for connectors that
own an in-flight call. Its idempotent abort is awaited before the physical HTTP
attempt returns, so cancellation or cleanup cannot advance retry ahead of
connector termination. The built-in `Awskit_lwt_unix` adapter implements that
contract with one exclusive fresh lower-level Cohttp connection per call,
matching Cohttp 6.2.1's no-cache baseline. It does not pool connections and
closes each one at response EOF or abandonment. The Eio adapter similarly owns
each physical call in a nested switch, so body cleanup and switch teardown
finish before the attempt leaves while native cancellation remains native.

## Exact Metric Families

Every label comes from a finite enum declared by the owning domain. Every
sample contains exactly the labels in its family, in declaration order; there
are no optional slots or `"none"` fillers.

| Families | Aggregation | Exact labels |
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

HTTP status and credential source have separate families because transport
failures have no status and unsuccessful resolutions may have no safe source.
S3 attempt retry class is likewise a failure-only family rather than a made-up
success value. Retry amplification is the aligned rate of
`awskit.s3.attempts` divided by `awskit.s3.operations`; Awskit does not emit a
point-in-time ratio sample.

## Tracing And OpenTelemetry

The private OpenTelemetry projection adapter names logical client spans
`S3.<Operation>` and maps them to `rpc.system.name = "aws-api"` plus the same
fully qualified `rpc.method`, for example `S3.GetObject`. Physical HTTP client
spans use the HTTP method as their name; S3 attempts, credentials, signing,
presigned-artifact generation, artifact signing, and connector phases are
internal spans.
Retry decisions are span events on the causing attempt. Caller cancellation
does not set error status, while a failed physical HTTP response retains its
status-derived error type.

The adapter intentionally omits raw URLs, paths, query strings, and arbitrary
endpoint values because presigned URLs, signatures, and tokens must never
cross the canonical safe interface. It also omits server/region attributes
that the current canonical model cannot provide safely, and it does not invent
lower transport phases. This is a deliberately incomplete safe mapping of the
current
[AWS SDK](https://opentelemetry.io/docs/specs/semconv/cloud-providers/aws-sdk/)
and [HTTP span](https://opentelemetry.io/docs/specs/semconv/http/http-spans/)
conventions rather than full HTTP semantic-convention conformance: required
HTTP target attributes are intentionally unavailable at the safe projection
boundary.

## Diagnostics And Cardinality

Dimensions, measurements, and diagnostics are different OCaml types.
Operations and finite status/retry classes are bounded dimensions; durations,
bytes, counts, delays, and budgets are numeric measurements; request IDs, host
IDs, safe status codes, and trace correlation are diagnostics.

Only `Diagnostic.Public.t` reaches Logs or trace callbacks. Sensitive values are
removed before that interface is built, and metric sinks cannot name a
diagnostic API. Credentials, authorization material, tokens, signatures, raw
presigned URLs, and unredacted exception text are prohibited from canonical
observations entirely. Bucket names, object keys, request IDs, host IDs, and
endpoint URLs never become metric labels.

## Extension Roles

Ordinary applications use runtime `Observability` modules, source values,
`Metric_sink.create`, safe `Trace_sink.create`, Logs tags, and health snapshots.
The remaining roles are explicit expert contracts:

- `Awskit.Observability.For_service` lets future service packages own typed
  definitions, exact metric families, lazy log policy, events, and instruments.
- `Awskit.Observability.For_runtime` supplies context and finalization
  contracts, semantic observer composition, and adapter-owned HTTP definitions;
  its lifecycle engine and scope types remain sealed.
- `Awskit.Observability.For_projection` is a read-only, public-diagnostic-only
  view for application adapters.
- `Awskit_s3.Observability.For_runtime` and `.For_simulator` are narrow roles
  required by the existing S3 sibling packages.

There is no Awskit observability syntax extension: definitions are ordinary
typed module values, and production call sites use small semantic wrappers such
as `with_operation`, `with_attempt`, `with_signing`, and `emit_retry`. This
keeps service logic visible without adding observability PPX build coupling or
exposing lifecycle machinery to applications.

The simulator records `Awskit_s3_sim.observations` in a per-connection terminal
completion stream that is independent of request history. Each entry has the
same safe logical completion shape as the real S3 client, while its physical
attempt measurement is absent because the simulator performs no transport.
