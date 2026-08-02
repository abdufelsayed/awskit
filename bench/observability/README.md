# Observability Overhead Baseline

`observability_overhead.exe` measures the per-call cost of the observation
paths directly, without any S3 transport: one synthetic operation completion
per iteration for each observer configuration, plus retry-fan-out and
instrument-lease paths. Run it with:

```sh
opam exec -- dune build @observability-bench
```

Set `AWSKIT_OBSERVABILITY_BENCH_ITERATIONS` to change the iteration count
(default 100,000).

## What Each Mode Measures

| Mode | Path measured |
| --- | --- |
| `hard-off` | `none` observer: one operation call with no source checks, clock reads, field encoding, or sink dispatch. |
| `logs-success` | Logs projection, successful operation, level enabled, message forced through a nop reporter. |
| `logs-failure` | Logs projection, failed operation, same enabled path. |
| `metrics` | One metric sink with clock: counter plus duration histogram per completion. |
| `trace` | One trace sink with clock: start, context `within`, finish per completion. |
| `combined` | Logs, metric, and trace projections together on one completion. |
| `retry` | Full fan-out per iteration: 1 operation + 3 attempts x (credentials + signing + HTTP) + 2 retry events = 14 completions/events. |
| `instrument` | Gauge lease acquire plus release. |
| `streaming-bytes` | Gauge lease acquire, two counter adjustments, release. |

## Recorded Baseline

Machine: Apple M3 Max (arm64), macOS 26.5.2, OCaml 5.3.0, 100,000
iterations per mode. Timings vary roughly ±10% between runs; allocation
figures are stable across runs.

| Mode | ns/op | words/op |
| --- | --- | --- |
| hard-off | 4.8 | 7.9 |
| logs-success | 246.9 | 602.9 |
| logs-failure | 260.2 | 605.5 |
| metrics | 328.3 | 516.4 |
| trace | 271.0 | 610.8 |
| combined | 529.3 | 946.3 |
| retry | 4095.4 | 8721.4 |
| instrument | 45.7 | 81.3 |
| streaming-bytes | 93.9 | 131.1 |

Reading the numbers:

- The `none` observer costs single-digit nanoseconds and a handful of words
  per call — the residual call closures documented in
  `docs/observability.md`. There is no hidden work on the hard-off path.
- `retry` divides back down to roughly 290 ns per completion/event
  (14 per iteration), consistent with the single-projection modes.
- These are microbenchmarks of the observation engine, not of SDK requests;
  real operations are dominated by transport. Record new baselines on the
  same machine when the engine or projection hot paths change; treat the
  values as indicative evidence, not a CI gate.
