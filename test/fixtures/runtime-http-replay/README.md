# Runtime HTTP Replay Corpus

This directory is reserved for reduced runtime HTTP workload failures. Replay
fixture paths are the replay identifiers. Do not add a central manifest,
central registry, evidence-id catalog, compatibility alias, or old-name layer.

Each replay file uses a small line-oriented format owned by
`test/support/runtime_http_replay.ml`. String payloads are encoded as `h`
followed by lowercase hex bytes so reduced malformed wire data can be reviewed
without escaping ambiguity.

```text
runtime-http-replay-v1
name=<case-name>
method=<GET|HEAD|PUT|POST|DELETE|PATCH>
status=<http-status>
headers=<count> <hex-name> <hex-value> ...
framing=<framing-directive>
connection=<close|keep-alive>
consume=<read-all|read-once:n|drop-without-read|raise-in-consume>
```
