# Runtime HTTP Replay Corpus

This directory is reserved for reduced runtime HTTP workload failures. Replay
paths identify evidence; do not add a central manifest or separate central
registry.

Runtime HTTP replays are currently typed scenarios in
`test/support/runtime_http_replay.ml` because the shared workload model already
owns the response framing language. Add path-scoped fixture files here only
when a reduced case needs opaque bytes that should be reviewed outside OCaml.
