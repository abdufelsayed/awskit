# Fuzz Replay Corpus

This directory contains minimized cases discovered manually or by optional
mutation fuzzing. Every file is replayed by an ordinary deterministic Alcotest
target before it counts as fixed release evidence.

Do not add long-running fuzzers to default builds. Add the minimized failure
here, make the replay test fail, then fix the parser or validator.

Each replay input has an expected sidecar describing the asserted error
category and retry class. Keep those sidecars in sync with the public error
classification, not with incidental printer wording.
