# Fuzz Replay Corpus

This directory contains minimized cases discovered manually or by optional
mutation fuzzing. Every file is replayed by an ordinary deterministic Alcotest
target before it counts as fixed release evidence.

Do not add long-running fuzzers to default builds. Add the minimized failure
here, make the replay test fail, then fix the parser or validator.
