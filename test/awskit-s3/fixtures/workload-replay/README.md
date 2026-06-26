# S3 Workload Replay Corpus

This directory stores reduced S3 workload failures as deterministic replay
inputs. File paths are the replay identifiers; do not add a separate manifest
or central evidence registry.

Each replay file uses one command per line. Blank lines and lines starting with
`# ` are ignored. Keep files minimal enough for reviewers to understand why the
case exists.
