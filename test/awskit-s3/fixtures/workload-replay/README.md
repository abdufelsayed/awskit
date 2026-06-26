# S3 Workload Replay Corpus

This directory stores reduced S3 workload failures as deterministic replay
inputs. Fixture paths are the replay identifiers. Do not add a separate
manifest, central evidence registry, compatibility alias, or old-name layer.

Each replay file uses one command per line. Blank lines and lines starting with
`# ` are ignored. Keep files minimal enough for reviewers to understand why the
case exists.

New replay entries use a versioned, line-oriented format:

```text
replay-v1 <command> <arguments...>
```

String arguments are encoded as `h` followed by lowercase hex bytes. Pair lists,
such as object tags and metadata, are encoded as `<count>` followed by encoded
key/value tokens. Optional strings are encoded as either `none` or `some
<encoded-string>`.

Supported commands mirror the generated `S3_command.t` workload language:

```text
replay-v1 put-string <key> <body> <tags>
replay-v1 put-string-metadata <key> <body> <tags> <metadata>
replay-v1 get-string <key>
replay-v1 find-string <key>
replay-v1 head-object <key>
replay-v1 exists-object <key>
replay-v1 delete-object <key>
replay-v1 list-keys
replay-v1 list-prefix <prefix>
replay-v1 list-keys-page <prefix-option> <max-keys>
replay-v1 list-versions-page <max-keys>
replay-v1 copy-object <source-key> <destination-key>
replay-v1 copy-object-metadata <source-key> <destination-key> copy-source-metadata
replay-v1 copy-object-metadata <source-key> <destination-key> replace-metadata <metadata>
replay-v1 put-object-tags <key> <tags>
replay-v1 get-object-tags <key>
replay-v1 delete-object-tags <key>
replay-v1 put-bucket-tags <tags>
replay-v1 get-bucket-tags
replay-v1 delete-bucket-tags
replay-v1 put-versioning <status>
replay-v1 get-versioning
```
