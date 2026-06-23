# Testing And Validation

This document defines how Awskit changes should be tested and validated.

## Test Current Behavior

Tests should protect the current supported contract. When removing old
behavior, do not add tests that only prove the old symbol, option, or module is
gone.

Use these replacements instead:

- Test the new API that replaces the old one.
- Test the migration-sensitive behavior users rely on now.
- Test the regression symptom that motivated the change.
- Test public boundaries through exposed modules.
- Let `dune build` prove removed symbols are no longer available.

Good:

```ocaml
let test_object_get_uses_scoped_reader () =
  (* Verify the supported reader workflow. *)
  ()
```

Bad:

```ocaml
let test_removed_legacy_object_api_no_longer_exists () =
  (* Tombstone tests for removed symbols do not belong in the suite. *)
  ()
```

## Regression Fixes

For real bugs:

1. Reproduce the failure.
2. Add a focused regression test for the supported behavior.
3. Verify the test fails before the fix when practical.
4. Implement the narrow fix.
5. Run the focused test and the failing CI-equivalent command.
6. Update `CHANGES.md` if the fix belongs in the release notes.

For resource fixes, test both the normal path and the callback or IO failure
path when the failure mode is observable.

## OCaml Test Shape

Tests should cover behavior through the public surface whenever practical.
Implementation-level tests are acceptable when the behavior cannot be reached
through a public API without making the public API worse.

- Put the bulk of tests under `test/`, not inside production libraries.
- Keep tests deterministic and fast unless they are explicitly integration or
  contract tests.
- Use Alcotest assertions that show useful values on failure.
- Use property tests for parsers, formatters, validation boundaries, and
  round-trips when examples alone would miss important combinations.
- Use examples or trace-style tests for workflows whose behavior is easier to
  understand from a short execution story.

## Check Selection

Use the narrowest check that proves the change, then run broader checks when
the change affects shared behavior, packaging, CI, or releases.

Common commands:

```sh
opam exec -- dune fmt
opam exec -- dune build
opam exec -- dune test
opam exec -- dune build @doc
opam exec -- dune build @opam
git diff --check
```

For S3 contract work:

```sh
docker compose up -d
opam exec -- dune build --force @minio-contract
docker compose down -v
```

For runtime contract work:

```sh
opam exec -- dune build @runtime-conformance
```

For releases:

```sh
scripts/release-check.sh
```

If a broad command is too expensive for the current change, run the focused
command that proves the touched behavior and clearly state what was not run.
