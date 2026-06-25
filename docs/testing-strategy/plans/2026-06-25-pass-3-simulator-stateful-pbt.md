# Simulator Stateful PBT Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use
> superpowers:subagent-driven-development (recommended) or
> superpowers:executing-plans to implement this plan task-by-task. Steps use
> checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a no-network stateful property test that compares public simulator
behavior with an independent model of simple S3 object lifecycle semantics.

**Architecture:** Keep the model pure and deliberately smaller than the
simulator. Exercise the simulator only through public `Awskit_s3_sim` APIs and
public inspection helpers.

**Tech Stack:** OCaml, Dune aliases, Alcotest, QCheck, `awskit-s3-sim`,
Awskit simulator public APIs.

---

## Related Draft

For deeper rationale, model boundaries, history caveats, and fault-injection
backlog, read:

- `docs/testing-strategy/drafts/2026-06-25-simulator-stateful-pbt.md`

## No-Fix Rule

This is a test-construction pass. Do not edit simulator implementation modules.
If the model oracle exposes a simulator bug, keep the property and record the
shrunk command transcript.

Read-only production files for this pass:

- `packages/awskit-s3/sim/**/*.ml`
- `packages/awskit-s3/sim/**/*.mli`

## Files

- Create `test/awskit-s3/sim/test_simulator_stateful_pbt.ml`
- Modify `test/awskit-s3/sim/test_awskit_s3_sim.ml`
- Modify `test/awskit-s3/sim/dune`
- Modify `dune-project`
- Regenerate `awskit-s3-sim.opam`

## Task 1: Test Dependencies

- [ ] Add test-only dependencies to the `awskit-s3-sim` package in
  `dune-project`:

  ```scheme
  (qcheck-core :with-test)
  (qcheck-alcotest :with-test)
  ```

- [ ] Regenerate opam metadata:

  ```sh
  opam exec -- dune build @opam
  ```

- [ ] Confirm `awskit-s3-sim.opam` includes the new `with-test` dependencies.

## Task 2: Pure Model

- [ ] Create `test/awskit-s3/sim/test_simulator_stateful_pbt.ml`.

- [ ] Define model state with:

  - One pre-created bucket.
  - A string map from object key to object body.

- [ ] Keep the first model out of scope for:

  - Versioning.
  - Multipart.
  - Tags.
  - Metadata.
  - Checksums.
  - Fault injection.

- [ ] Use one valid bucket name for every generated scenario, such as
  `stateful-pbt-bucket`.

## Task 3: Commands And Shrinking

- [ ] Define generated command variants:

  - `Put_string (key, body)`
  - `Get_string key`
  - `Find_string key`
  - `Head_object key`
  - `Exists_object key`
  - `Delete_object key`
  - `List_keys`
  - `Copy_object (source_key, destination_key)`

- [ ] Use a small key domain:

  - `a.txt`
  - `b.txt`
  - `logs/a.txt`
  - `logs/b.txt`
  - `photos/2026.jpg`

- [ ] Use short printable bodies.

- [ ] Generate 100 to 200 scenarios.

- [ ] Generate 1 to 40 commands per scenario.

- [ ] Provide explicit shrinkers:

  - Shrink command lists by removing commands.
  - Shrink keys toward the first key in the small domain.
  - Shrink bodies by shortening strings.

- [ ] Provide a compact printer that emits a replayable transcript with command
  names, keys, bodies, and copy destinations.

## Task 4: Simulator Execution

- [ ] For each generated scenario, create a fresh simulator clock, store, and
  connection.

- [ ] Create the single model bucket before executing generated commands.

- [ ] Apply each command to the pure model.

- [ ] Apply each command through public simulator APIs:

  - `Simulator.Object.put`
  - `Simulator.Object.get_string`
  - `Simulator.Object.find_string`
  - `Simulator.Object.head`
  - `Simulator.Object.exists`
  - `Simulator.Object.delete`
  - `Simulator.Object.List.keys`
  - `Simulator.Object.copy`

- [ ] Normalize errors by structured classification:

  - `Error.is_no_such_bucket`
  - `Error.is_no_such_key`
  - service status
  - service code
  - validation field
  - error kind

- [ ] Do not compare full human error strings in the property.

## Task 5: Invariants

- [ ] After every command, compare `Simulator.objects_as_strings store ~bucket`
  with the model object map sorted by key.

- [ ] After every command, compare `Simulator.keys store ~bucket` with the
  model key set sorted lexicographically.

- [ ] Assert `Get_string` returns the modeled body for present keys.

- [ ] Assert `Get_string` returns `NoSuchKey` for absent keys.

- [ ] Assert `Find_string` returns `Ok None` only for missing objects in the
  existing bucket.

- [ ] Assert `Head_object` and `Exists_object` agree with the model without
  reading object bodies.

- [ ] Assert `Delete_object` removes present objects and succeeds for absent
  objects in the existing bucket.

- [ ] Assert failed validation or preflight cases do not mutate model-visible
  simulator state.

- [ ] Do not use `Simulator.keys` or `Simulator.objects_as_strings` alone to
  distinguish a missing bucket from an empty bucket.

## Task 6: Suite And Alias

- [ ] Add the property to an Alcotest suite ID named
  `pbt:awskit-s3-sim:simulator-stateful`.

- [ ] Register `Test_simulator_stateful_pbt.suite` in
  `test/awskit-s3/sim/test_awskit_s3_sim.ml`.

- [ ] Add `qcheck-core` and `qcheck-alcotest` to
  `test/awskit-s3/sim/dune`.

- [ ] Add a focused alias in `test/awskit-s3/sim/dune`:

  ```scheme
  (rule
   (alias simulator-stateful-pbt)
   (package awskit-s3-sim)
   (deps test_awskit_s3_sim.exe)
   (action
    (run ./test_awskit_s3_sim.exe test "pbt:awskit-s3-sim:simulator-stateful")))
  ```

- [ ] Ensure `@simulator-contract` runs the new property.

## Task 7: Validation

- [ ] Run the focused simulator property:

  ```sh
  opam exec -- dune build @test/awskit-s3/sim/simulator-stateful-pbt
  ```

- [ ] Run the simulator contract:

  ```sh
  opam exec -- dune build @simulator-contract
  ```

- [ ] Run broader protocol evidence:

  ```sh
  opam exec -- dune build @check-protocol
  ```

- [ ] Run whitespace validation:

  ```sh
  git diff --check
  ```

- [ ] If the property exposes a simulator bug, write failure evidence with the
  shrunk command transcript and do not edit simulator implementation code.

## Backlog

- Prefix and paginated listing model.
- `Delete_objects` model.
- Metadata and tags model.
- ETag precondition model.
- Versioning enabled and suspended states.
- Multipart create, upload, list, complete, and abort model.
- Fault scheduling by eligible operation index.
