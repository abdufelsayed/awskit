# Documentation Publishing

Awskit publishes generated odoc documentation with GitHub Pages.

Related maintainer docs:

- `docs/ci.md` describes the `Publish docs` workflow in the CI map.
- `docs/release.md` includes the post-merge documentation publication check.
- `docs/release-gates.md` covers release documentation evidence.

## Public URL

```text
https://abdufelsayed.github.io/awskit/
```

Package metadata should use this URL for documentation links.

## Repository Setting

GitHub Pages must be configured in the repository settings:

```text
Settings -> Pages -> Build and deployment -> Source: GitHub Actions
```

No custom domain, `CNAME`, or Cloudflare configuration is required.

## Publish Workflow

The `Publish docs` workflow in `.github/workflows/docs.yml`:

- runs after `.github/workflows/ci.yml` succeeds on `main`;
- can be dispatched manually from `main`;
- checks out the exact commit whose `CI` workflow completed when triggered by
  `workflow_run`;
- installs documentation dependencies with
  `opam install --yes --with-doc --deps-only .`;
- builds docs with `opam exec -- dune build @doc`;
- uploads `_build/default/_doc/_html`;
- deploys with `actions/deploy-pages`.

The workflow is expected to skip on release branch pushes and pull requests.

## Updating Documentation URLs

Update source metadata in `dune-project`, then regenerate opam files:

```sh
opam exec -- dune build @opam
```

Commit `dune-project` and generated `*.opam` changes together.

## Local Validation

Run:

```sh
opam exec -- dune build @examples @doc
```

For release gates, also build examples, review generated documentation output
for warnings or unresolved references, and build documentation from the
distribution archive as described in `docs/release-gates.md`.

## Publication Check

After a release PR merges to `main`, confirm:

- `CI` completed successfully for the merged commit;
- `Publish docs` completed for the same commit;
- the public URL serves the generated documentation.
