# Documentation Publishing

Awskit publishes generated odoc documentation with GitHub Pages.

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

## Workflow

The `publish-docs` job in `.github/workflows/main.yml`:

- Runs only on pushes to `main`.
- Waits for build/test, docs/examples, and MinIO contract jobs.
- Installs documentation dependencies with `opam install --with-doc --deps-only .`.
- Builds docs with `opam exec -- dune build @doc`.
- Uploads `_build/default/_doc/_html`.
- Deploys with `actions/deploy-pages`.

The job is expected to skip on release branch pushes and pull requests.

## Updating Documentation URLs

Update the source metadata in `dune-project`, then regenerate opam files:

```sh
opam exec -- dune build @opam
```

Commit the `dune-project` and generated `*.opam` changes together.

## Local Validation

Run:

```sh
opam exec -- dune build @doc @examples @docs-content
```

For release validation, `scripts/release-check.sh` also checks documentation
build output, example executables, docs content policy, and documentation
generation from the distribution archive.
