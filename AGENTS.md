# Working on updoc

`updoc` is an R package providing a live development server and non-destructive
build checks for `pkgdown` sites.

## Repository map

- `R/` — Package source:
  - `api.R`: `serve()`, `preview()`, preview handle methods (`$status()`, `$wait()`, `$rebuild()`, `$open()`, `$stop()`).
  - `check.R`: `check()` one-shot non-destructive site check (`format = "text"` or `"json"`).
  - `deploy.R`: `build()` deployable site builder.
  - `server.R`: local `httpuv` server, HTML injection, and `/__updoc__/*` endpoints.
  - `preview-worker.R`, `build.R`, `build-setup.R`, `build-pkgdown.R`, `articles.R`: mirai background worker, rebuild planning, and targeted/full builds.
  - `build-output.R`: build log capture and Pandoc 3.11 deprecation noise filter.
  - `diagnostics.R`: structured error/warning parsing and source excerpts.
  - `pandoc.R`, `paths.R`, `state.R`, `validate.R`: Pandoc resolution, file watching/mirroring, state machine, input validation.
- `inst/mascot/` — Source of truth for Doc, the SVG/JS/CSS mascot.
- `inst/preview/` — Browser status popup and live-reload scripts (`panel.js`, `panel-primitives.js`, `panel.css`, `reload.js`).
- `inst/skills/updoc/SKILL.md` — Bundled agent skill for using `updoc` on R packages.
- `tests/testthat/` — R unit and integration tests (`UPDOC_INTEGRATION_TESTS=true` enables live builds).
- `tests/browser/` — Node.js regression tests for the browser panel, reload controller, and mascot.
- `pkgdown/`, `vignettes/articles/`, `_pkgdown.yml` — Documentation website sources.
- `tools/` — Mascot asset generator (`generate-doc-assets.cjs`), Chromium rendering QA (`doc/check-rendering.cjs`), vendored `tools/vendor/flexokidown` theme, and `tools/README.md`.

## Key invariants

- **Never mutate the user's package during `serve()`, `preview()`, or `check()`.** Always build in a mirrored temporary copy; only `build()` writes to `man/`, `README.md`, or `docs/`.
- **Never hand-edit `man/*.Rd` or `NAMESPACE`.** Edit roxygen comments in `R/` and run `roxygen2::roxygenise()`.
- **Never hand-edit generated mascot or preview copies** in `pkgdown/assets/doc-assets/` or `man/figures/`. Edit `inst/mascot/` or `inst/preview/` and run `node tools/generate-doc-assets.cjs` (preserving generated-block markers in CSS).
- **Keep `R CMD check --as-cran` clean** (0 errors, 0 warnings; only the expected "New submission" note).
- **Preserve licenses and notices** in `inst/COPYRIGHTS` and `tools/vendor/flexokidown/`.

## Pull request workflow

- **Work on a branch or worktree and open a pull request against `main`.** Do not push directly to `main`.
- **Use Conventional Commit PR titles** (`feat:`, `fix:`, `docs:`, `style:`, `ci:`, `chore:`); PRs are squash-merged with the PR title.
- **Format R code with Air (`air format .`)** using `air.toml`, and keep bulk formatting-only changes out of feature/fix PRs.
- **Update `NEWS.md` for user-facing changes** (under `# updoc (development version)` between releases).
- **Check `git diff` before opening a PR** for stray generated files (such as `docs/`, unintended `man/` edits, or unnecessary `Config/roxygen2/version` churn in `DESCRIPTION`) and list the exact verification commands you ran in the PR description.
- **Required CI checks:** `All checks passed` (Air formatting, browser tests, generated asset check, Chromium rendering QA, roxygen drift check, and all non-`devel` `R CMD check --as-cran` matrix jobs) and `Build and check documentation` (`pkgdown`).

## Verification commands

Run from the repository root:

```sh
./tools/check-pr.sh        # fast pre-PR checks
./tools/check-pr.sh --full # full suite including integration tests and R CMD check --as-cran
```

Or run individual checks directly:

```sh
air format --check .
Rscript -e 'roxygen2::roxygenise()'
node --test tests/browser/*.test.cjs
node tools/generate-doc-assets.cjs --check
Rscript -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"), build_args = "--no-manual", error_on = "warning")'
```

For full end-to-end build, Pandoc, and HTTP integration tests:

```sh
UPDOC_INTEGRATION_TESTS=true Rscript -e 'testthat::test_local()'
```

To verify the pkgdown website without dirtying the repo:

```sh
Rscript -e 'dst <- tempfile("updoc-site-"); on.exit(unlink(dst, recursive = TRUE)); pkgdown::build_site(override = list(destination = dst), preview = FALSE, new_process = FALSE, install = FALSE)'
```
