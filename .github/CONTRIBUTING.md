# Contributing to updoc

Thanks for helping improve `updoc`. All changes go through pull requests against
`main`.

## Reporting issues

Open an issue on GitHub with:

- What you ran and what you expected vs. what happened.
- A minimal reproducible example or package layout when possible.
- Your `sessionInfo()`, `rmarkdown::pandoc_version()`, and any relevant
  `build_log` excerpt.

## Pull request workflow

1. Create a focused branch from `main` (for example, `fix/...`, `feat/...`, or
   `docs/...`).
2. Keep changes scoped and add tests in `tests/testthat/` (for R behavior) or
   `tests/browser/` (for the preview popup, live reload, or mascot).
3. Update `NEWS.md` for user-facing changes (under `# updoc (development version)`
   between releases).
4. Format R code with [Air](https://posit-dev.github.io/air/) (`air format .`)
   and keep formatting-only changes in dedicated `style:` PRs rather than mixing
   them into feature or bugfix diffs.
5. Do not hand-edit generated files:
   - Edit roxygen comments in `R/` and run `roxygen2::roxygenise()` instead of
     editing `man/*.Rd` or `NAMESPACE`.
   - Edit `inst/mascot/` or `inst/preview/` and run
     `node tools/generate-doc-assets.cjs` instead of editing generated copies in
     `pkgdown/assets/doc-assets/` or `man/figures/`.
6. Use a concise Conventional Commit PR title (e.g., `fix: ...`, `feat: ...`,
   `docs: ...`), as PRs are squash-merged using the PR title.

## Local verification

Run the pre-PR script from the repository root:

```sh
./tools/check-pr.sh        # fast checks (formatting, assets, browser, roxygen, unit tests)
./tools/check-pr.sh --full # adds integration tests and R CMD check --as-cran
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

To preview website changes locally without modifying tracked files:

```r
updoc::preview()
```

## Code of Conduct

Please note that this project follows the
[Code of Conduct](CODE_OF_CONDUCT.md). By participating, you agree to abide by
its terms.
