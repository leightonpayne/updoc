# Maintenance

Install development dependencies with `pak::local_install_dev_deps(".")`.

## Running checks

Run these from the repository root:

```sh
node --test tests/browser/*.test.cjs
node tools/generate-doc-assets.cjs --check
R CMD build .
UPDOC_INTEGRATION_TESTS=true R CMD check --as-cran updoc_0.1.0.tar.gz
```

## Mascot and browser checks

Edit the mascot source in `inst/mascot/`, then run
`node tools/generate-doc-assets.cjs` to update the website and README copies
(keep the generated-block markers in the stylesheets intact).

If Playwright and Chrome are available, run `node tools/doc/check-rendering.cjs`
to verify mascot motion, responsive layout, keyboard controls, and failed-rebuild
feedback. Set `NODE_PATH` for an external Playwright installation or
`CHROME_PATH` for a custom Chromium binary.

## Building the website

The site uses the bundled `tools/vendor/flexokidown` theme. Install it with
`R CMD INSTALL tools/vendor/flexokidown`, then run `updoc::build()`. When
updating the vendored theme from upstream, keep its license and notices.

## Before release

Generated output and check logs are gitignored. Before publishing, verify the
final source archive on the CI matrix, test installing it into a clean R
library, and update `cran-comments.md` with the results. Add public website and
issue URLs to `DESCRIPTION` once those destinations are live.
