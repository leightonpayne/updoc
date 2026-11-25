---
name: updoc
description: Preview, check, and build R package pkgdown documentation sites using updoc. Use when editing R roxygen docs, vignettes, README.Rmd, NEWS.md, or _pkgdown.yml and verifying or previewing the rendered site.
---

# Using updoc

`updoc` builds and previews `pkgdown` sites for R packages. Previews (`serve()`, `preview()`) and one-shot checks (`check()`) run in an isolated temporary copy so your package's `man/`, `NAMESPACE`, `README.md`, and `docs/` files stay untouched. Only `build()` writes directly to the package.

## 1. One-shot verification (recommended for edit-verify loops)

To verify that roxygen docs, `README.Rmd`, vignettes, and the `pkgdown` site all build cleanly without modifying files in the working tree:

```sh
Rscript -e 'updoc::check(format = "json")'
```

- Exits `0` on success and non-zero on failure.
- Prints one JSON object to stdout with `ok`, `package_root`, `duration`, `warnings`, `error` (`summary`, `location`, `cause`, `details`, `hint`, `excerpt`), and `build_log`.
- To inspect the JSON object without a non-zero exit status, pass `error = FALSE`.
- Pass `document = FALSE` if the package manages `man/` files without roxygen2.

## 2. Live preview server (for repeated edits or browser inspection)

Start the server in the background with an automatic free port (`port = 0`) and no browser popup:

```sh
Rscript -e 'updoc::serve(port = 0, browse = FALSE, quiet = TRUE)'
```

Read the printed URL (for example `http://127.0.0.1:54321`), then interact over HTTP:

- **Wait for builds to settle (after startup or saving edits):**
  ```sh
  curl -s "http://127.0.0.1:<port>/__updoc__/status?wait=1&timeout=60"
  ```
  `?wait=1` immediately checks for any just-saved files, starts/finishes the rebuild, and returns JSON with `phase` (`"ready"`, `"building"`, `"error"`), `url`, `rebuild_count`, `last_success`, `changed`, `warnings`, `error`, and `build_log`.
  Optionally pass `&after=<rebuild_count>` to wait until a newer build succeeds or fails.
- **Read the full unfiltered build log:**
  ```sh
  curl -s "http://127.0.0.1:<port>/__updoc__/log"
  ```
- **Trigger a manual rebuild (add `?full=true` for a full rebuild):**
  ```sh
  curl -s -X POST -H "X-Updoc-Request: rebuild" "http://127.0.0.1:<port>/__updoc__/rebuild?full=true"
  ```
- **Inspect rendered HTML** by opening `http://127.0.0.1:<port>/` in a browser tool or fetching pages directly.

## 3. From a persistent R session

```r
site <- updoc::preview(port = 0, browse = FALSE, quiet = TRUE)
status <- site$wait(timeout = 60)   # Wait for initial build or latest edit
site$rebuild(full = TRUE)
status <- site$wait(timeout = 60)
site$stop()
```

## 4. Building the final site to publish

When ready to update generated package docs, render `README.Rmd` to `README.md`, and write the final `pkgdown` output folder (usually `docs/`):

```sh
Rscript -e 'updoc::build()'
```

If `build()` fails, the error message includes `Build log: <path>` with the full unfiltered output.

## Troubleshooting

- **Pandoc:** `updoc` requires Pandoc 2.10.1 or newer. To use a specific installation, pass `pandoc = "/path/to/bin"` (the folder containing the `pandoc` binary, not the binary itself) or set `options(updoc.pandoc = "/path/to/bin")`.
- **Full logs:** `updoc` hides two harmless Pandoc 3.11 deprecation warnings (`--mathml`, `--mathjax`) from normal output; they are always kept in `build_log` and `GET /__updoc__/log`.
