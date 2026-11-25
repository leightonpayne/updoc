# updoc 0.1.0

First release of updoc, a live preview server for pkgdown sites.

* `serve()` builds your pkgdown site, watches for saved changes, and refreshes
  your browser automatically. Press Ctrl+C to stop. Pass `port = 0` to pick
  any free local port automatically.
* `preview()` runs the same live preview in the background so you can keep
  working in the R console, returning a handle with `$status()`, `$wait()`,
  `$rebuild()`, `$open()`, and `$stop()`.
* `check()` runs a one-shot non-destructive site check in a temporary copy,
  with optional `format = "json"` output for CI and AI agents.
* `build()` updates your package's generated docs, renders `README.Rmd` when
  present, and writes the finished site to your pkgdown output folder for
  publishing. Errors include the `Build log:` path directly in the message.
* Previews build in a temporary copy, leaving your package's `man/` files,
  `README.md`, and `docs/` folder untouched. If a build fails, the last working
  preview stays up until you fix the problem and save again.
* Local preview HTTP endpoints (`/__updoc__/status` with optional `?wait=1`,
  `/__updoc__/log`, and `POST /__updoc__/rebuild?full=true`) plus a bundled
  `SKILL.md` make updoc easy to drive from scripts and AI agents.
* The in-browser status popup tracks build progress, surfaces warnings and
  errors with source context, includes a manual **Rebuild** button, and
  supports light/dark themes, keyboard navigation, and reduced motion.
* Builds use your installed Pandoc (2.10.1+), with an optional `pandoc`
  directory override, and save full unfiltered output to a temporary build log.
