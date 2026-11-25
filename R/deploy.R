#' Build your pkgdown site for publishing
#'
#' Builds a complete pkgdown site ready to deploy. Use [serve()] or [preview()]
#' for a live preview while you edit, or [check()] to verify the site in a
#' temporary copy without modifying files in your package.
#'
#' Unlike a live preview, `build()` writes directly to your package: it updates
#' roxygen documentation (unless `document = FALSE`), renders `README.Rmd` to
#' `README.md` if present, and builds the site into your configured pkgdown
#' output folder (usually `docs/`). It does not upload the site anywhere;
#' publish that folder with your usual hosting setup.
#'
#' @inheritParams preview
#' @param destination Optional output folder to write the site into instead of
#'   pkgdown's default. Relative paths are resolved from the package root.
#' @param document Whether to run roxygen2 before building. Set to `FALSE` if
#'   you manage `man/` files yourself. Even when `FALSE`, `README.Rmd` is still
#'   rendered if present.
#' @param lazy Whether to skip rebuilding reference pages and articles whose
#'   output is already newer than their source files.
#' @return Invisibly, an `updoc_build` object with `package_root`, `site_dir`,
#'   `duration`, `lazy`, and `build_log`. The full build log is saved in the
#'   current R session's temporary directory; if a build fails, the error
#'   message includes the log path and the error condition carries it in
#'   `$build_log`.
#' @examples
#' if (interactive()) {
#'   # Run from your package directory when you are ready to publish.
#'   build()
#' }
#' @export
build <- function(pkg = ".", destination = NULL, document = TRUE,
                  lazy = FALSE, quiet = FALSE,
                  pandoc = getOption("updoc.pandoc", NULL)) {
  root <- package_root(pkg)
  check_flag(document, "document")
  check_flag(lazy, "lazy")
  check_flag(quiet, "quiet")
  override <- destination_override(root, destination)
  pandoc_dir <- resolve_pandoc(pandoc)
  build_log <- tempfile("updoc-build-", fileext = ".log")
  result <- tryCatch(
    run_updoc_build(
      "build_site_selected",
      list(root = root, destination = destination, document = document,
           lazy = lazy, quiet = quiet, override = override),
      build_log, pandoc_dir
    ),
    error = function(condition) {
      stop(attach_build_log_message(condition, build_log))
    }
  )
  result$build_log <- build_log
  invisible(result)
}

build_site_selected <- function(root, destination, document, lazy, quiet,
                                override) {
  state <- new_preview_state(root, destination, document, "full", 1L, 1, quiet)
  state$build_root <- root
  started <- proc.time()[["elapsed"]]
  package <- cli::style_bold(basename(root))
  user_message(sprintf("Building deployable pkgdown site for %s", package), "info")

  old_wd <- getwd()
  on.exit(try(setwd(old_wd), silent = TRUE), add = TRUE)
  validate_changed_r_sources(fs::path(root, "<initial>"), root)
  if (isTRUE(document)) run_documentation(state)
  if (fs::file_exists(fs::path(root, "README.Rmd"))) run_readme(state)
  report_build_progress(
    state, "Building deployable pkgdown site",
    if (isTRUE(lazy)) "incremental site" else "full site"
  )
  relay_build_messages(
    pkgdown::build_site(
      pkg = root,
      lazy = lazy,
      override = override,
      preview = FALSE,
      devel = FALSE,
      new_process = TRUE,
      install = TRUE,
      quiet = quiet
    ),
    quiet = quiet
  )

  result <- list(
    package_root = root,
    site_dir = resolve_site_dir(root, destination),
    duration = unname(proc.time()[["elapsed"]] - started),
    lazy = lazy
  )
  class(result) <- "updoc_build"
  site_dir <- terminal_path(result$site_dir)
  user_message(sprintf("pkgdown site built at %s", site_dir), "success")
  invisible(result)
}

#' @export
print.updoc_build <- function(x, ...) {
  app <- cli::start_app(output = "stdout", .auto_close = FALSE)
  on.exit(cli::stop_app(app), add = TRUE)
  cli::cli_text(
    "{cli::symbol$tick} pkgdown site built at {.path {x$site_dir}}"
  )
  duration <- cli::col_silver(sprintf("%.2fs", x$duration))
  cli::cli_text(
    "{.strong Duration:} {duration}  {.strong Lazy:} {x$lazy}"
  )
  invisible(x)
}
