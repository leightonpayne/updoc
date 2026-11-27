#' Check a pkgdown site without modifying your package
#'
#' Validates your package sources and builds the pkgdown site in a temporary
#' copy, leaving your `man/` files, `NAMESPACE`, `README.md`, and `docs/`
#' folder untouched. Handy in CI, scripts, and AI agent edit-verify loops.
#'
#' Like [serve()] and [preview()], `check()` checks R syntax and article YAML
#' front matter, runs roxygen2 (unless `document = FALSE`), renders
#' `README.Rmd` if present, and builds the site in isolation. Temporary site
#' files are deleted before returning, while the full build log stays in the
#' current R session's temporary directory.
#'
#' @inheritParams preview
#' @param document Whether to run roxygen2 in the temporary copy before
#'   building. Set to `FALSE` if you manage `man/` files yourself. R syntax is
#'   still checked, and `README.Rmd` is still rendered if present.
#' @param quiet Whether to hide detailed pkgdown output. Always treated as
#'   `TRUE` when `format = "json"` so standard output stays valid JSON.
#' @param format Output format:
#'   * `"text"` (default): print a readable summary and structured diagnostics.
#'   * `"json"`: print one JSON object (`ok`, `package_root`, `duration`,
#'     `warnings`, `error`, and `build_log`) to standard output.
#' @param error Whether to signal an error if the build fails. Defaults to
#'   `TRUE` so `Rscript -e "updoc::check()"` exits with a non-zero status on
#'   failure. Set to `FALSE` to inspect the returned `updoc_check` object
#'   without throwing.
#' @return Invisibly, an `updoc_check` object with `ok`, `package_root`,
#'   `duration`, `warnings`, `error`, and `build_log`. When `error = TRUE` and
#'   the check fails, the thrown error message includes the log path and the
#'   condition carries `$build_log` and `$check`.
#' @examples
#' if (interactive()) {
#'   # Verify the site without touching generated files in your package.
#'   check()
#'
#'   # Emit machine-readable JSON for scripts or AI agents.
#'   check(format = "json", error = FALSE)
#' }
#' @export
check <- function(
  pkg = ".",
  document = TRUE,
  quiet = FALSE,
  format = c("text", "json"),
  error = TRUE,
  pandoc = getOption("updoc.pandoc", NULL)
) {
  root <- package_root(pkg)
  check_flag(document, "document")
  check_flag(quiet, "quiet")
  check_flag(error, "error")
  format <- match.arg(format)
  pandoc_dir <- resolve_pandoc(pandoc)

  json_mode <- identical(format, "json")
  effective_quiet <- isTRUE(quiet) || json_mode
  check_dir <- tempfile(sprintf("updoc-check-%s-", basename(root)))
  fs::dir_create(check_dir)
  on.exit(if (dir.exists(check_dir)) fs::dir_delete(check_dir), add = TRUE)

  source_site_dir <- resolve_site_dir(root)
  build_root <- fs::path(check_dir, "package")
  destination <- fs::path(check_dir, "site")
  mirror_package(root, build_root, source_site_dir)

  build_log <- tempfile("updoc-check-", fileext = ".log")
  emit <- if (json_mode) function(text) invisible(NULL) else NULL
  started <- proc.time()[["elapsed"]]

  result <- tryCatch(
    run_updoc_build(
      "check_site_selected",
      list(
        root = root,
        build_root = build_root,
        destination = destination,
        document = document,
        quiet = effective_quiet
      ),
      build_log,
      pandoc_dir,
      emit = emit
    ),
    error = function(condition) {
      record <- build_error_record(condition, c(root, build_root))
      record <- enrich_build_error(
        record,
        character(),
        "<initial>",
        c(root, build_root)
      )
      list(
        ok = FALSE,
        package_root = root,
        duration = unname(proc.time()[["elapsed"]] - started),
        warnings = list(),
        error = build_error_payload(record, root)
      )
    }
  )

  result$build_log <- build_log
  class(result) <- "updoc_check"

  if (json_mode) {
    cat(check_json(result), "\n", sep = "", file = stdout())
  } else if (isTRUE(result$ok)) {
    if (length(result$warnings)) {
      emit_check_warnings(result$warnings, failed = FALSE)
    }
    duration <- sprintf("%.2fs", result$duration)
    user_message(sprintf("pkgdown check passed (%s)", duration), "success")
  } else {
    emit_build_error(result$error)
    if (length(result$warnings)) {
      emit_check_warnings(result$warnings, failed = TRUE)
    }
    log_path <- terminal_path(build_log)
    cli::cli_bullets(
      c(i = "{.strong Build log:} {log_path}"),
      .envir = environment()
    )
  }

  if (!isTRUE(result$ok) && isTRUE(error)) check_abort(result)
  invisible(result)
}

check_site_selected <- function(
  root,
  build_root,
  destination,
  document,
  quiet
) {
  state <- new_preview_state(root, destination, document, "full", 1L, 1, quiet)
  state$build_root <- build_root
  state$worker <- isTRUE(quiet)
  started <- proc.time()[["elapsed"]]
  build_warnings <- character()
  success <- FALSE

  old_wd <- getwd()
  on.exit(try(setwd(old_wd), silent = TRUE), add = TRUE)

  if (!isTRUE(quiet)) {
    package <- cli::style_bold(basename(root))
    user_message(sprintf("Checking pkgdown site for %s", package), "info")
  }

  error <- tryCatch(
    withCallingHandlers(
      {
        validate_changed_r_sources(
          fs::path(build_root, "<initial>"),
          build_root
        )
        validate_changed_front_matter(
          fs::path(build_root, "<initial>"),
          build_root
        )
        if (isTRUE(document)) run_documentation(state)
        if (fs::file_exists(fs::path(build_root, "README.Rmd")))
          run_readme(state)
        override <- list(destination = destination)
        pkg_meta <- pkgdown::as_pkgdown(build_root, override = override)
        checked_preview_output_dir(state, pkg_meta)
        report_build_progress(
          state,
          "Checking pkgdown site",
          "full site",
          announce = !isTRUE(quiet)
        )
        relay_build_messages(
          pkgdown::build_site(
            pkg = build_root,
            lazy = FALSE,
            override = override,
            preview = FALSE,
            devel = FALSE,
            new_process = TRUE,
            install = TRUE,
            quiet = quiet
          ),
          quiet = quiet
        )
        success <- TRUE
        NULL
      },
      warning = function(condition) {
        log_build_condition(condition)
        build_warnings <<- c(build_warnings, conditionMessage(condition))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      log_build_condition(e)
      filter_pandoc_condition(e, getOption("updoc.capture_pandoc"))
    }
  )

  duration <- unname(proc.time()[["elapsed"]] - started)
  cleaned_warnings <- clean_build_warnings(build_warnings, c(root, build_root))
  warnings_payload <- build_warning_payload(cleaned_warnings, root, "<initial>")
  error_payload <- NULL
  if (!isTRUE(success)) {
    record <- build_error_record(error, c(root, build_root))
    record <- enrich_build_error(
      record,
      cleaned_warnings,
      "<initial>",
      c(root, build_root)
    )
    error_payload <- build_error_payload(record, root)
  }

  list(
    ok = isTRUE(success),
    package_root = root,
    duration = duration,
    warnings = warnings_payload,
    error = error_payload
  )
}

emit_check_warnings <- function(warnings, failed = FALSE) {
  if (!length(warnings)) return(invisible(NULL))
  count <- length(warnings)
  heading <- if (isTRUE(failed)) {
    "Build also produced %d warning%s"
  } else {
    "Build completed with %d warning%s"
  }
  user_message(
    sprintf(heading, count, if (count == 1L) "" else "s"),
    type = "warning"
  )
  for (warning in warnings) {
    summary <- cli::style_bold(cli::col_yellow(warning$summary))
    cli::cli_bullets(c(`!` = "{summary}"), .envir = environment())
    if (!is.null(warning$location)) {
      label <- cli::style_bold("Location:")
      location <- terminal_location(warning$location)
      cli::cli_bullets(c(i = "{label} {location}"), .envir = environment())
    }
    if (!is.null(warning$cause)) {
      label <- cli::style_bold(cli::col_yellow("Caused by:"))
      cause <- paste(label, cli::col_yellow(warning$cause))
      cause_symbol <- cli::col_yellow("!")
      cli::cli_text("{cause_symbol} {cause}")
    }
    if (!is.null(warning$details)) {
      details <- cli::col_yellow(warning$details)
      cli::cli_bullets(c(`>` = "{details}"), .envir = environment())
    }
    if (!is.null(warning$hint)) {
      label <- cli::style_bold("Try:")
      hint <- warning$hint
      cli::cli_bullets(c(`>` = "{label} {hint}"), .envir = environment())
    }
    if (!is.null(warning$excerpt)) emit_source_excerpt(warning$excerpt)
  }
  invisible(NULL)
}

check_json <- function(result) {
  jsonlite::toJSON(
    list(
      ok = isTRUE(result$ok),
      package_root = result$package_root,
      duration = result$duration,
      warnings = I(result$warnings),
      error = result$error,
      build_log = result$build_log
    ),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = TRUE
  )
}

check_abort <- function(result) {
  summary <- if (!is.null(result$error) && !is.null(result$error$summary)) {
    result$error$summary
  } else {
    "pkgdown site check failed."
  }
  message <- paste(
    sprintf("pkgdown check failed: %s", summary),
    sprintf("Build log: %s", result$build_log),
    sep = "\n"
  )
  condition <- structure(
    list(
      message = message,
      call = NULL,
      build_log = result$build_log,
      check = result
    ),
    class = c("updoc_check_error", "error", "condition")
  )
  stop(condition)
}

#' @export
print.updoc_check <- function(x, ...) {
  app <- cli::start_app(output = "stdout", .auto_close = FALSE)
  on.exit(cli::stop_app(app), add = TRUE)
  duration <- if (is.na(x$duration)) "" else {
    paste0(" ", cli::col_silver(sprintf("(%.2fs)", x$duration)))
  }
  if (isTRUE(x$ok)) {
    cli::cli_text(
      "{cli::symbol$tick} pkgdown check passed for {.path {x$package_root}}{duration}"
    )
  } else {
    cli::cli_text(
      "{cli::symbol$cross} pkgdown check failed for {.path {x$package_root}}{duration}"
    )
  }
  if (!is.null(x$error)) {
    summary <- x$error$summary
    line <- paste(
      cli::style_bold(cli::col_red("Error:")),
      cli::col_red(summary)
    )
    cli::cli_text("{line}")
    if (!is.null(x$error$location)) {
      location <- terminal_location(x$error$location)
      cli::cli_text("{.strong Location:} {location}")
    }
    if (!is.null(x$error$cause)) {
      line <- paste(
        cli::style_bold(cli::col_red("Cause:")),
        cli::col_red(x$error$cause)
      )
      cli::cli_text("{line}")
    }
    if (!is.null(x$error$hint)) {
      hint <- x$error$hint
      cli::cli_text("{.strong Try:} {hint}")
    }
  }
  if (length(x$warnings)) {
    cli::cli_text("{.strong Warnings:} {length(x$warnings)}")
  }
  if (!is.null(x$build_log)) {
    cli::cli_text("{.strong Build log:} {.path {x$build_log}}")
  }
  invisible(x)
}
