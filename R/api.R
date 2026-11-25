#' Serve a live pkgdown preview
#'
#' Builds your pkgdown site, opens it in your browser, and automatically
#' rebuilds and refreshes whenever you save a change. The R console stays busy
#' until you press Ctrl+C to stop the server; use [preview()] instead if you
#' want to keep using R while the preview runs.
#'
#' Previews run in a temporary copy of your package, so your `man/` files,
#' `README.md`, and `docs/` folder stay untouched. Use [check()] for a one-shot
#' non-destructive build check, or [build()] when you are ready to build the
#' site for publishing.
#'
#' @inheritParams preview
#' @return Invisibly, the stopped `updoc_preview` handle.
#' @examples
#' if (interactive()) {
#'   # Run from your package directory. Press Ctrl+C to stop.
#'   serve()
#' }
#' @export
serve <- function(pkg = ".", document = TRUE,
                  strategy = c("smart", "lazy", "full"), port = 4321,
                  browse = interactive(), quiet = FALSE,
                  theme = getOption("updoc.theme", "auto"),
                  pandoc = getOption("updoc.pandoc", NULL)) {
  handle <- preview(
    pkg = pkg,
    document = document,
    strategy = strategy,
    port = port,
    browse = browse,
    quiet = quiet,
    theme = theme,
    pandoc = pandoc
  )
  root <- handle$package_root
  state <- registry_get(root)
  if (is.null(state)) return(invisible(handle))
  state$restore_prompt <- FALSE
  key <- terminal_key("Ctrl+C")
  user_message(sprintf("Watching for changes; press %s to stop.", key), "info")
  on.exit(if (preview_running(root)) stop_preview(root), add = TRUE)
  tryCatch({
    while (state_alive(state)) {
      httpuv::service(timeoutMs = 100)
      later::run_now(timeoutSecs = 0)
    }
  }, interrupt = function(e) invisible(NULL))
  if (preview_running(root)) stop_preview(root)
  invisible(handle)
}

#' Start a background live pkgdown preview
#'
#' Starts the same live preview server as [serve()], then returns right away
#' so you can keep working in the R console. Assign the result (for example,
#' `site <- preview()`) to check status, trigger rebuilds, or stop the server
#' later.
#'
#' Previews build in a temporary copy of your package, leaving your `man/`
#' files, `README.md`, and `docs/` folder untouched. Edits to `README.Rmd` are
#' rendered automatically, and if a build fails, your last working preview
#' stays up until you fix the error and save again.
#'
#' @param pkg Path to an R package (or any file or folder inside it). Defaults
#'   to the current directory (`"."`).
#' @param document Whether to re-run roxygen2 when files in `R/` change. Set to
#'   `FALSE` if you manage `man/` files yourself. R syntax is still checked,
#'   and `README.Rmd` is still rendered when edited.
#' @param strategy How to rebuild after you save a file:
#'   * `"smart"` (default): rebuilds only the pages affected by your edit, and
#'     does a full build for broader changes like `_pkgdown.yml`, `DESCRIPTION`,
#'     or R code.
#'   * `"lazy"`: lets pkgdown skip reference pages and articles that are
#'     already up to date.
#'   * `"full"`: rebuilds the whole site on every save.
#' @param port Preferred local port for the preview server (defaults to `4321`).
#'   Use `0` to pick any free local port automatically. If a specific port is
#'   already in use, updoc picks another free port.
#' @param browse Whether to open the preview in your browser right away.
#'   Defaults to `TRUE` in interactive R sessions and `FALSE` otherwise.
#' @param quiet Whether to hide detailed pkgdown output in the console. Build
#'   progress, warnings, and errors are still shown.
#' @param theme Color scheme for the in-browser status popup:
#'   * `"auto"` (default): matches the site's light or dark theme, falling back
#'     to your system preference.
#'   * `"inverse"`: uses the opposite of `"auto"` for extra contrast.
#'   * `"light"` or `"dark"`: keeps the popup in that theme.
#'
#'   Override the default across sessions with `options(updoc.theme = ...)`.
#' @param pandoc Path to the directory containing the `pandoc` binary (version
#'   2.10.1 or newer). When `NULL` (the default), updoc uses the Pandoc found
#'   by \pkg{rmarkdown}. Set `options(updoc.pandoc = "/path/to/bin")` to reuse
#'   a specific directory across builds. updoc never downloads Pandoc or
#'   changes your system installation.
#' @return Invisibly, an `updoc_preview` handle with `url` and `package_root`
#'   fields. Print the handle to see its current status, or call its methods:
#'   * `$status()`: inspect build status, warnings, errors, and the
#'     `build_log` file path (kept for the current R session).
#'   * `$wait(timeout = 60, after = NULL)`: check for any newly saved changes,
#'     wait until the preview finishes building (or until `timeout` seconds
#'     pass), and return the current status. Pass `after` (a previous
#'     `rebuild_count`) to wait until a newer build finishes or fails.
#'   * `$rebuild(full = FALSE)`: trigger a manual rebuild (pass `full = TRUE`
#'     to rebuild the entire site).
#'   * `$open()`: open the preview in your browser again.
#'   * `$stop()`: stop watching files and shut down the server.
#' @examples
#' if (interactive()) {
#'   # Run from your package directory.
#'   site <- preview()
#'   site$status()
#'   site$rebuild()
#'   # Stop the server when you are done.
#'   site$stop()
#' }
#' @export
preview <- function(pkg = ".", document = TRUE,
                    strategy = c("smart", "lazy", "full"), port = 4321,
                    browse = interactive(), quiet = FALSE,
                    theme = getOption("updoc.theme", "auto"),
                    pandoc = getOption("updoc.pandoc", NULL)) {
  root <- package_root(pkg)
  strategy <- match.arg(strategy)
  theme <- match.arg(theme, c("auto", "inverse", "light", "dark"))
  port <- check_port(port)
  interval <- getOption("updoc.poll_interval", 0.5)
  interval <- check_interval(interval)
  check_flag(document, "document")
  check_flag(browse, "browse")
  check_flag(quiet, "quiet")
  existing <- registry_get(root)
  if (!is.null(existing)) {
    url <- terminal_url(existing$url)
    user_message(sprintf("Preview is already running at %s", url), "info")
    if (isTRUE(browse)) utils::browseURL(existing$url)
    return(invisible(new_preview_handle(root)))
  }

  pandoc_dir <- resolve_pandoc(pandoc)
  preview_dir <- tempfile(sprintf("updoc-%s-", basename(root)))
  fs::dir_create(preview_dir)
  destination <- fs::path(preview_dir, "work")
  state <- new_preview_state(root, destination, document, strategy, port,
                             interval, quiet, theme)
  state$pandoc_dir <- pandoc_dir
  state$preview_dir <- preview_dir
  state$progress_path <- fs::path(preview_dir, "progress.rds")
  state$build_root <- fs::path(preview_dir, "package")
  state$restore_prompt <- interactive()
  state$site_dir <- fs::path(preview_dir, "site-0")

  tryCatch({
    state$source_site_dir <- resolve_site_dir(root)
    state$warm_started <- prepare_initial_preview(
      root, state$source_site_dir, state$site_dir
    )
    mirror_package(root, state$build_root, state$source_site_dir)
    state$build_dir <- resolve_site_dir(state$build_root, destination)
    state$snapshot <- source_snapshot(root, state$source_site_dir)
    start_preview_server(state)
    registry_set(state)
    start_build_worker(state)
    start_watcher(state)
    package <- cli::style_bold(basename(root))
    if (isTRUE(state$warm_started)) {
      user_message(
        "Serving existing pkgdown output while the initial build runs",
        "info"
      )
    }
    user_message(sprintf("Building pkgdown site for %s", package), "info")
    request_rebuild(state, fs::path(root, "<initial>"), initial = TRUE)
  }, error = function(e) {
    stop_state(state)
    registry_remove(root)
    stop(e)
  })
  url <- terminal_url(state$url)
  user_message(sprintf("Preview is available at %s", url), "success")
  if (isTRUE(browse)) utils::browseURL(state$url)

  invisible(new_preview_handle(root))
}

new_preview_handle <- function(root) {
  state <- registry_get(root)
  handle <- new.env(parent = emptyenv())
  handle$package_root <- root
  handle$url <- if (is.null(state)) NA_character_ else state$url
  handle$status <- function() preview_status(root)
  handle$wait <- function(timeout = 60, after = NULL) {
    preview_wait(root, timeout = timeout, after = after)
  }
  handle$rebuild <- function(full = FALSE) {
    preview_rebuild(root, full = full)
    invisible(handle)
  }
  handle$open <- function() {
    current <- preview_status(root)
    if (!isTRUE(current$active)) {
      updoc_abort(
        "The preview is not running; call updoc::preview() to start it."
      )
    }
    utils::browseURL(current$url)
    invisible(handle)
  }
  handle$stop <- function() {
    stop_preview(root)
    invisible(handle)
  }
  class(handle) <- "updoc_preview"
  handle
}

#' @export
print.updoc_preview <- function(x, ...) {
  print(x$status(), ...)
  invisible(x)
}

stop_preview <- function(pkg = ".") {
  root <- if (is.character(pkg) && length(pkg) == 1L &&
              !is.null(registry_get(pkg))) {
    pkg
  } else {
    package_root(pkg)
  }
  state <- registry_get(root)
  if (is.null(state)) {
    path <- terminal_path(root)
    user_message(sprintf("No preview is running for %s", path), "info")
    return(invisible(FALSE))
  }
  registry_remove(root, stop = TRUE)
  user_message("Preview stopped", "success")
  invisible(TRUE)
}

preview_running <- function(pkg = ".") {
  root <- package_root(pkg)
  !is.null(registry_get(root))
}

preview_status <- function(pkg = ".") {
  root <- package_root(pkg)
  state <- registry_get(root)
  if (is.null(state)) {
    out <- list(
      active = FALSE, phase = "down", package_root = root, site_dir = NA_character_,
      url = NA_character_, host = NA_character_, port = NA_integer_,
      building = FALSE, pending = FALSE, last_changed = character(),
      progress = NULL,
      last_success = as.POSIXct(NA), last_duration = NA_real_,
      last_error = NULL, warnings = list(), rebuild_count = 0L,
      build_log = NULL,
      strategy = NA_character_, theme = NA_character_
    )
  } else {
    out <- list(
      active = state_alive(state), phase = state_phase(state),
      package_root = state$root,
      site_dir = state$site_dir, url = state$url, host = state$host,
      port = state$port, building = state$building,
      pending = length(state$pending) > 0L || length(state$debounced) > 0L,
      progress = if (isTRUE(state$building)) state$progress else NULL,
      last_changed = state$last_changed, last_success = state$last_success,
      last_duration = state$last_duration, last_error = state$last_error,
      build_log = state$build_log,
      warnings = build_warning_payload(
        state$last_warnings, state$root, state$last_changed
      ),
      rebuild_count = state$rebuild_count,
      strategy = state$strategy,
      theme = state$theme
    )
  }
  class(out) <- "updoc_status"
  out
}

#' @export
print.updoc_status <- function(x, ...) {
  app <- cli::start_app(output = "stdout", .auto_close = FALSE)
  on.exit(cli::stop_app(app), add = TRUE)
  if (!isTRUE(x$active)) {
    cli::cli_text(
      "{cli::symbol$stop} Preview stopped for {.path {x$package_root}}"
    )
    return(invisible(x))
  }
  cli::cli_text(
    "{cli::symbol$play} Preview at {.url {x$url}} [{.field {x$phase}}]"
  )
  cli::cli_text(
    "{.strong Builds:} {x$rebuild_count}  {.strong Strategy:} {x$strategy}"
  )
  if (!is.na(x$last_duration)) {
    duration <- cli::col_silver(sprintf("%.2fs", x$last_duration))
    cli::cli_text("{.strong Last build:} {duration}")
  }
  if (isTRUE(x$pending)) cli::cli_text("{.strong Queue:} changes waiting")
  if (isTRUE(x$building) && !is.null(x$progress)) {
    progress <- format_build_progress(x$progress, styled = TRUE)
    cli::cli_text("{.strong Rebuilding:} {progress}")
  }
  if (!is.null(x$last_error)) {
    error <- normalize_build_error(x$last_error, x$package_root)
    summary <- error$summary
    line <- paste(
      cli::style_bold(cli::col_red("Last error:")), cli::col_red(summary)
    )
    cli::cli_text("{line}")
    if (!is.null(error$location)) {
      location <- terminal_location(error$location)
      cli::cli_text("{.strong Location:} {location}")
    }
    if (!is.null(error$cause)) {
      line <- paste(
        cli::style_bold(cli::col_red("Cause:")), cli::col_red(error$cause)
      )
      cli::cli_text("{line}")
    }
    if (!is.null(error$hint)) {
      hint <- error$hint
      cli::cli_text("{.strong Try:} {hint}")
    }
    if (!is.null(error$excerpt)) {
      emit_source_excerpt(error$excerpt)
    }
  }
  if (length(x$warnings)) {
    count <- length(x$warnings)
    heading <- cli::style_bold(cli::col_yellow(
      sprintf("Warnings (%d):", count)
    ))
    cli::cli_text("{heading}")
    for (warning in x$warnings) {
      summary <- cli::style_bold(cli::col_yellow(warning$summary))
      cli::cli_bullets(c(`!` = "{summary}"), .envir = environment())
      if (!is.null(warning$location)) {
        location <- terminal_location(warning$location)
        cli::cli_text("  {.strong Location:} {location}")
      }
      if (!is.null(warning$cause)) {
        label <- cli::style_bold(cli::col_yellow("Caused by:"))
        cause <- cli::col_yellow(warning$cause)
        cli::cli_text("  {label} {cause}")
      }
      if (!is.null(warning$hint)) {
        hint <- warning$hint
        cli::cli_text("  {.strong Try:} {hint}")
      }
      if (!is.null(warning$excerpt)) emit_source_excerpt(warning$excerpt)
    }
  }
  invisible(x)
}

preview_wait <- function(pkg = ".", timeout = 60, after = NULL) {
  root <- package_root(pkg)
  timeout <- check_timeout(timeout)
  after <- check_after(after)
  state <- registry_get(root)
  if (is.null(state)) return(preview_status(root))
  flush_watched_changes(state)
  deadline <- Sys.time() + timeout
  while (state_alive(state) && !preview_is_settled(state, after) &&
         Sys.time() < deadline) {
    later::run_now(0.05)
    httpuv::service(50)
  }
  preview_status(root)
}

preview_rebuild <- function(pkg = ".", full = FALSE) {
  root <- package_root(pkg)
  check_flag(full, "full")
  state <- registry_get(root)
  if (is.null(state)) {
    updoc_abort(sprintf(
      "No preview is running for %s; call updoc::preview() first.", root
    ))
  }
  request_rebuild(state, fs::path(root, "<rebuild>"), force_full = isTRUE(full))
  invisible(preview_status(root))
}
