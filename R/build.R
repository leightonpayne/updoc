generated_by_prebuild <- function(paths, root, documented, rendered_readme) {
  rel <- vapply(paths, relative_to_root, character(1), root = root)
  generated <- rep(FALSE, length(paths))
  if (documented) {
    generated <- generated |
      rel %in% c("DESCRIPTION", "NAMESPACE") |
      startsWith(rel, "man/")
  }
  if (rendered_readme) generated <- generated | rel == "README.md"
  paths[generated]
}

user_message <- function(
  text,
  type = "info",
  asynchronous = FALSE,
  leading_break = FALSE
) {
  if (isTRUE(asynchronous) && isTRUE(leading_break) && interactive()) {
    cli::cli_text()
  }
  alert <- switch(
    type,
    success = cli::cli_alert_success,
    warning = cli::cli_alert_warning,
    danger = cli::cli_alert_danger,
    cli::cli_alert_info
  )
  text <- switch(
    type,
    warning = cli::col_yellow(text),
    danger = cli::col_red(text),
    text
  )
  alert("{text}", .envir = environment())
  invisible(NULL)
}

terminal_semantic <- function(value, class) {
  template <- sprintf("{.%s {value}}", class)
  formatted <- cli::format_inline(template, .envir = environment())
  cli::ansi_strip(formatted, sgr = TRUE, csi = FALSE, link = FALSE)
}

terminal_path <- function(path) terminal_semantic(path, "path")

terminal_location <- function(location) {
  match <- regexec(
    "^(.*?)(:[0-9]+(?:-[0-9]+)?(?:[[:space:]]+\\[.*\\])?)$",
    location,
    perl = TRUE
  )
  pieces <- regmatches(location, match)[[1L]]
  if (length(pieces) != 3L) return(terminal_path(location))
  paste0(terminal_path(pieces[[2L]]), pieces[[3L]])
}

terminal_url <- function(url) terminal_semantic(url, "url")

terminal_code <- function(code) terminal_semantic(code, "code")

terminal_key <- function(key) terminal_semantic(key, "kbd")

terminal_paths <- function(paths) {
  paste(vapply(paths, terminal_path, character(1)), collapse = ", ")
}

restore_console_prompt <- function(state, asynchronous) {
  if (
    !isTRUE(asynchronous) || !isTRUE(state$restore_prompt) || !interactive()
  ) {
    return(invisible(NULL))
  }
  prompt <- getOption("prompt", "> ")
  if (is.character(prompt) && length(prompt) == 1L && nzchar(prompt)) {
    cat(prompt, file = stdout())
    utils::flush.console()
  }
  invisible(NULL)
}

describe_rebuild_trigger <- function(paths, styled = FALSE) {
  if (length(paths) == 1L && identical(paths, "<rebuild>")) {
    return("Manual rebuild requested")
  }
  display <- if (isTRUE(styled)) terminal_paths(paths) else
    paste(paths, collapse = ", ")
  sprintf("%s changed", display)
}

execute_build <- function(
  state,
  changed,
  force_full = FALSE,
  initial = FALSE,
  asynchronous = FALSE
) {
  force_full <- isTRUE(force_full) || !is.null(state$last_error)
  before <- source_snapshot(state$root, state$source_site_dir)
  rel <- display_paths(changed, state$root)
  reconcile <- force_full || any(rel %in% c("<initial>", "<rebuild>"))
  sync_paths <- changed
  if (
    reconcile &&
      !is.null(state$build_root) &&
      !identical(state$build_root, state$root)
  ) {
    sync_paths <- package_sync_paths(
      state$root,
      state$build_root,
      before,
      state$source_site_dir
    )
  }
  deleted <- any(
    !fs::file_exists(changed) & rel != "<initial>" & rel != "<rebuild>"
  )
  if (reconcile) deleted <- deleted || any(!fs::file_exists(sync_paths))
  documentation_only <- r_changes_are_documentation_only(
    changed,
    state$root,
    state$build_root
  )
  plan <- build_plan(
    state$strategy,
    rel,
    force_full = force_full,
    deleted = deleted,
    documentation_only = documentation_only
  )
  plan <- scope_article_index(plan, changed, state$root, state$build_root)
  targeted_articles <- identical(plan$kind, "sections") &&
    identical(plan$targets, "articles") &&
    length(plan$article_names) > 0L
  state$can_yield <- targeted_articles
  state$superseded <- FALSE
  state$publish_delta <- if (targeted_articles) {
    list(
      write = stats::setNames(character(), character()),
      remove = character()
    )
  } else NULL
  state$last_changed <- rel
  started <- proc.time()[["elapsed"]]
  documented <- FALSE
  rendered_readme <- FALSE
  success <- FALSE
  build_warnings <- character()

  if (!initial && !isTRUE(state$worker)) {
    user_message(
      sprintf(
        "%s; rebuilding %s",
        describe_rebuild_trigger(rel, styled = TRUE),
        cli::style_bold(describe_build_plan(plan, state$document))
      ),
      type = "info",
      asynchronous = asynchronous,
      leading_break = TRUE
    )
  }
  old_wd <- getwd()
  on.exit(try(setwd(old_wd), silent = TRUE), add = TRUE)

  error <- tryCatch(
    withCallingHandlers(
      {
        report_build_progress(
          state,
          "Validating changed sources",
          paste(rel, collapse = ", ")
        )
        validation_paths <- if (reconcile) {
          fs::path(state$root, "<rebuild>")
        } else changed
        validate_changed_r_sources(validation_paths, state$root)
        validate_changed_front_matter(validation_paths, state$root)
        if (
          !is.null(state$build_root) && !identical(state$build_root, state$root)
        ) {
          report_build_progress(
            state,
            "Syncing changed sources",
            paste(rel, collapse = ", ")
          )
          sync_package_changes(state$root, state$build_root, sync_paths)
        }
        if (plan$document && isTRUE(state$document)) {
          documented <- TRUE
          documentation <- run_documentation(state)
          plan <- apply_documentation_changes(plan, documentation)
        }
        if (plan$readme) {
          rendered_readme <- TRUE
          run_readme(state)
        }
        completed <- if (state_should_yield_build(state)) {
          FALSE
        } else {
          run_pkgdown(state, plan)
        }
        if (!isTRUE(completed)) state$superseded <- TRUE
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

  state$last_duration <- unname(proc.time()[["elapsed"]] - started)
  state$last_warnings <- clean_build_warnings(build_warnings, state$root)
  after <- source_snapshot(state$root, state$source_site_dir)
  during <- snapshot_changes(before, after)
  # Every source edit during a shadow build belongs in the next queue.
  internal <- if (identical(state$build_root, state$root)) {
    generated_by_prebuild(during, state$root, documented, rendered_readme)
  } else character()
  during <- setdiff(during, internal)
  state$snapshot <- after

  if (!success) {
    error <- build_error_record(
      error,
      c(state$root, state$build_root)
    )
    error <- enrich_build_error(
      error,
      state$last_warnings,
      rel,
      c(state$root, state$build_root)
    )
    state_fail_build(state, error)
    if (!isTRUE(state$worker)) {
      emit_build_error(state$last_error, asynchronous)
      if (length(state$last_warnings)) {
        emit_build_warnings(
          state$last_warnings,
          asynchronous,
          state$root,
          rel,
          failed = TRUE
        )
      }
    }
  }
  if (length(during)) {
    if (isTRUE(state$worker)) {
      state$observed_changes <- unique(c(state$observed_changes, during))
    } else {
      run_rebuild_sync(state, during, asynchronous = asynchronous)
    }
  }
  invisible(success)
}

run_rebuild_sync <- function(
  state,
  changed,
  force_full = FALSE,
  initial = FALSE,
  asynchronous = FALSE
) {
  changed <- unique(as.character(changed))
  if (isTRUE(state$building)) {
    state_queue_build(state, changed, force_full)
    return(invisible(FALSE))
  }

  state_begin_build(state)
  on.exit(state_end_build(state), add = TRUE)
  success <- execute_build(
    state,
    changed,
    force_full = force_full,
    initial = initial,
    asynchronous = asynchronous
  )

  if (length(state$pending)) {
    pending <- state_take_build_queue(state)
    success <- execute_build(
      state,
      pending$changed,
      force_full = pending$force_full,
      asynchronous = asynchronous
    )
  }
  if (length(state$pending) && isTRUE(state$active)) {
    later::later(
      function() {
        if (!isTRUE(state$active) || isTRUE(state$building))
          return(invisible(NULL))
        pending <- state_take_build_queue(state)
        run_rebuild_sync(
          state,
          pending$changed,
          force_full = pending$force_full,
          asynchronous = TRUE
        )
      },
      delay = 0
    )
  }
  superseded <- isTRUE(success) &&
    (isTRUE(state$superseded) || state_should_yield_build(state))
  if (superseded) state$superseded <- TRUE
  if (isTRUE(success) && !superseded && !length(state$pending)) {
    started <- proc.time()[["elapsed"]]
    index_error <- tryCatch(
      {
        refresh_shared_indexes(state)
        NULL
      },
      error = function(e) e
    )
    state$last_duration <- state$last_duration +
      unname(proc.time()[["elapsed"]] - started)
    if (is.null(index_error)) {
      state_complete_build(state, reload = isTRUE(state$reload_dirty))
      state$reload_dirty <- FALSE
      if (!initial && !isTRUE(state$worker)) {
        package <- cli::style_bold("pkgdown")
        user_message(
          sprintf("%s is up to date", package),
          "success",
          asynchronous
        )
      }
    } else {
      state_fail_build(
        state,
        build_error_record(index_error, c(state$root, state$build_root))
      )
      emit_build_error(state$last_error, asynchronous)
      if (length(state$last_warnings)) {
        emit_build_warnings(
          state$last_warnings,
          asynchronous,
          state$root,
          state$last_changed,
          failed = TRUE
        )
      }
      success <- FALSE
    }
  }
  restore_console_prompt(state, asynchronous)
  invisible(success)
}
