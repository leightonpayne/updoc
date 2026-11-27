build_worker_job <- function(spec) {
  run_updoc_build(
    "build_worker_job_selected",
    list(spec = spec),
    spec$build_log,
    spec$pandoc_dir
  )
}

build_worker_job_selected <- function(spec) {
  state <- new_preview_state(
    spec$root,
    spec$destination,
    spec$document,
    spec$strategy,
    1L,
    spec$interval,
    spec$quiet
  )
  state$site_dir <- spec$site_dir
  state$published_site_dir <- spec$published_site_dir
  state$build_root <- spec$build_root
  state$source_site_dir <- spec$source_site_dir
  state$snapshot <- spec$snapshot
  state$indexes_dirty <- spec$indexes_dirty
  state$progress_path <- spec$progress_path
  state$supersede_path <- spec$supersede_path
  state$worker <- TRUE
  state$pandoc_dir <- spec$pandoc_dir
  state$trace_enabled <- isTRUE(spec$trace_enabled)

  success <- run_rebuild_sync(
    state,
    spec$changed,
    force_full = spec$force_full,
    initial = spec$initial
  )

  list(
    success = isTRUE(success),
    site_dir = state$site_dir,
    snapshot = state$snapshot,
    observed_changes = state$observed_changes,
    last_changed = state$last_changed,
    last_success = state$last_success,
    last_duration = state$last_duration,
    last_error = state$last_error,
    last_warnings = state$last_warnings,
    rebuild_count = state$rebuild_count,
    reload_count = state$reload_version,
    indexes_dirty = state$indexes_dirty,
    publish_delta = state$publish_delta,
    superseded = isTRUE(state$superseded),
    trace = state$trace
  )
}

start_build_worker <- function(state) {
  state$compute <- new_compute_name()
  mirai::daemons(
    1L,
    dispatcher = TRUE,
    output = TRUE,
    .compute = state$compute
  )
  invisible(state$compute)
}

build_worker_spec <- function(
  state,
  changed,
  force_full,
  initial,
  supersede_path
) {
  list(
    root = state$root,
    destination = state$destination,
    document = state$document,
    strategy = state$strategy,
    interval = state$interval,
    quiet = state$quiet,
    pandoc_dir = state$pandoc_dir,
    build_log = state$build_log,
    site_dir = state$build_dir,
    published_site_dir = state$site_dir,
    build_root = state$build_root,
    source_site_dir = state$source_site_dir,
    snapshot = state$snapshot,
    indexes_dirty = state$indexes_dirty,
    progress_path = state$progress_path,
    supersede_path = supersede_path,
    changed = changed,
    force_full = force_full,
    initial = initial,
    trace_enabled = isTRUE(state$trace_enabled)
  )
}

dispatch_build_job <- function(
  state,
  changed,
  force_full,
  initial,
  can_yield = FALSE
) {
  namespace_path <- getNamespaceInfo(asNamespace("updoc"), "path")
  source_tree <- fs::file_exists(fs::path(namespace_path, "R", "build.R"))
  lib_paths <- .libPaths()
  supersede_path <- tempfile("supersede-", tmpdir = state$preview_dir)
  # Keep logs outside preview_dir so stopping the server does not erase them.
  state$build_log <- tempfile("updoc-preview-", fileext = ".log")
  spec <- build_worker_spec(
    state,
    changed,
    force_full,
    initial,
    supersede_path
  )

  job <- mirai::mirai(
    {
      .libPaths(lib_paths)
      if (source_tree) {
        if (!requireNamespace("pkgload", quietly = TRUE)) {
          updoc_abort(
            paste(
              "The suggested package 'pkgload' is required for",
              "source-tree previews."
            )
          )
        }
        pkgload::load_all(namespace_path, quiet = TRUE)
      } else {
        loadNamespace("updoc", lib.loc = dirname(namespace_path))
      }
      worker <- get("build_worker_job", envir = asNamespace("updoc"))
      worker(spec)
    },
    spec = spec,
    namespace_path = namespace_path,
    source_tree = source_tree,
    lib_paths = lib_paths,
    .compute = state$compute
  )
  state_set_build_job(
    state,
    job,
    changed,
    initial,
    can_yield,
    supersede_path
  )
  state$job_poller <- later::later(
    function() check_build_job(state),
    delay = 0.05
  )
  invisible(state$job)
}

record_worker_failure <- function(state, value) {
  message <- if (inherits(value, "errorValue")) {
    as.character(value)
  } else {
    "The build worker returned an invalid result."
  }
  state_fail_build(
    state,
    build_error_record_text(
      message,
      c(state$root, state$build_root),
      class(value)
    )
  )
  state$last_duration <- NA_real_
  state$last_warnings <- character()
  state$snapshot <- source_snapshot(state$root, state$source_site_dir)
  invisible(FALSE)
}

manifest_paths_are_safe <- function(paths) {
  if (!length(paths)) return(TRUE)
  paths <- gsub("\\\\", "/", paths)
  parts <- strsplit(paths, "/", fixed = TRUE)
  !any(
    !nzchar(paths) |
      startsWith(paths, "/") |
      grepl("^[A-Za-z]:/", paths) |
      vapply(parts, function(path) any(path %in% c("", ".", "..")), logical(1))
  )
}

publish_preview_delta <- function(state, delta) {
  write <- delta$write
  remove <- unique(as.character(delta$remove))
  paths <- unique(c(names(write), remove))
  if (!manifest_paths_are_safe(paths)) {
    updoc_abort("The preview worker returned unsafe publication paths.")
  }
  if (!length(paths)) return(invisible(state$site_dir))

  backup <- tempfile("publish-rollback-", tmpdir = state$preview_dir)
  fs::dir_create(backup)
  existed <- stats::setNames(logical(length(paths)), paths)
  restore <- FALSE
  temporary <- NULL
  on.exit(
    {
      if (!is.null(temporary)) unlink(temporary)
      if (restore) {
        for (path in paths) {
          target <- fs::path(state$site_dir, path)
          if (isTRUE(existed[[path]])) {
            fs::dir_create(fs::path_dir(target))
            fs::file_copy(fs::path(backup, path), target, overwrite = TRUE)
          } else if (fs::file_exists(target) || fs::is_link(target)) {
            remove_path_safe(target)
          }
        }
      }
      # A failed restoration must leave its backup available.
      if (dir.exists(backup)) fs::dir_delete(backup)
    },
    add = TRUE
  )
  for (path in paths) {
    target <- fs::path(state$site_dir, path)
    existed[[path]] <- fs::file_exists(target) || fs::is_link(target)
    if (isTRUE(existed[[path]])) {
      saved <- fs::path(backup, path)
      fs::dir_create(fs::path_dir(saved))
      fs::file_copy(target, saved, overwrite = TRUE)
    }
  }

  restore <- TRUE

  for (path in remove) {
    target <- fs::path(state$site_dir, path)
    if (fs::file_exists(target) || fs::is_link(target)) remove_path_safe(target)
  }
  for (path in names(write)) {
    source <- fs::path(state$build_dir, path)
    if (!fs::file_exists(source)) {
      updoc_abort(sprintf("Generated preview output is missing: %s", path))
    }
    hash <- unname(tools::md5sum(source))
    if (!identical(hash, unname(write[[path]]))) {
      updoc_abort(sprintf(
        "Generated preview output changed before publication: %s",
        path
      ))
    }
    target <- fs::path(state$site_dir, path)
    temporary <- tempfile("publish-", tmpdir = fs::path_dir(target))
    fs::dir_create(fs::path_dir(target))
    fs::file_copy(source, temporary, overwrite = TRUE)
    if (fs::file_exists(target) || fs::is_link(target)) remove_path_safe(target)
    if (!file.rename(temporary, target)) {
      if (fs::file_exists(temporary)) remove_path_safe(temporary)
      updoc_abort(sprintf(
        "Could not publish generated preview output: %s",
        path
      ))
    }
  }
  restore <- FALSE
  state_publish_site(state, state$site_dir)
  invisible(state$site_dir)
}

publish_preview <- function(state, delta = NULL) {
  if (
    is.null(state$preview_dir) ||
      is.null(state$build_dir) ||
      !dir.exists(state$build_dir)
  ) {
    updoc_abort("The preview worker did not produce a site directory.")
  }
  if (
    !is.null(delta) && !is.null(state$site_dir) && dir.exists(state$site_dir)
  ) {
    return(publish_preview_delta(state, delta))
  }
  generation <- state$publish_generation + 1L
  target <- fs::path(state$preview_dir, sprintf("site-%d", generation))
  if (dir.exists(target)) fs::dir_delete(target)
  copy_directory_safe(state$build_dir, target)

  previous <- state_publish_site(state, target)
  if (
    !is.null(previous) && !identical(previous, target) && dir.exists(previous)
  ) {
    fs::dir_delete(previous)
  }
  invisible(target)
}

apply_worker_result <- function(state, result) {
  if (!is.list(result) || is.null(result$success)) {
    return(record_worker_failure(state, result))
  }
  if (!length(state$pending)) state$snapshot <- result$snapshot
  state$last_changed <- result$last_changed
  state$last_duration <- result$last_duration
  state$last_warnings <- if (is.null(result$last_warnings)) {
    character()
  } else {
    clean_build_warnings(result$last_warnings, state$root)
  }
  state$indexes_dirty <- isTRUE(result$indexes_dirty)
  # A later delta omits earlier unpublished output, so publish the full tree.
  state$publish_delta <- if (state$reload_pending > 0L) NULL else
    result$publish_delta
  if (isTRUE(state$trace_enabled) && length(result$trace)) {
    state$trace <- c(state$trace, result$trace)
  }
  if (isTRUE(result$success)) {
    # Development-mode changes can move pkgdown's output directory.
    if (!is.null(result$site_dir)) state$build_dir <- result$site_dir
    state$reload_pending <- state$reload_pending + result$reload_count
  } else {
    state_fail_build(state, result$last_error)
  }
  if (length(result$observed_changes)) {
    state_queue_build(state, result$observed_changes)
  }
  isTRUE(result$success)
}

check_build_job <- function(state) {
  state$job_poller <- NULL
  if (!isTRUE(state$active) || is.null(state$job)) return(invisible(NULL))
  if (mirai::unresolved(state$job)) {
    sync_build_progress(state)
    state$job_poller <- later::later(
      function() check_build_job(state),
      delay = 0.05
    )
    return(invisible(NULL))
  }

  result <- state$job$data
  sync_build_progress(state)
  initial <- state_clear_build_job(state)$initial
  success <- if (mirai::is_error_value(result)) {
    record_worker_failure(state, result)
  } else {
    apply_worker_result(state, result)
  }
  if (isTRUE(success) && !length(state$pending)) {
    publish_error <- tryCatch(
      {
        target <- if (is.null(state$publish_delta)) {
          "new site generation"
        } else {
          "changed outputs"
        }
        report_build_progress(state, "Publishing preview", target)
        publish_preview(state, state$publish_delta)
        NULL
      },
      error = function(e) e
    )
    if (is.null(publish_error)) {
      # Reload paths describe committed output from a single worker only.
      reload_paths <- if (
        !isTRUE(initial) &&
          state$reload_pending <= 1L &&
          !is.null(state$publish_delta)
      ) {
        unique(c(names(state$publish_delta$write), state$publish_delta$remove))
      } else NULL
      state_complete_build(
        state,
        result$last_success,
        reload = state$reload_pending > 0L,
        reload_paths = reload_paths
      )
      state$reload_pending <- 0L
    } else {
      state_fail_build(
        state,
        build_error_record(publish_error, c(state$root, state$build_root))
      )
      success <- FALSE
    }
  }
  outcome <- if (!isTRUE(success)) {
    "Failed"
  } else if (length(state$pending)) {
    "Superseded"
  } else {
    "Ready"
  }
  finish_build_trace(state, outcome)
  state_end_build(state, clear_progress = TRUE, track_timing = TRUE)

  if (
    isTRUE(success) && !length(state$pending) && length(state$last_warnings)
  ) {
    emit_build_warnings(
      state$last_warnings,
      TRUE,
      state$root,
      state$last_changed
    )
  }

  if (length(state$pending)) {
    pending <- state_take_build_queue(state)
    request_rebuild(
      state,
      pending$changed,
      force_full = pending$force_full,
      asynchronous = TRUE
    )
  } else if (success) {
    if (!initial) {
      package <- cli::style_bold("pkgdown")
      user_message(sprintf("%s is up to date", package), "success", TRUE)
    }
  } else {
    emit_build_error(state$last_error, TRUE)
    if (length(state$last_warnings)) {
      emit_build_warnings(
        state$last_warnings,
        TRUE,
        state$root,
        state$last_changed,
        failed = TRUE
      )
    }
    if (initial) {
      user_message(
        "Serving the last usable output; the next save will retry.",
        "warning",
        asynchronous = TRUE
      )
    }
  }
  restore_console_prompt(state, asynchronous = TRUE)
  invisible(success)
}

request_rebuild <- function(
  state,
  changed,
  force_full = FALSE,
  initial = FALSE,
  asynchronous = FALSE
) {
  changed <- unique(as.character(changed))
  if (isTRUE(state$building)) {
    state_queue_active_build(state, changed, force_full)
    return(invisible(FALSE))
  }

  # Recovery must replace partial work before publishing again.
  if (
    !isTRUE(initial) &&
      (state$rebuild_count == 0L ||
        !is.null(state$last_error) ||
        !isTRUE(fs::file_exists(fs::path(state$site_dir, "index.html"))))
  ) {
    force_full <- TRUE
  }

  if (isTRUE(state$trace_enabled)) {
    if (length(state$trace)) {
      state$trace_history[[length(state$trace_history) + 1L]] <- state$trace
    }
    state$trace <- list()
  }
  state_begin_build(state, changed, track_timing = TRUE)
  deleted <- any(
    !fs::file_exists(changed) &
      !state$last_changed %in% c("<initial>", "<rebuild>")
  )
  documentation_only <- r_changes_are_documentation_only(
    changed,
    state$root,
    state$build_root
  )
  plan <- build_plan(
    state$strategy,
    state$last_changed,
    force_full = force_full,
    deleted = deleted,
    documentation_only = documentation_only
  )
  can_yield <- identical(plan$kind, "sections") &&
    identical(plan$targets, "articles") &&
    length(plan$article_names) > 0L
  clear_build_progress(state)
  report_build_progress(
    state,
    "Preparing rebuild",
    describe_build_plan(plan, state$document),
    announce = FALSE
  )
  if (!initial) {
    user_message(
      sprintf(
        "%s; rebuilding %s",
        describe_rebuild_trigger(state$last_changed, styled = TRUE),
        cli::style_bold(describe_build_plan(plan, state$document))
      ),
      type = "info",
      asynchronous = asynchronous,
      leading_break = TRUE
    )
  }
  error <- tryCatch(
    {
      dispatch_build_job(state, changed, force_full, initial, can_yield)
      NULL
    },
    error = function(e) e
  )
  if (!is.null(error)) {
    state_fail_build(
      state,
      build_error_record(error, c(state$root, state$build_root))
    )
    state_end_build(state, clear_progress = TRUE, track_timing = TRUE)
    emit_build_error(state$last_error, asynchronous)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

schedule_rebuild <- function(state, changed) {
  state$debounced <- unique(c(state$debounced, changed))
  if (is.function(state$debouncer)) state$debouncer()
  delay <- min(0.1, state$interval)
  state$debouncer <- later::later(
    function() {
      state$debouncer <- NULL
      if (!isTRUE(state$active)) return(invisible(NULL))
      paths <- state$debounced
      state$debounced <- character()
      if (length(paths)) request_rebuild(state, paths, asynchronous = TRUE)
      invisible(NULL)
    },
    delay = delay
  )
  invisible(state)
}

flush_watched_changes <- function(state) {
  if (
    !isTRUE(state$active) || is.null(state$snapshot) || is.null(state$compute)
  ) {
    return(invisible(FALSE))
  }
  current <- source_snapshot(state$root, state$source_site_dir)
  changed <- snapshot_changes(state$snapshot, current)
  state$snapshot <- current
  queued <- unique(c(state$debounced, changed))
  if (!length(queued)) return(invisible(FALSE))
  state$debounced <- character()
  if (is.function(state$debouncer)) {
    try(state$debouncer(), silent = TRUE)
    state$debouncer <- NULL
  }
  if (isTRUE(state$building)) {
    state_queue_active_build(state, queued)
  } else {
    request_rebuild(state, queued, asynchronous = TRUE)
  }
  invisible(TRUE)
}

preview_is_settled <- function(state, after = NULL) {
  if (is.null(state) || !isTRUE(state$active)) return(TRUE)
  if (
    isTRUE(state$building) ||
      length(state$pending) > 0L ||
      length(state$debounced) > 0L
  ) {
    return(FALSE)
  }
  if (!is.null(after)) {
    return(state$rebuild_count > after || !is.null(state$last_error))
  }
  TRUE
}

poll_sources <- function(state) {
  if (!isTRUE(state$active)) return(invisible(NULL))
  current <- source_snapshot(state$root, state$source_site_dir)
  changed <- snapshot_changes(state$snapshot, current)
  state$snapshot <- current
  if (length(changed)) {
    if (isTRUE(state$building)) {
      state_queue_active_build(state, changed)
    } else {
      schedule_rebuild(state, changed)
    }
  }
  state$watcher <- later::later(
    function() poll_sources(state),
    delay = state$interval
  )
  invisible(NULL)
}

start_watcher <- function(state) {
  state$snapshot <- source_snapshot(state$root, state$source_site_dir)
  state$watcher <- later::later(
    function() poll_sources(state),
    delay = state$interval
  )
  invisible(state$watcher)
}
