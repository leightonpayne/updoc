.updoc_state <- new.env(parent = emptyenv())
.updoc_state$registry <- new.env(parent = emptyenv())
.updoc_state$compute_counter <- 0L

new_compute_name <- function() {
  .updoc_state$compute_counter <- .updoc_state$compute_counter + 1L
  sprintf("updoc-%d-%d", Sys.getpid(), .updoc_state$compute_counter)
}

registry_get <- function(root) {
  key <- enc2utf8(root)
  if (!exists(key, envir = .updoc_state$registry, inherits = FALSE)) {
    return(NULL)
  }
  state <- get(key, envir = .updoc_state$registry, inherits = FALSE)
  if (!state_alive(state)) {
    registry_remove(root, stop = TRUE)
    return(NULL)
  }
  state
}

registry_set <- function(state) {
  assign(enc2utf8(state$root), state, envir = .updoc_state$registry)
  invisible(state)
}

registry_remove <- function(root, stop = FALSE) {
  key <- enc2utf8(root)
  if (!exists(key, envir = .updoc_state$registry, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  state <- get(key, envir = .updoc_state$registry, inherits = FALSE)
  if (isTRUE(stop)) stop_state(state)
  rm(list = key, envir = .updoc_state$registry)
  invisible(TRUE)
}

state_alive <- function(state) {
  if (is.null(state) || !isTRUE(state$active) || is.null(state$server)) {
    return(FALSE)
  }
  isTRUE(tryCatch(state$server$isRunning(), error = function(e) FALSE))
}

state_phase <- function(state) {
  if (is.null(state) || !isTRUE(state$active)) return("down")
  if (isTRUE(state$building)) return("building")
  if (!is.null(state$last_error)) return("error")
  "ready"
}

new_preview_state <- function(root, destination, document, strategy, port,
                              interval, quiet, theme = "auto") {
  state <- new.env(parent = emptyenv())
  state$root <- root
  state$destination <- destination
  state$preview_dir <- NULL
  state$build_dir <- NULL
  state$build_root <- root
  state$source_site_dir <- NULL
  state$warm_started <- FALSE
  state$site_dir <- NULL
  state$published_site_dir <- NULL
  state$document <- document
  state$strategy <- strategy
  state$requested_port <- as.integer(port)
  state$host <- "127.0.0.1"
  state$port <- NA_integer_
  state$url <- NA_character_
  state$interval <- interval
  state$quiet <- quiet
  state$theme <- theme
  state$pandoc_dir <- NULL
  state$build_log <- NULL
  state$server <- NULL
  state$compute <- NULL
  state$job <- NULL
  state$job_poller <- NULL
  state$job_changed <- character()
  state$job_initial <- FALSE
  state$job_can_yield <- FALSE
  state$supersede_path <- NULL
  state$observed_changes <- character()
  state$worker <- FALSE
  state$progress_path <- NULL
  state$progress <- NULL
  state$trace_enabled <- isTRUE(getOption("updoc.trace", FALSE))
  state$trace <- list()
  state$trace_history <- list()
  state$watcher <- NULL
  state$debouncer <- NULL
  state$active <- FALSE
  state$building <- FALSE
  state$pending <- character()
  state$debounced <- character()
  state$indexes_dirty <- FALSE
  state$reload_dirty <- FALSE
  state$snapshot <- NULL
  state$last_changed <- character()
  state$last_success <- as.POSIXct(NA)
  state$last_duration <- NA_real_
  state$build_started <- as.POSIXct(NA)
  state$last_error <- NULL
  state$last_warnings <- character()
  state$rebuild_count <- 0L
  state$reload_version <- 0L
  state$reload_descriptor_version <- 0L
  state$reload_descriptor <- list(full = TRUE, paths = character())
  state$reload_pending <- 0L
  state$publish_generation <- 0L
  state$publish_delta <- NULL
  state$pkgdown <- NULL
  state$can_yield <- FALSE
  state$superseded <- FALSE
  state
}

state_queue_build <- function(state, changed = character(),
                              force_full = FALSE) {
  pending <- unique(c(state$pending, as.character(changed)))
  if (isTRUE(force_full)) pending <- c(pending, "<force-full>")
  state$pending <- unique(pending)
  invisible(state$pending)
}

state_take_build_queue <- function(state) {
  pending <- state$pending
  state$pending <- character()
  list(
    changed = setdiff(pending, "<force-full>"),
    force_full = "<force-full>" %in% pending
  )
}

state_begin_build <- function(state, changed = NULL, track_timing = FALSE) {
  if (isTRUE(state$building)) {
    updoc_abort("Internal error: a build is already in progress.")
  }
  if (isTRUE(track_timing) && is.na(state$build_started)) {
    state$build_started <- Sys.time()
  }
  state$building <- TRUE
  if (!is.null(changed)) {
    state$last_changed <- display_paths(changed, state$root)
  }
  validate_preview_state(state)
  invisible(state)
}

state_finish_build_timing <- function(state) {
  if (!is.na(state$build_started)) {
    state$last_duration <- as.numeric(
      difftime(Sys.time(), state$build_started, units = "secs")
    )
    state$build_started <- as.POSIXct(NA)
  }
  invisible(state$last_duration)
}

state_end_build <- function(state, clear_progress = FALSE,
                            track_timing = FALSE) {
  state$building <- FALSE
  if (isTRUE(clear_progress)) clear_build_progress(state)
  if (isTRUE(track_timing) && !length(state$pending)) {
    state_finish_build_timing(state)
  }
  validate_preview_state(state)
  invisible(state)
}

state_complete_build <- function(state, last_success = Sys.time(),
                                 reload = FALSE, reload_paths = NULL) {
  state$last_success <- last_success
  state$last_error <- NULL
  state$rebuild_count <- state$rebuild_count + 1L
  if (isTRUE(reload)) {
    state$reload_version <- state$reload_version + 1L
    state$reload_descriptor <- list(
      full = is.null(reload_paths),
      paths = unique(as.character(reload_paths))
    )
    state$reload_descriptor_version <- state$reload_version
  }
  validate_preview_state(state)
  invisible(TRUE)
}

state_fail_build <- function(state, error) {
  state$last_error <- error
  invisible(FALSE)
}

state_publish_site <- function(state, site_dir) {
  previous <- state$site_dir
  state$site_dir <- site_dir
  state$publish_generation <- state$publish_generation + 1L
  validate_preview_state(state)
  invisible(previous)
}

state_set_build_job <- function(state, job, changed, initial,
                                can_yield = FALSE,
                                supersede_path = NULL) {
  if (!is.null(state$job)) {
    updoc_abort("Internal error: a build worker job is already registered.")
  }
  state$job <- job
  state$job_changed <- as.character(changed)
  state$job_initial <- isTRUE(initial)
  state$job_can_yield <- isTRUE(can_yield)
  state$supersede_path <- supersede_path
  validate_preview_state(state)
  invisible(job)
}

state_queue_active_build <- function(state, changed = character(),
                                     force_full = FALSE) {
  if (isTRUE(state$job_can_yield)) {
    changed <- unique(c(state$job_changed, changed))
  }
  state_queue_build(state, changed, force_full)
  if (isTRUE(state$job_can_yield) && !is.null(state$supersede_path)) {
    try(file.create(state$supersede_path), silent = TRUE)
  }
  invisible(state$pending)
}

state_should_yield_build <- function(state) {
  isTRUE(state$can_yield) && !is.null(state$supersede_path) &&
    file.exists(state$supersede_path)
}

state_clear_build_job <- function(state) {
  job <- list(
    value = state$job,
    changed = state$job_changed,
    initial = isTRUE(state$job_initial)
  )
  state$job <- NULL
  state$job_changed <- character()
  state$job_initial <- FALSE
  state$job_can_yield <- FALSE
  state$job_poller <- NULL
  if (!is.null(state$supersede_path) && file.exists(state$supersede_path)) {
    unlink(state$supersede_path)
  }
  state$supersede_path <- NULL
  validate_preview_state(state)
  job
}

preview_state_problems <- function(state) {
  problems <- character()
  if (!is.logical(state$building) || length(state$building) != 1L ||
        is.na(state$building)) {
    problems <- c(problems, "`building` must be one TRUE or FALSE value")
  }
  if (!is.character(state$pending) || anyDuplicated(state$pending)) {
    problems <- c(problems, "`pending` must contain unique paths")
  }
  if (is.null(state$job)) {
    # The worker subprocess legitimately holds supersede_path without a mirai job.
    has_job_meta <- length(state$job_changed) || isTRUE(state$job_initial) ||
      isTRUE(state$job_can_yield) ||
      (!isTRUE(state$worker) && !is.null(state$supersede_path))
    if (has_job_meta) {
      problems <- c(problems, "job metadata exists without a worker job")
    }
  } else if (!isTRUE(state$building)) {
    problems <- c(problems, "a worker job exists while no build is active")
  }
  counters <- c(
    rebuild_count = state$rebuild_count,
    reload_version = state$reload_version,
    reload_pending = state$reload_pending,
    publish_generation = state$publish_generation
  )
  if (any(!is.finite(counters)) || any(counters < 0) ||
        any(counters != as.integer(counters))) {
    problems <- c(problems, "lifecycle counters must be non-negative integers")
  }
  unique(problems)
}

validate_preview_state <- function(state) {
  problems <- preview_state_problems(state)
  if (length(problems)) {
    detail <- paste(problems, collapse = "; ")
    updoc_abort(paste("Invalid preview state:", detail))
  }
  invisible(state)
}

stop_state <- function(state) {
  state$active <- FALSE
  if (is.function(state$job_poller)) {
    try(state$job_poller(), silent = TRUE)
  }
  if (!is.null(state$job) && mirai::unresolved(state$job)) {
    try(mirai::stop_mirai(state$job), silent = TRUE)
  }
  state_clear_build_job(state)
  state$pending <- character()
  state$debounced <- character()
  state_end_build(state, clear_progress = TRUE)
  state$build_started <- as.POSIXct(NA)
  if (!is.null(state$compute)) {
    try(mirai::daemons(0, .compute = state$compute), silent = TRUE)
  }
  state$compute <- NULL
  if (is.function(state$watcher)) {
    try(state$watcher(), silent = TRUE)
  }
  state$watcher <- NULL
  if (is.function(state$debouncer)) {
    try(state$debouncer(), silent = TRUE)
  }
  state$debouncer <- NULL
  if (!is.null(state$server)) {
    try(state$server$stop(), silent = TRUE)
  }
  state$server <- NULL
  if (!is.null(state$preview_dir) && fs::dir_exists(state$preview_dir)) {
    try(fs::dir_delete(state$preview_dir), silent = TRUE)
  }
  state$preview_dir <- NULL
  validate_preview_state(state)
  invisible(state)
}

.onUnload <- function(libpath) {
  keys <- ls(.updoc_state$registry, all.names = TRUE)
  for (key in keys) {
    state <- get(key, envir = .updoc_state$registry, inherits = FALSE)
    stop_state(state)
  }
}
