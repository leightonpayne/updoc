updoc_abort <- function(message) {
  cli::cli_abort("{message}", call = NULL, .envir = environment())
}

updoc_warn <- function(message) {
  cli::cli_warn("{message}", .envir = environment())
}

check_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    updoc_abort(sprintf("`%s` must be `TRUE` or `FALSE`.", name))
  }
  invisible(value)
}

check_port <- function(port) {
  if (
    !is.numeric(port) ||
      length(port) != 1L ||
      is.na(port) ||
      !is.finite(port) ||
      port < 0L ||
      port > 65535L ||
      port != floor(port)
  ) {
    updoc_abort("`port` must be an integer from 0 through 65535.")
  }
  invisible(as.integer(port))
}

check_interval <- function(interval) {
  if (
    !is.numeric(interval) ||
      length(interval) != 1L ||
      is.na(interval) ||
      !is.finite(interval) ||
      interval <= 0
  ) {
    updoc_abort("`interval` must be a positive number of seconds.")
  }
  invisible(interval)
}

check_timeout <- function(timeout) {
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      is.na(timeout) ||
      !is.finite(timeout) ||
      timeout < 0
  ) {
    updoc_abort("`timeout` must be a non-negative number of seconds.")
  }
  invisible(as.numeric(timeout))
}

check_after <- function(after) {
  if (is.null(after)) return(invisible(NULL))
  if (
    !is.numeric(after) ||
      length(after) != 1L ||
      is.na(after) ||
      !is.finite(after) ||
      after < 0 ||
      after != floor(after)
  ) {
    updoc_abort("`after` must be NULL or a non-negative integer.")
  }
  invisible(as.integer(after))
}
