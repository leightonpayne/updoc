updoc_pandoc_minimum_version <- function() "2.10.1"

pandoc_setup_error <- function(detail) {
  updoc_abort(paste(
    detail,
    sprintf(
      "updoc requires Pandoc %s or newer for its pkgdown builds.",
      updoc_pandoc_minimum_version()
    ),
    "Install Pandoc from https://pandoc.org/installing.html,",
    "or supply an installed version with `pandoc = '/path/to/bin'`.",
    sep = "\n"
  ))
}

pandoc_binary_version <- function(directory) {
  binary <- file.path(
    directory,
    if (.Platform$OS.type == "windows") {
      "pandoc.exe"
    } else "pandoc"
  )
  if (!file.exists(binary) || dir.exists(binary)) return(NULL)

  output <- tempfile("updoc-pandoc-version-")
  on.exit(unlink(output), add = TRUE)
  tryCatch(
    {
      status <- suppressWarnings(system2(
        binary,
        "--version",
        stdout = output,
        stderr = output,
        timeout = 5
      ))
      if (!identical(status, 0L)) return(NULL)
      first <- readLines(output, n = 1L, warn = FALSE)
      if (!length(first)) return(NULL)
      match <- regexec(
        "^pandoc(?:[.]exe)?[[:space:]]+([0-9]+(?:[.][0-9]+)*)(?:[[:space:]]|$)",
        first,
        perl = TRUE
      )
      parts <- regmatches(first, match)[[1L]]
      if (length(parts) == 2L) parts[[2L]] else NULL
    },
    error = function(condition) NULL
  )
}

resolve_pandoc <- function(directory = NULL) {
  if (is.null(directory)) {
    # Use rmarkdown's ordinary discovery without changing the caller's cache.
    old_pandoc <- find_build_pandoc()
    on.exit(restore_build_pandoc(old_pandoc), add = TRUE)
    directory <- find_build_pandoc(cache = FALSE)$dir
    if (is.null(directory)) {
      pandoc_setup_error("A Pandoc installation could not be found.")
    }
  } else if (
    !is.character(directory) ||
      length(directory) != 1L ||
      is.na(directory) ||
      !nzchar(trimws(directory))
  ) {
    updoc_abort("`pandoc` must be NULL or one non-empty directory path.")
  }

  directory <- path.expand(directory)
  if (!dir.exists(directory)) {
    pandoc_setup_error(sprintf(
      "Pandoc directory does not exist: %s",
      directory
    ))
  }
  directory <- normalizePath(directory, winslash = "/", mustWork = TRUE)
  version <- pandoc_binary_version(directory)
  if (is.null(version)) {
    pandoc_setup_error(sprintf(
      "No usable Pandoc executable was found in %s.",
      directory
    ))
  }
  if (
    numeric_version(version) < numeric_version(updoc_pandoc_minimum_version())
  ) {
    pandoc_setup_error(sprintf(
      "Pandoc %s was found in %s.",
      version,
      directory
    ))
  }
  directory
}

find_build_pandoc <- function(cache = TRUE, dir = NULL, version = NULL) {
  rmarkdown::find_pandoc(cache = cache, dir = dir, version = version)
}

restore_build_pandoc <- function(pandoc) {
  # An empty explicit directory restores a cache with no discovered Pandoc.
  find_build_pandoc(
    cache = FALSE,
    dir = if (is.null(pandoc$dir)) "" else pandoc$dir,
    version = if (is.null(pandoc$dir)) NULL else pandoc$version
  )
  invisible(NULL)
}

restore_pandoc_environment <- function(values) {
  unset <- names(values)[is.na(values)]
  if (length(unset)) Sys.unsetenv(unset)
  present <- values[!is.na(values)]
  if (length(present)) do.call(Sys.setenv, as.list(present))
  invisible(NULL)
}

with_build_pandoc <- function(directory, code) {
  directory <- resolve_pandoc(directory)
  version <- pandoc_binary_version(directory)
  old_environment <- Sys.getenv(
    c("PATH", "RSTUDIO_PANDOC"),
    unset = NA_character_
  )
  old_pandoc <- find_build_pandoc()
  on.exit(
    {
      restore_pandoc_environment(old_environment)
      restore_build_pandoc(old_pandoc)
    },
    add = TRUE
  )

  search_path <- c(directory, old_environment[["PATH"]])
  search_path <- search_path[!is.na(search_path)]
  Sys.setenv(
    PATH = paste(search_path, collapse = .Platform$path.sep),
    RSTUDIO_PANDOC = directory
  )

  # Article and publication subprocesses perform their own discovery. Check
  # their default search too: ~/opt/pandoc can outrank both environment paths.
  discovered <- find_build_pandoc(cache = FALSE)
  same_directory <- !is.null(discovered$dir) &&
    identical(
      normalizePath(discovered$dir, winslash = "/", mustWork = FALSE),
      directory
    )
  if (
    !identical(as.character(discovered$version), version) ||
      !same_directory
  ) {
    found <- if (is.null(discovered$dir)) "no Pandoc installation" else
      sprintf(
        "Pandoc %s in %s",
        as.character(discovered$version),
        discovered$dir
      )
    updoc_abort(paste(
      sprintf("Build subprocesses would select %s.", found),
      sprintf("The selected Pandoc is %s in %s.", version, directory),
      "Check for a conflicting installation in ~/opt/pandoc before retrying.",
      sep = "\n"
    ))
  }
  find_build_pandoc(cache = FALSE, dir = directory, version = version)
  force(code)
}
