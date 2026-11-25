# Pandoc 3.11 still accepts these flags. Keep this allowlist deliberately small:
# other versions and every unrecognised diagnostic pass through unchanged.
known_pandoc_noise <- function(lines, version) {
  known_version <- length(version) == 1L && !is.na(version) &&
    grepl("^3[.]11(?:[.][0-9]+)*$", version, perl = TRUE)
  if (!known_version) return(rep(FALSE, length(lines)))
  plain <- sub("[\r\n]+$", "", cli::ansi_strip(lines))
  plain %in% c(
    "[WARNING] Deprecated: --mathml. Use --math-method=mathml instead.",
    "[WARNING] Deprecated: --mathjax. Use --math-method=mathjax[:URL] instead."
  )
}

new_build_output <- function(log, version, emit = function(text) {
  cat(text, file = stderr())
}) {
  cat("", file = log)
  pending <- ""
  relay <- function(text) {
    if (!known_pandoc_noise(text, version)) emit(text)
    invisible(NULL)
  }
  list(
    write = function(chunk) {
      append_build_output(chunk, log)
      pending <<- paste0(pending, chunk)
      repeat {
        newline <- regexpr("\n", pending, fixed = TRUE)[[1L]]
        if (newline < 0L) break
        relay(substr(pending, 1L, newline))
        pending <<- substring(pending, newline + 1L)
      }
      invisible(NULL)
    },
    flush = function() {
      if (nzchar(pending)) relay(pending)
      pending <<- ""
      invisible(NULL)
    }
  )
}

append_build_output <- function(text, path) {
  # Text connections translate newlines on Windows, so append stream bytes.
  connection <- file(path, open = "ab")
  on.exit(close(connection), add = TRUE)
  writeBin(charToRaw(text), connection)
  invisible(NULL)
}

filter_pandoc_text <- function(text, version) {
  if (!is.character(text)) return(text)
  vapply(text, function(value) {
    if (is.na(value)) return(value)
    lines <- strsplit(value, "(?<=\n)", perl = TRUE)[[1L]]
    paste0(lines[!known_pandoc_noise(lines, version)], collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

filter_pandoc_condition <- function(condition, version, depth = 0L) {
  if (!inherits(condition, "condition") || depth >= 10L) return(condition)
  for (field in c("message", "stdout", "stderr", "output")) {
    if (is.character(condition[[field]])) {
      condition[[field]] <- filter_pandoc_text(condition[[field]], version)
    }
  }
  if (inherits(condition$parent, "condition")) {
    condition$parent <- filter_pandoc_condition(condition$parent, version, depth + 1L)
  }
  condition
}

attach_build_log_message <- function(condition, build_log) {
  if (!inherits(condition, "condition") || !is.character(build_log) ||
      length(build_log) != 1L || is.na(build_log) || !nzchar(build_log)) {
    return(condition)
  }
  condition$build_log <- build_log
  note <- sprintf("Build log: %s", build_log)
  rendered <- tryCatch(conditionMessage(condition), error = function(e) "")
  if (!grepl(note, rendered, fixed = TRUE)) {
    base_msg <- condition$message
    condition$message <- if (is.character(base_msg) && length(base_msg) == 1L &&
                             !is.na(base_msg) && nzchar(base_msg)) {
      paste(base_msg, note, sep = "\n")
    } else {
      note
    }
  }
  condition
}

run_captured_build <- function(func, args = list(),
                               build_log = tempfile("updoc-build-", fileext = ".log"),
                               pandoc_version = NULL,
                               emit = NULL) {
  output <- if (is.null(emit)) {
    new_build_output(build_log, pandoc_version)
  } else {
    new_build_output(build_log, pandoc_version, emit = emit)
  }
  on.exit(output$flush(), add = TRUE)
  tryCatch(
    callr::r(
      func, args = args, show = FALSE, stderr = "2>&1",
      block_callback = output$write, spinner = FALSE,
      supervise = TRUE, cleanup_tree = TRUE
    ),
    error = function(condition) {
      log_build_condition(condition, build_log)
      # Preserve the original error and exit status; filter only display text.
      condition <- filter_pandoc_condition(condition, pandoc_version)
      condition$build_log <- build_log
      stop(condition)
    }
  )
}

run_updoc_build <- function(name, args, build_log, pandoc_dir, emit = NULL) {
  namespace_path <- getNamespaceInfo(asNamespace("updoc"), "path")
  source_tree <- fs::file_exists(fs::path(namespace_path, "R", "build.R"))
  version <- pandoc_binary_version(pandoc_dir)
  run_captured_build(
    function(name, args, namespace_path, source_tree, build_log, pandoc_dir,
             version) {
      if (source_tree) {
        if (!requireNamespace("pkgload", quietly = TRUE)) {
          stop("The suggested package 'pkgload' is required for source-tree builds.")
        }
        pkgload::load_all(namespace_path, quiet = TRUE)
      } else {
        loadNamespace("updoc", lib.loc = dirname(namespace_path))
      }
      options(updoc.capture_log = build_log, updoc.capture_pandoc = version)
      namespace <- asNamespace("updoc")
      selected <- get("with_build_pandoc", envir = namespace)
      selected(pandoc_dir, do.call(get(name, envir = namespace), args))
    },
    args = list(name = name, args = args, namespace_path = namespace_path,
                source_tree = source_tree, build_log = build_log,
                pandoc_dir = pandoc_dir, version = version),
    build_log = build_log, pandoc_version = version, emit = emit
  )
}

log_build_condition <- function(condition, path = getOption("updoc.capture_log")) {
  if (!is.null(path)) {
    depth <- 0L
    while (inherits(condition, "condition") && depth < 10L) {
      cat(conditionMessage(condition), "\n", file = path, append = TRUE, sep = "")
      for (field in c("stdout", "stderr", "output")) {
        if (is.character(condition[[field]])) {
          cat(condition[[field]], "\n", file = path, append = TRUE, sep = "")
        }
      }
      condition <- condition$parent
      depth <- depth + 1L
    }
  }
  invisible(NULL)
}
