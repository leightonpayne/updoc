clean_diagnostic_text <- function(text, root = NULL) {
  text <- paste(as.character(text), collapse = "\n")
  text <- cli::ansi_strip(text)
  text <- gsub("\r\n?", "\n", text)
  text <- gsub("[[:space:]]+(Quitting from[[:space:]])", "\n\\1", text)
  text <- gsub("[[:space:]]+(Caused by error:)", "\n\\1", text)
  if (length(root)) {
    for (path in unique(root[!is.na(root) & nzchar(root)])) {
      text <- gsub(paste0(path, .Platform$file.sep), "", text, fixed = TRUE)
    }
  }
  lines <- trimws(strsplit(text, "\n", fixed = TRUE)[[1L]])
  lines <- gsub("^[\u2716!\u2139\u2022>]+[[:space:]]*", "", lines)
  lines <- lines[nzchar(lines)]
  paste(lines, collapse = "\n")
}

diagnostic_from_text <- function(text, root = NULL, parent = NULL) {
  text <- clean_diagnostic_text(text, root)
  lines <- if (nzchar(text)) strsplit(text, "\n", fixed = TRUE)[[1L]] else
    character()
  if (!length(lines)) lines <- "Unknown build error"

  cause_pattern <- "^Caused by error(?:[[:space:]]+in[[:space:]].*)?:"
  cause_index <- grep(cause_pattern, lines, perl = TRUE)
  cause <- NULL
  if (length(cause_index)) {
    index <- cause_index[[1L]]
    inline <- trimws(sub(
      paste0(cause_pattern, "[[:space:]]*"), "", lines[[index]], perl = TRUE
    ))
    following <- if (index < length(lines)) lines[(index + 1L):length(lines)] else
      character()
    cause <- paste(c(inline[nzchar(inline)], following), collapse = " ")
    lines <- lines[seq_len(index - 1L)]
  }

  location_index <- grep("^Quitting from[[:space:]]+", lines)
  location <- NULL
  if (length(location_index)) {
    index <- location_index[[1L]]
    location <- sub("^Quitting from[[:space:]]+", "", lines[[index]])
    lines <- lines[-index]
  }

  summary <- lines[[1L]]
  details <- if (length(lines) > 1L) paste(lines[-1L], collapse = "\n") else
    NULL
  if (is.null(cause) && inherits(parent, "condition")) {
    parent_text <- clean_diagnostic_text(conditionMessage(parent), root)
    if (nzchar(parent_text) && !identical(parent_text, text)) {
      cause <- paste(strsplit(parent_text, "\n", fixed = TRUE)[[1L]],
                     collapse = " ")
    }
  }

  list(
    summary = summary,
    location = location,
    cause = if (is.null(cause) || !nzchar(cause)) NULL else cause,
    details = if (is.null(details) || !nzchar(details)) NULL else details,
    hint = NULL,
    excerpt = NULL,
    message = text
  )
}

quoted_vignette_path <- function(text) {
  match <- regexpr("vignettes/[^'\"[:space:]]+[.](Rmd|rmd|qmd)", text)
  if (match[[1L]] < 0L) return(NULL)
  regmatches(text, match)[[1L]]
}

warning_file_path <- function(warnings) {
  if (!length(warnings)) return(NULL)
  candidates <- warnings[grepl(
    "cannot open (compressed )?file|No such file or directory",
    warnings,
    ignore.case = TRUE
  )]
  if (!length(candidates)) return(NULL)
  text <- candidates[[length(candidates)]]
  match <- regexec(
    "cannot open (?:compressed )?file ['\"]([^'\"]+)['\"]",
    text,
    ignore.case = TRUE,
    perl = TRUE
  )
  pieces <- regmatches(text, match)[[1L]]
  if (length(pieces) < 2L) return(NULL)
  pieces[[2L]]
}

diagnostic_source_excerpt <- function(location, roots = character()) {
  if (is.null(location) || !length(roots)) return(NULL)
  plain <- sub("[[:space:]]+\\[.*$", "", location)
  match <- regexec("^(.*):([0-9]+)(?:-([0-9]+))?$", plain, perl = TRUE)
  pieces <- regmatches(plain, match)[[1L]]
  if (length(pieces) < 3L) return(NULL)
  relative <- pieces[[2L]]
  first <- as.integer(pieces[[3L]])
  last <- if (length(pieces) >= 4L && nzchar(pieces[[4L]])) {
    as.integer(pieces[[4L]])
  } else first
  candidates <- fs::path(roots, relative)
  existing <- candidates[fs::file_exists(candidates)]
  if (!length(existing)) return(NULL)
  lines <- readLines(existing[[1L]], warn = FALSE, encoding = "UTF-8")
  first <- max(1L, first)
  last <- min(length(lines), last)
  if (first > last) return(NULL)
  numbers <- seq.int(first, last)
  selected <- lines[numbers]
  keep <- nzchar(trimws(selected)) &
    !grepl("^[[:space:]]*```", selected) &
    !grepl("^[[:space:]]*#\\|", selected)
  selected <- selected[keep]
  numbers <- numbers[keep]
  if (!length(selected)) return(NULL)
  if (length(selected) > 6L) {
    selected <- selected[seq_len(6L)]
    numbers <- numbers[seq_len(6L)]
  }
  paste(sprintf("%d | %s", numbers, selected), collapse = "\n")
}

literal_connection_path <- function(excerpt) {
  if (is.null(excerpt)) return(NULL)
  code <- gsub(
    "(^|\\n)[0-9]+[[:space:]]*\\|[[:space:]]*", "\\1", excerpt,
    perl = TRUE
  )
  pattern <- paste0(
    "(?:file|readLines|readBin|readChar|read[.]csv|read[.]delim|source|",
    "load|gzfile|bzfile|xzfile|url)[[:space:]]*\\([[:space:]]*",
    "['\"]([^'\"]+)['\"]"
  )
  match <- regexec(pattern, code, ignore.case = TRUE, perl = TRUE)
  pieces <- regmatches(code, match)[[1L]]
  if (length(pieces) < 2L) return(NULL)
  pieces[[2L]]
}

enrich_build_error <- function(error, warnings = character(), changed = character(),
                               roots = character()) {
  error <- normalize_build_error(error)
  warnings <- clean_build_warnings(c(warnings, error$context))
  error$context <- NULL
  if (!is.null(error$details) && length(warnings)) {
    details <- error$details
    ordered <- warnings[order(nchar(warnings), decreasing = TRUE)]
    for (warning in ordered) details <- gsub(warning, "", details, fixed = TRUE)
    details <- clean_diagnostic_text(details)
    error$details <- if (nzchar(details)) details else NULL
  }
  source <- quoted_vignette_path(error$summary)
  if (is.null(source)) {
    vignette <- changed[grepl("^vignettes/.*[.](Rmd|rmd|qmd)$", changed)]
    if (length(vignette) == 1L) source <- vignette[[1L]]
  }

  if (!is.null(source) && !is.null(error$location) &&
      identical(basename(source), sub(":.*$", "", error$location))) {
    error$location <- paste0(dirname(source), "/", error$location)
  } else if (is.null(error$location) && !is.null(source)) {
    error$location <- source
  }

  error$excerpt <- diagnostic_source_excerpt(error$location, roots)

  path <- warning_file_path(warnings)
  connection_text <- paste(
    unlist(error[c("summary", "cause", "details")]), collapse = "\n"
  )
  connection_error <- grepl(
    "cannot open the connection", connection_text, fixed = TRUE
  )
  if (is.null(path) && connection_error) {
    path <- literal_connection_path(error$excerpt)
  }
  if (connection_error && !is.null(path)) {
    specific <- warnings[grepl(path, warnings, fixed = TRUE)]
    if (length(specific)) {
      error$cause <- specific[[length(specific)]]
    } else {
      error$cause <- sprintf(
        "cannot open the connection while opening `%s`", path
      )
    }
    if (!is.null(error$details)) {
      detail_lines <- strsplit(error$details, "\n", fixed = TRUE)[[1L]]
      detail_lines <- detail_lines[!grepl(path, detail_lines, fixed = TRUE)]
      error$details <- if (length(detail_lines)) {
        paste(detail_lines, collapse = "\n")
      } else NULL
    }
  }
  if (connection_error) {
    if (!is.null(path) && !is.null(source) && !fs::is_absolute_path(path)) {
      error$hint <- sprintf(
        "Check that `%s` exists and is resolved correctly from `%s`.",
        path, source
      )
    } else if (!is.null(path)) {
      error$hint <- sprintf("Check that `%s` exists and is readable.", path)
    } else {
      error$hint <- paste0(
        "Inspect file paths opened by this chunk; a referenced file may be ",
        "missing or unreadable."
      )
    }
  }
  if (grepl("^Invalid R syntax in ", error$summary) &&
      is.null(error$hint)) {
    error$hint <- "Fix the highlighted expression and save the R file again."
  }
  error
}

condition_diagnostic_context <- function(condition, root = NULL) {
  output <- character()
  current <- condition
  depth <- 0L
  while (inherits(current, "condition") && depth < 10L) {
    depth <- depth + 1L
    output <- c(output, conditionMessage(current))
    for (field in c("stderr", "stdout", "output")) {
      value <- tryCatch(current[[field]], error = function(e) NULL)
      if (is.character(value)) output <- c(output, value)
    }
    current <- tryCatch(current$parent, error = function(e) NULL)
  }
  clean_build_warnings(output, root)
}

build_error_record <- function(condition, root = NULL) {
  diagnostic <- diagnostic_from_text(
    conditionMessage(condition), root, condition$parent
  )
  c(diagnostic, list(
    context = condition_diagnostic_context(condition, root),
    time = Sys.time(),
    class = class(condition)
  ))
}

build_error_record_text <- function(text, root = NULL, class = "error") {
  diagnostic <- diagnostic_from_text(text, root)
  c(diagnostic, list(time = Sys.time(), class = class))
}

normalize_build_error <- function(error, root = NULL) {
  if (is.null(error)) return(NULL)
  if (!is.null(error$summary) && nzchar(error$summary)) return(error)
  text <- if (!is.null(error$message)) error$message else as.character(error)
  c(
    diagnostic_from_text(text, root),
    list(time = error$time, class = error$class)
  )
}

build_error_payload <- function(error, root = NULL) {
  error <- normalize_build_error(error, root)
  if (is.null(error)) return(NULL)
  error[c("summary", "location", "cause", "details", "hint", "excerpt")]
}

clean_build_warnings <- function(warnings, root = NULL) {
  warnings <- vapply(
    warnings, clean_diagnostic_text, character(1), root = root
  )
  unique(warnings[nzchar(warnings)])
}

vignette_title_source <- function(root, location, index_title, yaml_title) {
  if (is.null(root) || !fs::dir_exists(fs::path(root, "vignettes"))) {
    return(list(location = location, excerpt = NULL))
  }

  candidates <- character()
  if (!is.null(location) && nzchar(location)) {
    path <- sub(":[0-9]+(?:-[0-9]+)?$", "", location, perl = TRUE)
    candidate <- fs::path(root, path)
    if (fs::file_exists(candidate)) candidates <- candidate
  }
  if (!length(candidates)) {
    candidates <- fs::dir_ls(
      fs::path(root, "vignettes"),
      regexp = "[.](Rmd|rmd|qmd)$",
      type = "file"
    )
  }

  best <- NULL
  best_score <- 0L
  for (candidate in candidates) {
    lines <- readLines(candidate, warn = FALSE, encoding = "UTF-8")
    index_lines <- which(grepl(index_title, lines, fixed = TRUE))
    yaml_lines <- which(grepl(yaml_title, lines, fixed = TRUE))
    score <- as.integer(length(index_lines) > 0L) +
      as.integer(length(yaml_lines) > 0L)
    numbers <- c(
      if (length(index_lines)) index_lines[[1L]],
      if (length(yaml_lines)) yaml_lines[[1L]]
    )
    if (score > best_score) {
      best <- list(
        path = candidate,
        lines = lines,
        numbers = sort(unique(numbers))
      )
      best_score <- score
    }
  }
  if (is.null(best) || best_score < 2L) {
    return(list(location = location, excerpt = NULL))
  }

  relative <- fs::path_rel(best$path, start = root)
  numbers <- best$numbers
  located <- if (length(numbers) > 1L) {
    sprintf("%s:%d-%d", relative, min(numbers), max(numbers))
  } else {
    sprintf("%s:%d", relative, numbers[[1L]])
  }
  list(
    location = located,
    excerpt = paste(
      sprintf("%d | %s", numbers, best$lines[numbers]),
      collapse = "\n"
    )
  )
}

build_warning_record <- function(warning, root = NULL, changed = character()) {
  text <- clean_diagnostic_text(warning, root)
  location <- quoted_vignette_path(text)
  if (is.null(location)) {
    vignette <- changed[grepl("^vignettes/.*[.](Rmd|rmd|qmd)$", changed)]
    if (length(vignette) == 1L) location <- vignette[[1L]]
  }

  title_pattern <- paste0(
    "The vignette title specified in \\\\VignetteIndexEntry\\{\\} is different ",
    "from the title in the YAML metadata[.] The former is \"([^\"]*)\", and ",
    "the latter is \"([^\"]*)\"[.]"
  )
  pieces <- regmatches(text, regexec(title_pattern, text, perl = TRUE))[[1L]]
  if (length(pieces) == 3L) {
    source <- vignette_title_source(root, location, pieces[[2L]], pieces[[3L]])
    return(list(
      summary = "Vignette titles do not match",
      location = source$location,
      cause = paste0(
        "\\VignetteIndexEntry{} is \"", pieces[[2L]],
        "\"; the YAML title is \"", pieces[[3L]], "\"."
      ),
      details = NULL,
      hint = paste0(
        "Match the YAML and \\VignetteIndexEntry{} titles."
      ),
      excerpt = source$excerpt
    ))
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  lines <- lines[nzchar(trimws(lines))]
  summary <- if (length(lines)) lines[[1L]] else "Build warning"
  details <- if (length(lines) > 1L) paste(lines[-1L], collapse = "\n") else NULL
  if (nchar(summary, type = "width") > 120L) {
    details <- text
    summary <- "Build warning"
  }
  list(
    summary = summary,
    location = location,
    cause = NULL,
    details = details,
    hint = NULL,
    excerpt = NULL
  )
}

build_warning_payload <- function(warnings, root = NULL, changed = character()) {
  warnings <- clean_build_warnings(warnings, root)
  unname(lapply(
    warnings, build_warning_record, root = root, changed = changed
  ))
}

emit_source_excerpt <- function(excerpt) {
  if (is.null(excerpt) || !nzchar(excerpt)) return(invisible(NULL))
  lines <- strsplit(excerpt, "\n", fixed = TRUE)[[1L]]
  lines <- vapply(lines, function(line) {
    match <- regexec(
      "^([[:space:]]*[0-9]+)([[:space:]]*[|][[:space:]]*)(.*)$",
      line,
      perl = TRUE
    )
    pieces <- regmatches(line, match)[[1L]]
    if (length(pieces) != 4L) return(line)
    paste0(cli::col_silver(paste0(pieces[[2L]], pieces[[3L]])), pieces[[4L]])
  }, character(1))
  source_label <- cli::style_bold("Source:")
  cli::cli_bullets(c(`>` = "{source_label}"), .envir = environment())
  cli::cli_verbatim(paste0("  ", lines))
  invisible(NULL)
}

emit_build_error <- function(error, asynchronous = FALSE) {
  summary <- error$summary
  if (is.null(summary) || !nzchar(summary)) summary <- "Unknown build error"
  label <- cli::style_bold(cli::col_red("Build failed:"))
  message <- paste(label, cli::col_red(summary))
  user_message(
    message,
    type = "danger",
    asynchronous = asynchronous,
    leading_break = TRUE
  )
  if (!is.null(error$location) && nzchar(error$location)) {
    label <- cli::style_bold("Location:")
    location <- terminal_location(error$location)
    cli::cli_bullets(c(i = "{label} {location}"), .envir = environment())
  }
  if (!is.null(error$cause) && nzchar(error$cause)) {
    label <- cli::style_bold(cli::col_red("Caused by:"))
    cause <- paste(label, cli::col_red(error$cause))
    cause_symbol <- cli::col_red("!")
    cli::cli_text("{cause_symbol} {cause}")
  }
  if (!is.null(error$details) && nzchar(error$details)) {
    details <- cli::col_red(error$details)
    cli::cli_bullets(c(`>` = "{details}"), .envir = environment())
  }
  if (!is.null(error$hint) && nzchar(error$hint)) {
    label <- cli::style_bold("Try:")
    hint <- error$hint
    cli::cli_bullets(c(`>` = "{label} {hint}"), .envir = environment())
  }
  if (!is.null(error$excerpt) && nzchar(error$excerpt)) {
    emit_source_excerpt(error$excerpt)
  }
  invisible(NULL)
}

emit_build_warnings <- function(warnings, asynchronous = FALSE, root = NULL,
                                changed = character(), failed = FALSE) {
  warnings <- build_warning_payload(warnings, root, changed)
  if (!length(warnings)) return(invisible(NULL))
  count <- length(warnings)
  heading <- if (isTRUE(failed)) {
    "Build also produced %d warning%s"
  } else {
    "Build completed with %d warning%s"
  }
  user_message(
    sprintf(heading, count, if (count == 1L) "" else "s"),
    type = "warning",
    asynchronous = asynchronous
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
