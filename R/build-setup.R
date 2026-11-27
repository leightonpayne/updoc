build_plan <- function(
  strategy,
  changed,
  force_full = FALSE,
  deleted = FALSE,
  documentation_only = FALSE
) {
  strategy <- match.arg(strategy, c("smart", "lazy", "full"))
  rel <- gsub("\\\\", "/", changed)
  prebuild_all <- isTRUE(force_full) ||
    any(rel %in% c("<initial>", "<rebuild>"))
  needs_document <- prebuild_all || any(startsWith(rel, "R/"))
  needs_readme <- prebuild_all || "README.Rmd" %in% rel
  site_plan <- function(lazy, reason) {
    list(
      kind = "site",
      targets = character(),
      lazy = lazy,
      reference_lazy = lazy,
      articles_lazy = lazy,
      reference_files = character(),
      article_names = character(),
      article_index = TRUE,
      reference_index_only = FALSE,
      reference_unscoped = FALSE,
      document = needs_document,
      readme = needs_readme,
      reason = reason
    )
  }

  if (force_full || identical(strategy, "full") || isTRUE(deleted)) {
    return(site_plan(
      FALSE,
      if (isTRUE(deleted)) {
        "deleted source"
      } else "full strategy"
    ))
  }
  if (identical(strategy, "lazy")) {
    return(site_plan(TRUE, "lazy strategy"))
  }

  assets <- grepl("^pkgdown/(extra[^/]*|assets/|favicon/)", rel) |
    grepl("^man/figures/logo[.]", rel)
  r_source <- startsWith(rel, "R/")
  structural <- rel %in%
    c(
      "<initial>",
      "DESCRIPTION",
      "NAMESPACE",
      "_pkgdown.yml",
      "_pkgdown.yaml"
    ) |
    (r_source & !isTRUE(documentation_only)) |
    startsWith(rel, "data/") |
    startsWith(rel, "src/") |
    (startsWith(rel, "pkgdown/") & !assets) |
    startsWith(rel, "inst/")
  if (any(structural)) {
    return(site_plan(FALSE, "structural source change"))
  }

  target <- rep(NA_character_, length(rel))
  target[rel %in% c("README.Rmd", "README.md", "index.md")] <- "home"
  target[rel == "NEWS.md"] <- "news"
  target[startsWith(rel, "R/") | startsWith(rel, "man/")] <- "reference"
  target[startsWith(rel, "vignettes/tutorials/")] <- "tutorials"
  target[startsWith(rel, "vignettes/") & is.na(target)] <- "articles"
  target[assets] <- "assets"
  if (anyNA(target)) return(site_plan(TRUE, "unclassified source change"))

  list(
    kind = "sections",
    targets = unique(target),
    lazy = TRUE,
    reference_lazy = !any(startsWith(rel, "R/")),
    articles_lazy = all(grepl("[.](Rmd|rmd|qmd)$", rel[target == "articles"])),
    reference_files = basename(rel[
      target == "reference" & grepl("[.]Rd$", rel)
    ]),
    article_names = if (
      all(grepl(
        "[.](Rmd|rmd|qmd)$",
        rel[target == "articles"]
      ))
    ) {
      sub(
        "[.](Rmd|rmd|qmd)$",
        "",
        sub("^vignettes/", "", rel[target == "articles"])
      )
    } else character(),
    article_index = TRUE,
    reference_index_only = FALSE,
    reference_unscoped = any(
      target == "reference" & !startsWith(rel, "R/") & !grepl("[.]Rd$", rel)
    ),
    document = needs_document,
    readme = needs_readme,
    reason = "targeted section rebuild"
  )
}

article_index_signature <- function(path) {
  if (!fs::file_exists(path)) return(NA_character_)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines)) return("")

  yaml <- character()
  first <- sub("^\ufeff", "", lines[[1L]])
  if (identical(trimws(first), "---") && length(lines) > 1L) {
    closing <- which(trimws(lines[-1L]) %in% c("---", "..."))
    if (length(closing)) yaml <- lines[seq_len(closing[[1L]] + 1L)]
  }
  directives <- grep(
    "[\\]Vignette(IndexEntry|Engine|Encoding)[{]",
    lines,
    value = TRUE
  )
  paste(c(yaml, directives), collapse = "\n")
}

scope_article_index <- function(plan, changed, root, build_root) {
  if (
    !identical(plan$kind, "sections") ||
      !"articles" %in% plan$targets ||
      !length(plan$article_names) ||
      is.null(build_root) ||
      identical(root, build_root)
  ) {
    return(plan)
  }

  relative <- display_paths(changed, root)
  articles <- relative[
    startsWith(relative, "vignettes/") &
      grepl("[.](Rmd|rmd|qmd)$", relative)
  ]
  if (!length(articles)) return(plan)
  plan$article_index <- any(vapply(
    articles,
    function(path) {
      !identical(
        article_index_signature(fs::path(root, path)),
        article_index_signature(fs::path(build_root, path))
      )
    },
    logical(1)
  ))
  plan
}

parsed_r_code <- function(path) {
  if (!fs::file_exists(path)) return(NULL)
  tryCatch(
    parse(path, keep.source = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
}

validate_r_source <- function(path, root) {
  if (
    !fs::file_exists(path) ||
      !identical(tolower(fs::path_ext(path)), "r")
  ) {
    return(invisible(TRUE))
  }
  error <- tryCatch(
    {
      parse(path, keep.source = FALSE, encoding = "UTF-8")
      NULL
    },
    error = function(condition) condition
  )
  if (is.null(error)) return(invisible(TRUE))

  relative <- relative_to_root(path, root)
  first <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]][[1L]]
  match <- regexec(
    "^.*:([0-9]+):([0-9]+):[[:space:]]*(.*)$",
    first,
    perl = TRUE
  )
  pieces <- regmatches(first, match)[[1L]]
  if (length(pieces) == 4L) {
    location <- sprintf("%s:%s", relative, pieces[[2L]])
    cause <- sprintf("%s at column %s", pieces[[4L]], pieces[[3L]])
  } else {
    location <- relative
    cause <- first
  }
  condition <- structure(
    list(
      message = paste(
        sprintf("Invalid R syntax in %s.", relative),
        sprintf("Quitting from %s", location),
        "Caused by error in `parse()`:",
        cause,
        sep = "\n"
      ),
      call = NULL
    ),
    class = c("updoc_r_syntax_error", "error", "condition")
  )
  stop(condition)
}

validate_changed_r_sources <- function(changed, root) {
  relative <- display_paths(changed, root)
  validate_all <- any(relative %in% c("<initial>", "<rebuild>"))
  paths <- if (validate_all) {
    directory <- fs::path(root, "R")
    if (dir.exists(directory)) {
      candidates <- fs::dir_ls(
        directory,
        recurse = TRUE,
        type = "file",
        fail = FALSE
      )
      candidates[tolower(fs::path_ext(candidates)) == "r"]
    } else character()
  } else {
    changed[
      startsWith(relative, "R/") &
        tolower(fs::path_ext(changed)) == "r"
    ]
  }
  for (path in paths) validate_r_source(path, root)
  invisible(TRUE)
}

r_changes_are_documentation_only <- function(changed, root, build_root) {
  relative <- display_paths(changed, root)
  r_changes <- startsWith(relative, "R/")
  if (!any(r_changes) || is.null(build_root) || identical(root, build_root)) {
    return(FALSE)
  }
  all(vapply(
    relative[r_changes],
    function(path) {
      current <- parsed_r_code(fs::path(root, path))
      previous <- parsed_r_code(fs::path(build_root, path))
      !is.null(current) && !is.null(previous) && identical(current, previous)
    },
    logical(1)
  ))
}

describe_build_plan <- function(plan, document = TRUE) {
  targets <- character()
  if (isTRUE(plan$document) && isTRUE(document)) {
    targets <- c(targets, "package documentation")
  }
  if (isTRUE(plan$readme)) targets <- c(targets, "README")
  if (identical(plan$kind, "site")) {
    targets <- c(
      targets,
      if (isTRUE(plan$lazy)) {
        "incremental site"
      } else {
        "full site"
      }
    )
  } else {
    for (target in plan$targets) {
      label <- switch(
        target,
        home = "home page",
        reference = if (isTRUE(plan$reference_index_only)) {
          "reference index"
        } else if (length(plan$reference_files)) {
          paste0("reference: ", paste(plan$reference_files, collapse = ", "))
        } else "reference",
        articles = if (length(plan$article_names)) {
          paste0("article: ", paste(plan$article_names, collapse = ", "))
        } else "articles",
        tutorials = "tutorials",
        news = "news",
        assets = "site assets",
        target
      )
      targets <- c(targets, label)
    }
  }
  paste(unique(targets), collapse = ", ")
}

new_build_progress <- function(
  operation,
  target = NULL,
  current = NULL,
  total = NULL
) {
  list(
    operation = as.character(operation)[[1L]],
    target = if (is.null(target) || !nzchar(target)) NULL else target,
    current = if (is.null(current)) NULL else as.integer(current),
    total = if (is.null(total)) NULL else as.integer(total),
    updated = as.numeric(Sys.time())
  )
}

format_build_progress <- function(progress, styled = FALSE) {
  text <- progress$operation
  if (!is.null(progress$target) && nzchar(progress$target)) {
    target <- if (isTRUE(styled)) {
      cli::style_bold(progress$target)
    } else progress$target
    text <- sprintf("%s: %s", text, target)
  }
  if (!is.null(progress$current) && !is.null(progress$total)) {
    count <- sprintf("[%d/%d]", progress$current, progress$total)
    if (isTRUE(styled)) count <- cli::col_silver(count)
    text <- sprintf("%s %s", text, count)
  }
  text
}

write_build_progress <- function(path, progress) {
  if (is.null(path)) return(invisible(FALSE))
  temporary <- sprintf("%s.%d.tmp", path, Sys.getpid())
  ok <- tryCatch(
    {
      saveRDS(progress, temporary)
      if (.Platform$OS.type == "windows" && file.exists(path)) unlink(path)
      if (!file.rename(temporary, path))
        stop("Could not publish build progress")
      TRUE
    },
    error = function(e) FALSE
  )
  if (file.exists(temporary)) unlink(temporary)
  invisible(ok)
}

report_build_progress <- function(
  state,
  operation,
  target = NULL,
  current = NULL,
  total = NULL,
  announce = TRUE
) {
  progress <- new_build_progress(operation, target, current, total)
  state$progress <- progress
  if (isTRUE(state$trace_enabled)) {
    state$trace[[length(state$trace) + 1L]] <- progress
  }
  if (isTRUE(state$worker)) {
    write_build_progress(state$progress_path, progress)
  } else if (isTRUE(announce)) {
    user_message(format_build_progress(progress, styled = TRUE), "info")
  }
  invisible(progress)
}

finish_build_trace <- function(state, outcome) {
  if (!isTRUE(state$trace_enabled)) return(invisible(NULL))
  progress <- new_build_progress(outcome)
  state$trace[[length(state$trace) + 1L]] <- progress
  invisible(progress)
}

sync_build_progress <- function(state) {
  path <- state$progress_path
  if (is.null(path) || !file.exists(path)) return(invisible(FALSE))
  progress <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(progress) || identical(progress, state$progress)) {
    return(invisible(FALSE))
  }
  state$progress <- progress
  user_message(
    format_build_progress(progress, styled = TRUE),
    "info",
    asynchronous = TRUE
  )
  invisible(TRUE)
}

clear_build_progress <- function(state) {
  state$progress <- NULL
  path <- state$progress_path
  if (!is.null(path) && file.exists(path)) unlink(path)
  invisible(NULL)
}

validate_front_matter <- function(path, root) {
  if (
    !fs::file_exists(path) ||
      !tolower(fs::path_ext(path)) %in% c("rmd", "qmd", "md")
  ) {
    return(invisible(TRUE))
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines)) return(invisible(TRUE))
  first <- sub("^\ufeff", "", lines[[1L]])
  if (!identical(trimws(first), "---")) return(invisible(TRUE))

  closing <- which(trimws(lines[-1L]) %in% c("---", "..."))
  relative <- relative_to_root(path, root)
  if (!length(closing)) {
    updoc_abort(sprintf(
      "Unclosed YAML front matter in %s; add a closing `---` delimiter.",
      relative
    ))
  }

  metadata <- rmarkdown::yaml_front_matter(path)
  title <- metadata$title
  missing_title <- is.null(title) ||
    !length(title) ||
    is.na(title[[1L]]) ||
    !nzchar(trimws(as.character(title[[1L]])))
  if (missing_title) {
    updoc_warn(sprintf(
      "YAML front matter in %s has no non-empty `title`.",
      relative
    ))
  }
  validate_vignette_engines(lines, relative, root)
  invisible(TRUE)
}

vignette_builders <- function(root) {
  description <- fs::path(root, "DESCRIPTION")
  if (!fs::file_exists(description)) return("utils")
  value <- read.dcf(description, fields = "VignetteBuilder")[[1L]]
  if (is.na(value) || !nzchar(trimws(value))) return("utils")
  unique(c(trimws(strsplit(value, ",", fixed = TRUE)[[1L]]), "utils"))
}

validate_vignette_engines <- function(lines, relative, root) {
  pattern <- paste0(
    "[[:space:]]*%+[[:space:]]*\\\\VignetteEngine",
    "\\{([^}]*(\\{[^}]*\\})*[^}]*)\\}.*"
  )
  declarations <- grep(pattern, lines, value = TRUE, useBytes = TRUE)
  if (!length(declarations)) return(invisible(TRUE))
  engines <- trimws(gsub(pattern, "\\1", declarations, useBytes = TRUE))
  builders <- vignette_builders(root)

  for (engine in engines) {
    qualified <- grepl("::", engine, fixed = TRUE)
    resolved <- tryCatch(
      if (qualified) {
        tools::vignetteEngine(engine)
      } else {
        tools::vignetteEngine(engine, package = builders)
      },
      error = function(condition) {
        updoc_abort(sprintf(
          "Invalid vignette engine `%s` in %s: %s",
          engine,
          relative,
          conditionMessage(condition)
        ))
      }
    )
    if (!resolved$package %in% builders) {
      updoc_abort(sprintf(
        paste0(
          "Vignette engine `%s` in %s is not declared by DESCRIPTION's ",
          "`VignetteBuilder`; add `VignetteBuilder: %s`."
        ),
        engine,
        relative,
        resolved$package
      ))
    }
  }
  invisible(TRUE)
}

validate_changed_front_matter <- function(changed, root) {
  relative <- display_paths(changed, root)
  validate_all <- any(relative %in% c("<initial>", "<rebuild>"))
  paths <- if (validate_all) {
    directory <- fs::path(root, "vignettes")
    if (dir.exists(directory)) {
      fs::dir_ls(directory, recurse = TRUE, type = "file", fail = FALSE)
    } else character()
  } else {
    changed[startsWith(relative, "vignettes/")]
  }
  for (path in paths) validate_front_matter(path, root)
  invisible(TRUE)
}

display_paths <- function(paths, root) {
  rel <- vapply(paths, relative_to_root, character(1), root = root)
  rel[rel == ""] <- "."
  unname(rel)
}
