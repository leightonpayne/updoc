build_article_direct <- function(pkg, article, override) {
  article_row <- match(article, pkg$vignettes$name)
  input <- if (is.na(article_row)) {
    article
  } else {
    pkg$vignettes$file_in[[article_row]]
  }
  output <- if (is.na(article_row)) {
    fs::path("articles", paste0(article, ".html"))
  } else {
    pkg$vignettes$file_out[[article_row]]
  }
  support <- fs::path(
    pkg$dst_path,
    paste0(tools::file_path_sans_ext(output), "_files")
  )
  if (dir.exists(support)) fs::dir_delete(support)

  messages <- character()
  error <- tryCatch(
    withCallingHandlers(
      {
        pkgdown::build_article(
          article,
          pkg = pkg,
          lazy = FALSE,
          new_process = FALSE,
          override = override,
          quiet = TRUE
        )
        NULL
      },
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(condition) condition
  )
  if (is.null(error)) return(invisible(TRUE))

  output <- clean_diagnostic_text(messages)
  output_lines <- strsplit(output, "\n", fixed = TRUE)[[1L]]
  quitting <- grep(
    "^Quitting from[[:space:]]+",
    output_lines,
    value = TRUE
  )
  message <- c(
    sprintf("Failed to render '%s'.", input),
    if (length(quitting)) quitting[[length(quitting)]] else character(),
    "Caused by error:",
    conditionMessage(error)
  )
  stop(paste(message, collapse = "\n"), call. = FALSE)
}

build_targeted_articles <- function(state, pkg, articles, override) {
  articles <- unique(articles)
  total <- length(articles)
  for (index in seq_along(articles)) {
    if (state_should_yield_build(state)) return(invisible(FALSE))
    article <- articles[[index]]
    report_build_progress(state, "Building article", article, index, total)
    build_article_direct(pkg, article, override)
  }
  invisible(TRUE)
}
