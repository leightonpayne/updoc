make_pandoc_directory <- function(.env = parent.frame()) {
  directory <- tempfile("pandoc bin ")
  dir.create(directory)
  do.call(
    base::on.exit,
    list(
      bquote(unlink(.(directory), recursive = TRUE)),
      add = TRUE
    ),
    envir = .env
  )
  normalizePath(directory, winslash = "/", mustWork = TRUE)
}

mock_build_pandoc <- function(
  detected = NULL,
  version = "3.10.2",
  .env = parent.frame()
) {
  state <- new.env(parent = emptyenv())
  state$original <- list(
    dir = "/previous/pandoc",
    version = numeric_version("3.11")
  )
  state$selection <- state$original
  state$calls <- list()
  state$binary_version <- version
  local_mocked_bindings(
    pandoc_binary_version = function(directory) version,
    find_build_pandoc = function(cache = TRUE, dir = NULL, version = NULL) {
      state$calls[[length(state$calls) + 1L]] <- list(
        cache = cache,
        dir = dir,
        version = version
      )
      if (cache) return(state$selection)
      state$selection <- if (is.null(dir)) {
        if (is.null(detected)) {
          list(
            dir = Sys.getenv("RSTUDIO_PANDOC"),
            version = numeric_version(state$binary_version)
          )
        } else detected
      } else if (identical(dir, "")) {
        list(dir = NULL, version = numeric_version("0"))
      } else {
        list(dir = dir, version = numeric_version(version))
      }
      state$selection
    },
    .env = .env
  )
  state
}

pandoc_noise_messages <- c(
  "[WARNING] Deprecated: --mathml. Use --math-method=mathml instead.",
  "[WARNING] Deprecated: --mathjax. Use --math-method=mathjax[:URL] instead."
)

test_that("Pandoc discovery uses the ordinary installed version and preserves its cache", {
  directory <- make_pandoc_directory()
  cache <- mock_build_pandoc(
    list(dir = directory, version = numeric_version("3.11")),
    version = "3.11"
  )

  expect_identical(resolve_pandoc(), directory)
  expect_identical(cache$selection, cache$original)
  expect_identical(
    cache$calls[[2L]],
    list(cache = FALSE, dir = NULL, version = NULL)
  )
})

test_that("explicit Pandoc directories accept supported versions, including an optional pin", {
  directory <- make_pandoc_directory()
  for (version in c("2.10.1", "2.19", "3.10.2", "3.11")) {
    local_mocked_bindings(pandoc_binary_version = function(directory) version)
    expect_identical(resolve_pandoc(file.path(directory, ".")), directory)
  }
})

test_that("Pandoc paths and unsupported versions fail with useful setup instructions", {
  directory <- make_pandoc_directory()
  cache <- mock_build_pandoc(
    list(dir = directory, version = numeric_version("2.9.2")),
    version = "2.9.2"
  )

  for (value in list("", NA_character_, character(), c("a", "b"), 42)) {
    expect_error(resolve_pandoc(value), "one non-empty directory path")
  }
  expect_error(resolve_pandoc(tempfile()), "directory does not exist")
  expect_error(
    resolve_pandoc(directory),
    "Pandoc 2[.]9[.]2.*2[.]10[.]1 or newer"
  )
  expect_error(
    resolve_pandoc(),
    "https://pandoc.org/installing.html",
    fixed = TRUE
  )
  expect_identical(cache$selection, cache$original)
})

test_that("missing Pandoc discovery gives an actionable setup error", {
  cache <- mock_build_pandoc(list(dir = NULL, version = numeric_version("0")))
  expect_error(resolve_pandoc(), "A Pandoc installation could not be found")
  expect_identical(cache$selection, cache$original)
})

test_that("unusable Pandoc binaries are rejected", {
  directory <- make_pandoc_directory()
  expect_null(pandoc_binary_version(directory))
  local_mocked_bindings(pandoc_binary_version = function(directory) NULL)
  expect_error(resolve_pandoc(directory), "No usable Pandoc executable")
})

test_that("builds scope Pandoc selection and restore the environment and cache", {
  directory <- make_pandoc_directory()
  original <- Sys.getenv(c("PATH", "RSTUDIO_PANDOC"), unset = NA_character_)
  on.exit(restore_pandoc_environment(original), add = TRUE)
  Sys.setenv(PATH = "previous-path", RSTUDIO_PANDOC = "/previous/pandoc")
  before <- Sys.getenv(c("PATH", "RSTUDIO_PANDOC"), unset = NA_character_)
  cache <- mock_build_pandoc()
  evaluated <- 0L

  result <- with_build_pandoc(directory, {
    evaluated <- evaluated + 1L
    expect_identical(Sys.getenv("RSTUDIO_PANDOC"), directory)
    expect_identical(
      Sys.getenv("PATH"),
      paste(directory, "previous-path", sep = .Platform$path.sep)
    )
    expect_identical(cache$selection$dir, directory)
    expect_identical(as.character(cache$selection$version), "3.10.2")
    123L
  })

  expect_identical(result, 123L)
  expect_identical(evaluated, 1L)
  expect_identical(Sys.getenv(names(before), unset = NA_character_), before)
  expect_identical(cache$selection, cache$original)
  expect_identical(
    cache$calls[[2L]],
    list(cache = FALSE, dir = NULL, version = NULL)
  )
})

test_that("builds scope newer Pandoc, restore on error, and detect subprocess conflicts", {
  directory <- make_pandoc_directory()
  original <- Sys.getenv(c("PATH", "RSTUDIO_PANDOC"), unset = NA_character_)
  on.exit(restore_pandoc_environment(original), add = TRUE)

  cache <- mock_build_pandoc(version = "3.11")
  expect_identical(
    with_build_pandoc(directory, {
      expect_identical(as.character(cache$selection$version), "3.11")
      TRUE
    }),
    TRUE
  )
  expect_identical(cache$selection, cache$original)

  Sys.unsetenv(c("PATH", "RSTUDIO_PANDOC"))
  cache <- mock_build_pandoc()
  expect_error(
    with_build_pandoc(directory, {
      expect_identical(Sys.getenv("PATH"), directory)
      stop("render failed")
    }),
    "render failed"
  )
  expect_true(all(is.na(Sys.getenv(
    c("PATH", "RSTUDIO_PANDOC"),
    unset = NA_character_
  ))))
  expect_identical(cache$selection, cache$original)

  before <- Sys.getenv(c("PATH", "RSTUDIO_PANDOC"), unset = NA_character_)
  cache <- mock_build_pandoc(list(
    dir = path.expand("~/opt/pandoc"),
    version = numeric_version("3.11")
  ))
  evaluated <- FALSE
  expect_error(
    with_build_pandoc(directory, {
      evaluated <- TRUE
    }),
    "Build subprocesses would select Pandoc 3[.]11"
  )
  expect_false(evaluated)
  expect_identical(Sys.getenv(names(before), unset = NA_character_), before)
  expect_identical(cache$selection, cache$original)

  cache <- mock_build_pandoc()
  cache$original <- list(dir = NULL, version = numeric_version("0"))
  cache$selection <- cache$original
  expect_identical(with_build_pandoc(directory, TRUE), TRUE)
  expect_identical(cache$selection, cache$original)
  expect_identical(
    cache$calls[[length(cache$calls)]],
    list(cache = FALSE, dir = "", version = NULL)
  )
})

test_that("only verified Pandoc 3.11 deprecation messages are classified as noise", {
  for (version in c("3.11", "3.11.1", "3.11.10")) {
    expect_identical(
      known_pandoc_noise(pandoc_noise_messages, version),
      c(TRUE, TRUE)
    )
  }
  for (version in list(NULL, "", NA_character_, "3.10.2", "3.12", "4.0")) {
    expect_identical(
      known_pandoc_noise(pandoc_noise_messages, version),
      c(FALSE, FALSE)
    )
  }

  other_output <- c(
    "[WARNING] Could not fetch a resource.",
    "[ERROR] Deprecated: --mathml. Use --math-method=mathml instead.",
    paste0("prefix ", pandoc_noise_messages[[1L]]),
    paste0(pandoc_noise_messages[[1L]], " Additional information."),
    sub("mathml", "katex", pandoc_noise_messages[[1L]], fixed = TRUE),
    "[WARNING] Deprecated: --mathjax. Use --math-method=mathjax instead.",
    ""
  )
  expect_identical(
    known_pandoc_noise(other_output, "3.11"),
    rep(FALSE, length(other_output))
  )
})

test_that("stream filtering preserves raw logs and complete ordinary output", {
  log <- tempfile("updoc-output-", fileext = ".log")
  on.exit(unlink(log), add = TRUE)
  displayed <- character()
  output <- new_build_output(log, "3.11", emit = function(text) {
    displayed <<- c(displayed, text)
  })
  chunks <- c(
    "ordinary output\n[WARN",
    "ING] Deprecated: --mathml. Use --math-method=mathml instead.\r",
    "\n",
    paste0(pandoc_noise_messages[[2L]], "\n"),
    "[WARNING] Keep this warning.\r\n",
    "final partial line"
  )
  for (chunk in chunks) output$write(chunk)
  expect_false(grepl(
    "final partial",
    paste0(displayed, collapse = ""),
    fixed = TRUE
  ))
  output$flush()
  output$flush()
  expect_identical(
    paste0(displayed, collapse = ""),
    "ordinary output\n[WARNING] Keep this warning.\r\nfinal partial line"
  )
  expect_identical(
    readBin(log, "raw", n = file.info(log)$size),
    charToRaw(paste0(chunks, collapse = ""))
  )

  # Unterminated known message filtered on flush; unknown version passes through.
  displayed <- character()
  output <- new_build_output(log, "3.11", emit = function(text) {
    displayed <<- c(displayed, text)
  })
  output$write(pandoc_noise_messages[[1L]])
  output$flush()
  expect_length(displayed, 0L)
  expect_identical(
    readBin(log, "raw", n = file.info(log)$size),
    charToRaw(pandoc_noise_messages[[1L]])
  )

  displayed <- character()
  output <- new_build_output(log, NULL, emit = function(text) {
    displayed <<- c(displayed, text)
  })
  original <- paste0(paste(pandoc_noise_messages, collapse = "\n"), "\n")
  output$write(original)
  output$flush()
  expect_identical(paste0(displayed, collapse = ""), original)
})

test_that("captured builds filter nested noise while keeping diagnostics and raw logs", {
  script <- tempfile("updoc output child ", fileext = ".R")
  log <- tempfile("updoc-output-", fileext = ".log")
  on.exit(unlink(c(script, log)), add = TRUE)
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )

  writeLines(
    c(
      sprintf(
        "cat(%s, '\\n', sep = '', file = stderr())",
        deparse(pandoc_noise_messages[[1L]])
      ),
      sprintf(
        "cat(%s, '\\n', sep = '', file = stderr())",
        deparse(pandoc_noise_messages[[2L]])
      ),
      "cat('[WARNING] Genuine resource warning.\\n', file = stderr())",
      "cat('Nested stdout is retained.\\n')"
    ),
    script
  )
  result <- NULL
  displayed <- utils::capture.output(
    {
      result <- run_captured_build(
        function(script, rscript) {
          cat("Parent stdout is retained.\n")
          warning("Genuine R warning.", call. = FALSE, immediate. = TRUE)
          status <- system2(rscript, shQuote(script), stdout = "", stderr = "")
          list(status = status, value = 42L)
        },
        args = list(script = script, rscript = rscript),
        build_log = log,
        pandoc_version = "3.11"
      )
    },
    type = "message"
  )

  expect_identical(result, list(status = 0L, value = 42L))
  visible <- paste(displayed, collapse = "\n")
  expect_false(grepl("Deprecated:", visible, fixed = TRUE))
  expect_match(visible, "Genuine resource warning.", fixed = TRUE)
  expect_match(visible, "Genuine R warning.", fixed = TRUE)
  expect_match(visible, "Nested stdout is retained.", fixed = TRUE)
  expect_match(visible, "Parent stdout is retained.", fixed = TRUE)
  raw <- paste(readLines(log, warn = FALSE), collapse = "\n")
  for (message in pandoc_noise_messages)
    expect_match(raw, message, fixed = TRUE)
  expect_match(raw, "Genuine resource warning.", fixed = TRUE)

  # Failed subprocess preserves error, parent, filtered fields, and raw log.
  writeLines(
    c(
      sprintf(
        "cat(%s, '\\n', sep = '', file = stderr())",
        deparse(pandoc_noise_messages[[1L]])
      ),
      "cat('[ERROR] The input document is invalid.\\n', file = stderr())",
      "quit(status = 7L, save = 'no')"
    ),
    script
  )
  error <- NULL
  displayed <- utils::capture.output(
    {
      error <- tryCatch(
        run_captured_build(
          function(script, rscript) {
            status <- system2(
              rscript,
              shQuote(script),
              stdout = "",
              stderr = ""
            )
            if (status != 0L)
              stop(sprintf("Render failed with exit status %d", status))
            TRUE
          },
          args = list(script = script, rscript = rscript),
          build_log = log,
          pandoc_version = "3.11"
        ),
        error = identity
      )
    },
    type = "message"
  )

  expect_s3_class(error, "error")
  expect_match(
    conditionMessage(error),
    "Render failed with exit status 7",
    fixed = TRUE
  )
  expect_identical(error$build_log, log)
  expect_s3_class(error$parent, "condition")
  expect_false(grepl(
    "Deprecated:",
    paste(displayed, collapse = "\n"),
    fixed = TRUE
  ))
  expect_match(
    paste(displayed, collapse = "\n"),
    "The input document is invalid.",
    fixed = TRUE
  )
  raw <- paste(readLines(log, warn = FALSE), collapse = "\n")
  expect_match(raw, pandoc_noise_messages[[1L]], fixed = TRUE)
  expect_match(raw, "The input document is invalid.", fixed = TRUE)

  current <- error
  depth <- 0L
  while (inherits(current, "condition") && depth < 10L) {
    fields <- unlist(
      current[c("stdout", "stderr", "output")],
      use.names = FALSE
    )
    expect_false(any(grepl("Deprecated:", fields, fixed = TRUE)))
    current <- current$parent
    depth <- depth + 1L
  }
})
