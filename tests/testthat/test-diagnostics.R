test_that("render errors recover missing paths and source context", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "vignettes"))
  writeLines(
    c(
      "---",
      "title: Broken input",
      "---",
      "",
      "```{r load-data}",
      "readLines('data/missing.csv')",
      "```"
    ),
    file.path(root, "vignettes", "article.Rmd")
  )
  raw <- paste0(
    "\033[1mFailed to render 'vignettes/article.Rmd'.\033[22m\n",
    "Quitting from article.Rmd:5-7 [load-data]\n",
    "Caused by error:\n",
    "cannot open the connection"
  )

  error <- enrich_build_error(
    diagnostic_from_text(raw),
    changed = "vignettes/article.Rmd",
    roots = root
  )

  expect_identical(error$summary, "Failed to render 'vignettes/article.Rmd'.")
  expect_identical(error$location, "vignettes/article.Rmd:5-7 [load-data]")
  expect_match(error$cause, "data/missing.csv", fixed = TRUE)
  expect_match(error$hint, "vignettes/article.Rmd", fixed = TRUE)
  expect_identical(error$excerpt, "6 | readLines('data/missing.csv')")
  expect_false(grepl("\033", error$message, fixed = TRUE))
})

test_that("title warnings identify both conflicting declarations", {
  root <- tempfile("updoc-warning-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "vignettes"), recursive = TRUE)
  writeLines(
    c(
      "---",
      "title: Parsing command line values",
      "---",
      "",
      "%\\VignetteIndexEntry{Turn command-line text into R values}"
    ),
    file.path(root, "vignettes", "parser.Rmd")
  )
  warning <- paste0(
    "The vignette title specified in \\VignetteIndexEntry{} is different ",
    "from the title in the YAML metadata. The former is ",
    "\"Turn command-line text into R values\", and the latter is ",
    "\"Parsing command line values\"."
  )

  record <- build_warning_record(warning, root = root)

  expect_identical(record$summary, "Vignette titles do not match")
  expect_identical(record$location, "vignettes/parser.Rmd:2-5")
  expect_match(record$cause, "Turn command-line text", fixed = TRUE)
  expect_match(record$cause, "Parsing command line values", fixed = TRUE)
  expect_identical(
    record$excerpt,
    paste(
      c(
        "2 | title: Parsing command line values",
        "5 | %\\VignetteIndexEntry{Turn command-line text into R values}"
      ),
      collapse = "\n"
    )
  )
})

test_that("build() errors surface the Build log path directly in the error message", {
  skip_if_not(
    rmarkdown::pandoc_available("2.10.1"),
    "Pandoc >= 2.10.1 required"
  )
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("broken <- function( {", file.path(root, "R", "hello.R"))

  err <- tryCatch(
    build(root, document = FALSE, quiet = TRUE),
    error = identity
  )
  expect_s3_class(err, "error")
  expect_type(err$build_log, "character")
  expect_true(file.exists(err$build_log))
  expect_match(
    conditionMessage(err),
    paste0("Build log: ", err$build_log),
    fixed = TRUE
  )
})
