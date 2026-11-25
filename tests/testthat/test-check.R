test_that("check() reports syntax errors non-destructively in text and json formats", {
  skip_if_not(rmarkdown::pandoc_available("2.10.1"), "Pandoc >= 2.10.1 required")
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("broken <- function( {", file.path(root, "R", "hello.R"))

  result <- NULL
  out <- utils::capture.output({
    result <- check(root, document = FALSE, format = "json", error = FALSE)
  })
  expect_s3_class(result, "updoc_check")
  expect_false(result$ok)
  expect_true(file.exists(result$build_log))
  expect_match(result$error$summary, "Invalid R syntax", fixed = TRUE)
  expect_false(dir.exists(file.path(root, "docs")))
  expect_false(dir.exists(file.path(root, "man")))

  parsed <- jsonlite::fromJSON(paste(out, collapse = "\n"), simplifyVector = FALSE)
  expect_false(parsed$ok)
  expect_identical(parsed$build_log, result$build_log)
  expect_match(parsed$error$summary, "Invalid R syntax", fixed = TRUE)
  expect_true(is.list(parsed$warnings))

  thrown <- NULL
  json_out <- utils::capture.output({
    thrown <- tryCatch(
      check(root, document = FALSE, format = "json", error = TRUE),
      error = identity
    )
  })
  expect_s3_class(thrown, "updoc_check_error")
  expect_match(conditionMessage(thrown), "Build log:", fixed = TRUE)
  expect_true(file.exists(thrown$build_log))
  expect_s3_class(thrown$check, "updoc_check")
  expect_false(jsonlite::fromJSON(paste(json_out, collapse = "\n"))$ok)
})

test_that("check() refuses _pkgdown.yml destinations outside its temporary directory", {
  skip_if_not(rmarkdown::pandoc_available("2.10.1"), "Pandoc >= 2.10.1 required")
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c(
    "template:", "  bootstrap: 5",
    "development:", "  mode: devel", "  destination: ../escaped"
  ), file.path(root, "_pkgdown.yml"))

  result <- NULL
  utils::capture.output({
    result <- check(root, document = FALSE, format = "json", error = FALSE)
  })
  expect_false(result$ok)
  expect_match(result$error$summary, "temporary work directory", fixed = TRUE)
  expect_false(dir.exists(file.path(root, "docs")))
})

test_that("bundled SKILL.md is installed with the package", {
  skill <- system.file("skills", "updoc", "SKILL.md", package = "updoc")
  expect_true(nzchar(skill) && file.exists(skill))
  content <- paste(readLines(skill, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(content, "updoc::check", fixed = TRUE)
  expect_match(content, "/__updoc__/status", fixed = TRUE)
})
