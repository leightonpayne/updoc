test_that("an initial failed build recovers into a complete usable site", {
  skip_if(Sys.getenv("UPDOC_INTEGRATION_TESTS") != "true",
          "Set UPDOC_INTEGRATION_TESTS=true for live integration")
  old_options <- options(updoc.poll_interval = .05)
  on.exit(options(old_options), add = TRUE)

  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  cat(c("Suggests: knitr", "VignetteBuilder: knitr"),
      file = file.path(root, "DESCRIPTION"), sep = "\n", append = TRUE)
  writeLines(c(
    "url: https://example.test",
    "template:",
    "  bootstrap: 5"
  ), file.path(root, "_pkgdown.yml"))
  writeLines(c(
    "# Fixture", "", "Initial build recovery home."
  ), file.path(root, "README.md"))
  writeLines(c(
    "#' Say hello", "#' @return A greeting.", "#' @export",
    "hello <- function() 'hello'"
  ), file.path(root, "R", "hello.R"))
  dir.create(file.path(root, "man"))
  writeLines(c(
    "\\name{hello}", "\\alias{hello}", "\\title{Say hello}",
    "\\usage{hello()}", "\\value{A greeting.}",
    "\\description{Return a friendly greeting.}"
  ), file.path(root, "man", "hello.Rd"))
  dir.create(file.path(root, "vignettes"))
  article <- file.path(root, "vignettes", "article.Rmd")
  writeLines(c("---", "title: Missing close", "", "# Broken"), article)
  expect_false(dir.exists(file.path(root, "docs")))

  site <- preview(root, document = TRUE, strategy = "smart",
                  browse = FALSE, quiet = TRUE)
  on.exit(suppressMessages(site$stop()), add = TRUE, after = FALSE)
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && identical(current$phase, "error")
  }))
  failed <- site$status()
  expect_identical(failed$rebuild_count, 0L)
  expect_match(failed$last_error$message, "Unclosed YAML front matter",
               fixed = TRUE)
  expect_false(file.exists(file.path(failed$site_dir, "index.html")))
  placeholder <- fetch_preview(failed$url)
  expect_true(placeholder$ok)
  expect_match(rawToChar(placeholder$body), "updoc-placeholder", fixed = TRUE)

  writeLines(c(
    "---", "title: Recovered article", "output: html_document", "vignette: >",
    "  %\\VignetteIndexEntry{Recovered article}",
    "  %\\VignetteEngine{knitr::rmarkdown}",
    "  %\\VignetteEncoding{UTF-8}", "---", "",
    "Recovered article content."
  ), article)
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && identical(current$phase, "ready") &&
      current$rebuild_count > failed$rebuild_count
  }, timeout = 60))

  ready <- site$status()
  expect_null(ready$last_error)
  expect_identical(ready$rebuild_count, 1L)
  expect_true(all(file.exists(file.path(ready$site_dir, c(
    "index.html", "reference/index.html", "reference/hello.html",
    "articles/article.html"
  )))))
  home <- fetch_preview(ready$url)
  expect_true(home$ok)
  if (isTRUE(home$ok)) {
    expect_match(rawToChar(home$body), "Initial build recovery home.",
                 fixed = TRUE)
    expect_false(grepl("updoc-placeholder", rawToChar(home$body), fixed = TRUE))
  }
  expect_match(paste(readLines(
    file.path(ready$site_dir, "articles", "article.html"), warn = FALSE
  ), collapse = "\n"), "Recovered article content.", fixed = TRUE)
})

test_that("preview handles warm start, targeted edits, diagnostics, queuing, and recovery", {
  skip_if(Sys.getenv("UPDOC_INTEGRATION_TESTS") != "true",
          "Set UPDOC_INTEGRATION_TESTS=true for live integration")
  old_options <- options(updoc.poll_interval = .05)
  on.exit(options(old_options), add = TRUE)

  root <- make_package()
  second_root <- make_package()
  on.exit(unlink(c(root, second_root), recursive = TRUE), add = TRUE)
  cat(c("Suggests: knitr", "VignetteBuilder: knitr"),
      file = file.path(root, "DESCRIPTION"), sep = "\n", append = TRUE)
  writeLines(c(
    "url: https://example.test",
    "destination: preview",
    "development:",
    "  mode: devel",
    "template:",
    "  bootstrap: 5"
  ), file.path(root, "_pkgdown.yml"))
  dir.create(file.path(root, "vignettes"))
  article <- file.path(root, "vignettes", "article.Rmd")
  writeLines(c(
    "---", "title: Valid article", "output: html_document", "vignette: >",
    "  %\\VignetteEngine{knitr::rmarkdown}", "---", "", "# Article", "",
    "Initial content."
  ), article)
  warm_site <- file.path(root, "preview", "dev")
  dir.create(warm_site, recursive = TRUE)
  writeLines(
    "<!doctype html><h1>Existing pkgdown output</h1>",
    file.path(warm_site, "index.html")
  )

  blocked_port <- httpuv::randomPort()
  blocker <- httpuv::startServer(
    "127.0.0.1",
    blocked_port,
    list(call = function(req) {
      list(status = 200L, headers = list(), body = "busy")
    }),
    quiet = TRUE
  )
  on.exit(if (blocker$isRunning()) blocker$stop(), add = TRUE, after = FALSE)

  site <- preview(
    root, document = TRUE, strategy = "smart",
    port = blocked_port, browse = FALSE, quiet = TRUE
  )
  on.exit(suppressMessages(site$stop()), add = TRUE, after = FALSE)

  expect_s3_class(site, "updoc_preview")
  initial <- site$status()
  expect_true(initial$active)
  expect_false(identical(initial$port, blocked_port))
  warm <- rawToChar(fetch_preview(initial$url)$body)
  expect_match(warm, "Existing pkgdown output", fixed = TRUE)
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && current$rebuild_count == 1L
  }))

  ready <- site$status()
  expect_identical(ready$phase, "ready")
  expect_null(ready$last_error)
  expect_true(file.exists(file.path(ready$site_dir, "index.html")))
  expect_false(startsWith(ready$site_dir, paste0(root, .Platform$file.sep)))
  expect_true(fetch_preview(ready$url)$ok)

  # Asset-only rebuild
  before_assets <- ready$rebuild_count
  dir.create(file.path(root, "pkgdown"))
  writeLines("body { outline-color: transparent; }",
             file.path(root, "pkgdown", "extra.css"))
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && current$rebuild_count > before_assets
  }))
  expect_true(file.exists(file.path(site$status()$site_dir, "extra.css")))

  # Vignette validation errors, warnings, render failure, and recovery
  before_engine <- site$status()
  writeLines(c(
    "---", "title: Invalid engine", "output: html_document", "vignette: >",
    "  %\\VignetteEngine{knir::rmarkdown}", "---", "", "# Broken"
  ), article)
  expect_true(wait_until(function() !is.null(site$status()$last_error)))
  engine_error <- site$status()
  expect_identical(engine_error$phase, "error")
  expect_identical(engine_error$rebuild_count, before_engine$rebuild_count)
  expect_identical(engine_error$site_dir, before_engine$site_dir)
  expect_match(engine_error$last_error$message,
               "Invalid vignette engine `knir::rmarkdown`", fixed = TRUE)
  expect_true(fetch_preview(engine_error$url)$ok)

  writeLines(c("---", "title: Missing close", "", "# Broken"), article)
  expect_true(wait_until(function() {
    error <- site$status()$last_error
    !is.null(error) && grepl("Unclosed YAML front matter", error$message, fixed = TRUE)
  }))

  writeLines(c("---", "title:", "output: html_document", "---", "", "# Article"),
             article)
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && is.null(current$last_error) &&
      current$rebuild_count > before_engine$rebuild_count
  }))
  expect_match(site$status()$warnings[[1L]]$summary, "no non-empty `title`", fixed = TRUE)

  writeLines(c(
    "---", "title: Render failure", "output: html_document", "---", "",
    "# Render failure", "", "```{r diagnostic-chunk}",
    "readLines('missing-diagnostic-file.csv')", "```"
  ), article)
  before_render_error <- site$status()
  expect_true(wait_until(function() {
    error <- site$status()$last_error
    !is.null(error) && grepl("Failed to render", error$summary, fixed = TRUE)
  }))
  render_error <- site$status()
  error_text <- paste(unlist(render_error$last_error), collapse = "\n")
  expect_match(error_text, "missing-diagnostic-file.csv", fixed = TRUE)
  expect_match(render_error$last_error$hint, "vignettes/article.Rmd", fixed = TRUE)
  expect_false(grepl("\033", error_text, fixed = TRUE))

  writeLines(c("---", "title: Repaired article", "output: html_document", "---", "",
               "Recovered content."), article)
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && is.null(current$last_error) &&
      current$rebuild_count > before_render_error$rebuild_count
  }))
  expect_length(site$status()$warnings, 0L)
  expect_match(paste(readLines(
    file.path(site$status()$site_dir, "articles", "article.html"), warn = FALSE
  ), collapse = "\n"), "Recovered content.", fixed = TRUE)

  # Batched figure creation, update, and removal
  before_batch <- site$status()$rebuild_count
  second_article <- file.path(root, "vignettes", "second.Rmd")
  third_article <- file.path(root, "vignettes", "third.Rmd")
  write_fig <- function(path, title, expr) {
    writeLines(c(
      "---", paste("title:", title), "output: html_document", "vignette: >",
      paste0("  %\\VignetteIndexEntry{", title, "}"),
      "  %\\VignetteEngine{knitr::rmarkdown}",
      "  %\\VignetteEncoding{UTF-8}", "---", "",
      "```{r fig, echo=FALSE}", expr, "```"
    ), path)
  }
  write_fig(article, "Batch first", "plot(1:3)")
  write_fig(second_article, "Batch second", "plot(3:1)")
  write_fig(third_article, "Batch third", "plot(c(1, 3, 2))")
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && is.null(current$last_error) &&
      current$rebuild_count > before_batch
  }))
  batch <- site$status()
  expect_identical(batch$rebuild_count, before_batch + 1L)
  figures <- fs::dir_ls(file.path(batch$site_dir, "articles"),
                        recurse = TRUE, type = "file", regexp = "[.]png$", fail = FALSE)
  expect_gte(length(figures), 3L)
  expect_false(file.exists(file.path(root, "vignettes", "--find-assets.html")))

  figure <- figures[grepl("article_files", figures, fixed = TRUE)][[1L]]
  figure_hash <- unname(tools::md5sum(figure))
  reload_version <- jsonlite::fromJSON(rawToChar(fetch_preview(
    paste0(batch$url, "/__updoc__/version")
  )$body))$version
  before_figure <- batch$rebuild_count
  write_fig(article, "Batch first", "plot(3:1)")
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && is.null(current$last_error) &&
      current$rebuild_count > before_figure
  }))
  figure_update <- site$status()
  updated_figure <- fs::dir_ls(file.path(figure_update$site_dir, "articles"),
                               recurse = TRUE, type = "file",
                               regexp = "article_files.*[.]png$")[[1L]]
  expect_false(identical(figure_hash, unname(tools::md5sum(updated_figure))))
  updated_version <- jsonlite::fromJSON(rawToChar(fetch_preview(
    paste0(figure_update$url, "/__updoc__/version")
  )$body))$version
  expect_gt(updated_version, reload_version)

  before_figure_removal <- figure_update$rebuild_count
  for (p in c(article, second_article, third_article)) {
    writeLines(c("---", "title: No figure", "output: html_document", "---", "",
                 "Plain text."), p)
  }
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && is.null(current$last_error) &&
      current$rebuild_count > before_figure_removal
  }))
  expect_length(fs::dir_ls(file.path(site$status()$site_dir, "articles"),
                           recurse = TRUE, type = "file", regexp = "[.]png$",
                           fail = FALSE), 0L)

  # Superseded article rebuild & queued README.Rmd + R/ edits without dirtying source
  article_queue_before <- site$status()$rebuild_count
  writeLines(c(
    "---", "title: Queued article", "output: html_document", "---", "",
    "```{r, echo=FALSE}", "Sys.sleep(1.2)", "```", "", "Superseded article version."
  ), article)
  expect_true(wait_until(function() {
    progress <- site$status()$progress
    isTRUE(site$status()$building) && !is.null(progress) &&
      grepl("Building article", progress$operation, fixed = TRUE)
  }))
  writeLines(c("---", "title: Queued article", "output: html_document", "---", "",
               "Newest article version."), article)
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && is.null(current$last_error) &&
      current$rebuild_count > article_queue_before
  }))
  article_queue <- site$status()
  expect_identical(article_queue$rebuild_count, article_queue_before + 1L)
  article_html <- readLines(file.path(article_queue$site_dir, "articles", "article.html"),
                            warn = FALSE)
  expect_true(any(grepl("Newest article version", article_html, fixed = TRUE)))
  expect_false(any(grepl("Superseded article version", article_html, fixed = TRUE)))

  queued_before <- site$status()$rebuild_count
  writeLines(c(
    "---", "output: github_document", "---", "",
    "# Slow mirror build", "", "```{r, echo=FALSE}", "Sys.sleep(1.5)", "```"
  ), file.path(root, "README.Rmd"))
  expect_true(wait_until(function() {
    progress <- site$status()$progress
    isTRUE(site$status()$building) && !is.null(progress) &&
      identical(progress$operation, "Rendering README")
  }))
  writeLines(c("#' Intermediate documentation", "#' @export",
               "hello <- function() 'first'"), file.path(root, "R", "hello.R"))
  writeLines(c("#' Final documentation", "#' @export",
               "hello <- function() 'final'"), file.path(root, "R", "hello.R"))
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && current$rebuild_count > queued_before
  }))
  queued <- site$status()
  expect_identical(queued$rebuild_count, queued_before + 1L)
  expect_gte(queued$last_duration, 1.4)
  expect_true(file.exists(file.path(queued$site_dir, "reference", "hello.html")))
  expect_false(file.exists(file.path(root, "man", "hello.Rd")))
  expect_false(file.exists(file.path(root, "README.md")))

  # Ignored files do not trigger rebuilds
  ignored_before <- queued$rebuild_count
  writeLines("generated", file.path(queued$site_dir, "watch-proof.txt"))
  dir.create(file.path(root, ".git"), showWarnings = FALSE)
  writeLines("private", file.path(root, ".git", "HEAD"))
  service_for(.6)
  expect_identical(site$status()$rebuild_count, ignored_before)

  # R syntax error persists across unrelated edits until repaired
  before_syntax <- site$status()
  home_hash <- unname(tools::md5sum(file.path(before_syntax$site_dir, "index.html")))
  writeLines("hello <- function( {", file.path(root, "R", "hello.R"))
  expect_true(wait_until(function() identical(site$status()$phase, "error")))
  writeLines(c("# Fixture", "", "Changed while R was broken."),
             file.path(root, "README.md"))
  expect_true(wait_until(function() {
    status <- site$status()
    !status$building && "README.md" %in% status$last_changed
  }))
  syntax_error <- site$status()
  expect_identical(syntax_error$phase, "error")
  expect_identical(syntax_error$rebuild_count, before_syntax$rebuild_count)
  expect_match(syntax_error$last_error$summary, "Invalid R syntax", fixed = TRUE)
  expect_identical(unname(tools::md5sum(file.path(syntax_error$site_dir, "index.html"))),
                   home_hash)
  expect_true(fetch_preview(syntax_error$url)$ok)

  writeLines(c("#' Recovered documentation", "#' @export",
               "hello <- function() 'recovered'"), file.path(root, "R", "hello.R"))
  expect_true(wait_until(function() {
    current <- site$status()
    !current$building && is.null(current$last_error) &&
      current$rebuild_count > before_syntax$rebuild_count
  }, timeout = 60))
  expect_false(identical(site$status()$site_dir, before_syntax$site_dir))

  # Concurrent second preview stays isolated; stopping cleans up workspace
  writeLines(c("url: https://example.test", "template:", "  bootstrap: 5"),
             file.path(second_root, "_pkgdown.yml"))
  writeLines("broken <- function( {", file.path(second_root, "R", "hello.R"))
  second <- preview(second_root, document = FALSE, port = 0,
                    browse = FALSE, quiet = TRUE)
  on.exit(suppressMessages(second$stop()), add = TRUE, after = FALSE)
  expect_true(wait_until(function() !is.null(second$status()$last_error)))
  expect_match(second$status()$last_error$summary, "Invalid R syntax", fixed = TRUE)
  expect_true(site$status()$active)
  second$stop()
  expect_false(second$status()$active)

  workspace <- dirname(site$status()$site_dir)
  site$stop()
  expect_false(site$status()$active)
  expect_identical(site$status()$phase, "down")
  expect_false(dir.exists(workspace))
})

test_that("preview handles development mode switches, deletions, and shared index updates", {
  skip_if(Sys.getenv("UPDOC_INTEGRATION_TESTS") != "true",
          "Set UPDOC_INTEGRATION_TESTS=true for live integration")
  root <- make_package()
  scratch <- tempfile("updoc-destination-")
  on.exit(unlink(c(root, scratch), recursive = TRUE), add = TRUE)

  config <- file.path(root, "_pkgdown.yml")
  readme <- file.path(root, "README.md")
  configure <- function(mode, text, destination = NULL) {
    lines <- c("url: https://example.test", "development:",
               paste0("  mode: ", mode))
    if (!is.null(destination)) lines <- c(lines, paste0("  destination: ", destination))
    writeLines(c(lines, "template:", "  bootstrap: 5"), config)
    writeLines(c("# Fixture", "", text), readme)
  }
  dir.create(file.path(root, "vignettes", "articles"), recursive = TRUE)
  write_article <- function(path, title, text) {
    writeLines(c("---", paste("title:", title), "output: html_document", "---",
                 "", paste("#", title), "", text), path)
  }
  guide <- file.path(root, "vignettes", "articles", "guide.Rmd")
  unchanged_src <- file.path(root, "vignettes", "articles", "unchanged.Rmd")
  removed <- file.path(root, "vignettes", "articles", "removed.Rmd")
  write_article(guide, "Guide", "Original searchable text.")
  write_article(unchanged_src, "Unchanged", "Unchanged searchable text.")
  write_article(removed, "Removed article", "This article will be deleted.")
  configure("release", "First release content.")

  state <- new_preview_state(root, file.path(scratch, "work"), FALSE,
                             "smart", 4321, 1, TRUE)
  state$preview_dir <- scratch
  state$build_root <- file.path(scratch, "package")
  state$source_site_dir <- resolve_site_dir(root)
  mirror_package(root, state$build_root, state$source_site_dir)
  state$build_dir <- resolve_site_dir(state$build_root, state$destination)
  state$site_dir <- file.path(scratch, "site-0")
  dir.create(state$site_dir)
  state$snapshot <- source_snapshot(root, state$source_site_dir)

  rebuild <- function(changed, initial = FALSE) {
    spec <- build_worker_spec(state, changed, FALSE, initial, NULL)
    result <- suppressMessages(build_worker_job_selected(spec))
    expect_true(result$success, info = result$last_error$message)
    expect_true(apply_worker_result(state, result))
    publish_preview(state, state$publish_delta)
    paste(readLines(file.path(state$site_dir, "index.html"), warn = FALSE),
          collapse = "\n")
  }

  expect_match(rebuild(file.path(root, "<initial>"), TRUE),
               "First release content.", fixed = TRUE)
  expect_identical(state$build_dir, as.character(fs::path(state$destination)))
  expect_true(file.exists(file.path(state$site_dir, "articles", "removed.html")))

  # Switch to custom nested devel destination and exercise targeted article + shared indexes
  configure("devel", "Development content after reconfiguration.",
            destination = "preview/development")
  expect_match(rebuild(c(config, readme)),
               "Development content after reconfiguration.", fixed = TRUE)
  expect_identical(state$build_dir,
                   as.character(fs::path(state$destination, "preview", "development")))

  unchanged_md <- file.path(state$build_dir, "articles", "unchanged.md")
  Sys.setFileTime(unchanged_md, as.POSIXct("2000-01-01", tz = "UTC"))
  unchanged_mtime <- file.info(unchanged_md)$mtime
  write_article(guide, "Guide", "Updated searchable text.")
  added <- file.path(root, "vignettes", "articles", "added.Rmd")
  write_article(added, "Added", "New searchable article.")
  rebuild(c(guide, added))
  expect_identical(file.info(unchanged_md)$mtime, unchanged_mtime)
  search <- paste(readLines(file.path(state$site_dir, "search.json"), warn = FALSE),
                  collapse = "\n")
  expect_match(search, "Updated searchable text", fixed = TRUE)
  expect_match(search, "New searchable article", fixed = TRUE)
  expect_false(grepl("Original searchable text", search, fixed = TRUE))
  expect_true(file.exists(file.path(state$site_dir, "articles", "articles", "added.html")))
  llms <- paste(readLines(file.path(state$site_dir, "llms.txt"), warn = FALSE),
                collapse = "\n")
  expect_match(llms, "added.md", fixed = TRUE)

  # Switch back to release while deleting an article under lazy strategy
  configure("release", "Second release content after reconfiguration.")
  state$strategy <- "lazy"
  unlink(removed)
  expect_match(rebuild(c(config, readme, removed)),
               "Second release content after reconfiguration.", fixed = TRUE)
  expect_identical(state$build_dir, as.character(fs::path(state$destination)))
  expect_false(file.exists(file.path(state$site_dir, "articles", "removed.html")))
  expect_false(file.exists(file.path(state$site_dir, "articles", "removed.md")))
  expect_false(dir.exists(file.path(state$site_dir, "preview")))
  expect_false(any(grepl("Removed article", readLines(
    file.path(state$site_dir, "search.json"), warn = FALSE
  ), fixed = TRUE)))
  expect_false(dir.exists(file.path(root, "docs")))
})

test_that("preview and build use the selected installed Pandoc across processes", {
  skip_if(Sys.getenv("UPDOC_INTEGRATION_TESTS") != "true",
          "Set UPDOC_INTEGRATION_TESTS=true for live integration")
  directory <- resolve_pandoc(getOption("updoc.pandoc", NULL))
  version <- pandoc_binary_version(directory)
  expect_deprecations <- identical(version, "3.11") &&
    utils::packageVersion("pkgdown") == "2.2.1" &&
    utils::packageVersion("rmarkdown") == "2.32"
  version_check <- sprintf(
    "stopifnot(identical(as.character(rmarkdown::pandoc_version()), '%s'))", version
  )
  original_environment <- Sys.getenv(c("PATH", "RSTUDIO_PANDOC"), unset = NA_character_)
  original_pandoc <- rmarkdown::find_pandoc()
  original_options <- options(updoc.poll_interval = .05)
  root <- make_package()
  site <- NULL
  on.exit({
    if (!is.null(site)) try(site$stop(), silent = TRUE)
    unlink(root, recursive = TRUE)
    options(original_options)
    restore_pandoc_environment(original_environment)
    rmarkdown::find_pandoc(
      cache = FALSE,
      dir = if (is.null(original_pandoc$dir)) "" else original_pandoc$dir,
      version = if (is.null(original_pandoc$dir)) NULL else original_pandoc$version
    )
  }, add = TRUE)

  cat(c("Suggests: knitr, rmarkdown", "VignetteBuilder: knitr"),
      file = file.path(root, "DESCRIPTION"), sep = "\n", append = TRUE)
  writeLines(c("url: https://example.test", "template:", "  bootstrap: 5"),
             file.path(root, "_pkgdown.yml"))
  dir.create(file.path(root, "man"))
  writeLines(c(
    "\\name{hello}", "\\alias{hello}", "\\title{Say hello}",
    "\\usage{hello()}", "\\value{A greeting.}",
    "\\description{Return a friendly greeting.}"
  ), file.path(root, "man", "hello.Rd"))
  writeLines(c(
    "---", "output: github_document", "---", "", "# Fixture", "",
    "```{r, echo=FALSE, results='asis'}", version_check,
    sprintf("cat('updoc-readme-pandoc-%s')", version), "```"
  ), file.path(root, "README.Rmd"))
  dir.create(file.path(root, "vignettes"))
  article <- file.path(root, "vignettes", "article.Rmd")
  write_article <- function(marker) {
    writeLines(c(
      "---", "title: Pandoc selection", "output: html_document", "vignette: >",
      "  %\\VignetteIndexEntry{Pandoc selection}",
      "  %\\VignetteEngine{knitr::rmarkdown}",
      "  %\\VignetteEncoding{UTF-8}", "---", "",
      "```{r, echo=FALSE, results='asis'}", version_check,
      sprintf("cat('%s')", marker), "```"
    ), article)
  }
  write_article(paste0("updoc-article-initial-pandoc-", version))
  source_files <- file.path(root, c(
    "DESCRIPTION", "NAMESPACE", "R/hello.R", "man/hello.Rd", "README.Rmd",
    "vignettes/article.Rmd"
  ))
  source_hashes <- tools::md5sum(source_files)

  expect_parent_unchanged <- function() {
    expect_identical(Sys.getenv(names(original_environment), unset = NA_character_),
                     original_environment)
    expect_identical(rmarkdown::find_pandoc(), original_pandoc)
    expect_identical(tools::md5sum(source_files), source_hashes)
  }
  read_html <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_build_log <- function(result) {
    expect_type(result$build_log, "character")
    expect_length(result$build_log, 1L)
    expect_true(file.exists(result$build_log))
    text <- read_html(result$build_log)
    expect_true(nzchar(text))
    if (expect_deprecations) {
      expect_match(text, "[WARNING] Deprecated: --mathml.", fixed = TRUE)
      expect_match(text, "[WARNING] Deprecated: --mathjax.", fixed = TRUE)
    }
    text
  }
  expect_preview_ready <- function(minimum_builds) {
    expect_true(wait_until(function() {
      current <- site$status()
      !current$building &&
        (current$rebuild_count >= minimum_builds || identical(current$phase, "error"))
    }, timeout = 60))
    current <- site$status()
    expect_null(current$last_error)
    expect_identical(current$phase, "ready")
    expect_gte(current$rebuild_count, minimum_builds)
    current
  }

  site <- preview(root, document = FALSE, strategy = "lazy", browse = FALSE,
                  quiet = TRUE)
  expect_parent_unchanged()
  initial <- expect_preview_ready(1L)
  initial_log <- expect_build_log(initial)
  expect_match(initial_log, "Initialising site", fixed = TRUE)
  expect_match(read_html(file.path(initial$site_dir, "articles", "article.html")),
               paste0("updoc-article-initial-pandoc-", version), fixed = TRUE)
  expect_false(file.exists(file.path(root, "README.md")))
  expect_parent_unchanged()

  cat("\n\nPreview README update.\n", file = file.path(root, "README.Rmd"),
      append = TRUE)
  source_hashes <- tools::md5sum(source_files)
  readme_ready <- expect_preview_ready(initial$rebuild_count + 1L)
  expect_build_log(readme_ready)
  expect_match(read_html(file.path(readme_ready$site_dir, "index.html")),
               paste0("updoc-readme-pandoc-", version), fixed = TRUE)
  expect_false(file.exists(file.path(root, "README.md")))
  expect_parent_unchanged()

  write_article(paste0("updoc-article-lazy-pandoc-", version))
  Sys.setFileTime(article, Sys.time() + 2)
  source_hashes <- tools::md5sum(source_files)
  updated <- expect_preview_ready(readme_ready$rebuild_count + 1L)
  expect_build_log(updated)
  expect_match(read_html(file.path(updated$site_dir, "articles", "article.html")),
               paste0("updoc-article-lazy-pandoc-", version), fixed = TRUE)
  expect_parent_unchanged()
  site$stop()
  site <- NULL
  expect_identical(read_html(initial$build_log), initial_log)

  published <- build(root, destination = "published", document = FALSE,
                     quiet = TRUE, pandoc = directory)
  expect_s3_class(published, "updoc_build")
  publication_log <- expect_build_log(published)
  expect_match(publication_log, "Installing package fixture", fixed = TRUE)
  expect_match(read_html(file.path(published$site_dir, "index.html")),
               paste0("updoc-readme-pandoc-", version), fixed = TRUE)
  expect_match(read_html(file.path(published$site_dir, "articles", "article.html")),
               paste0("updoc-article-lazy-pandoc-", version), fixed = TRUE)
  expect_match(read_html(file.path(root, "README.md")),
               paste0("updoc-readme-pandoc-", version), fixed = TRUE)
  expect_parent_unchanged()
})

test_that("agent workflows: check(), port = 0, wait(), HTTP endpoints, and build()", {
  skip_if(Sys.getenv("UPDOC_INTEGRATION_TESTS") != "true",
          "Set UPDOC_INTEGRATION_TESTS=true for live integration")
  old_options <- options(updoc.poll_interval = 0.2)
  on.exit(options(old_options), add = TRUE)

  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c("url: https://example.test", "template:", "  bootstrap: 5"),
             file.path(root, "_pkgdown.yml"))
  writeLines(c("#' Say hello", "#' @export", "hello <- function() 'hello'"),
             file.path(root, "R", "hello.R"))

  res <- NULL
  json_lines <- utils::capture.output({
    res <- check(root, document = TRUE, format = "json")
  })
  expect_s3_class(res, "updoc_check")
  expect_true(res$ok)
  expect_true(file.exists(res$build_log))
  expect_false(dir.exists(file.path(root, "docs")))
  expect_false(dir.exists(file.path(root, "man")))

  parsed <- jsonlite::fromJSON(paste(json_lines, collapse = "\n"), simplifyVector = FALSE)
  expect_true(parsed$ok)
  expect_identical(parsed$build_log, res$build_log)
  expect_null(parsed$error)

  site <- preview(root, port = 0, browse = FALSE, quiet = TRUE)
  on.exit(suppressMessages(site$stop()), add = TRUE, after = FALSE)

  initial <- site$wait(timeout = 60)
  expect_identical(initial$phase, "ready")
  expect_identical(initial$rebuild_count, 1L)
  expect_true(initial$port >= 1L)

  writeLines(c("#' Say hello again", "#' @export", "hello <- function() 'hello'"),
             file.path(root, "R", "hello.R"))
  waited <- fetch_preview(paste0(initial$url, "/__updoc__/status?wait=1&timeout=60"),
                          timeout = 65)
  expect_true(waited$ok)
  status_json <- jsonlite::fromJSON(rawToChar(waited$body))
  expect_identical(status_json$phase, "ready")
  expect_identical(status_json$rebuild_count, 2L)
  expect_identical(status_json$url, initial$url)
  expect_true(nzchar(status_json$last_success))
  expect_true(file.exists(status_json$build_log))

  log_resp <- fetch_preview(paste0(initial$url, "/__updoc__/log"), timeout = 10)
  expect_true(log_resp$ok)
  expect_true(nzchar(rawToChar(log_resp$body)))
  expect_false(dir.exists(file.path(root, "man")))

  site$rebuild(full = TRUE)
  rebuilt <- site$wait(timeout = 60)
  expect_identical(rebuilt$phase, "ready")
  expect_identical(rebuilt$rebuild_count, 3L)

  site$stop()
  deployed <- build(root, destination = "deployed-site", document = FALSE,
                    lazy = FALSE, quiet = TRUE)
  expect_s3_class(deployed, "updoc_build")
  expect_false(deployed$lazy)
  expect_true(file.exists(file.path(deployed$site_dir, "index.html")))
})
