test_that("queued article publication includes earlier unpublished full output", {
  for (article_changes in c(TRUE, FALSE)) {
    root <- make_package()
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    state <- new_preview_state(root, "docs", FALSE, "smart", 4321, 1, TRUE)
    state$active <- TRUE
    state$preview_dir <- file.path(root, "preview")
    state$build_dir <- file.path(root, "work")
    state$site_dir <- file.path(state$preview_dir, "published")
    dir.create(state$build_dir)
    dir.create(state$site_dir, recursive = TRUE)
    writeLines("Old home", file.path(state$site_dir, "index.html"))
    writeLines("Old article", file.path(state$site_dir, "article.html"))
    writeLines("New home", file.path(state$build_dir, "index.html"))
    writeLines("Old article", file.path(state$build_dir, "article.html"))

    # A full/home build finishes while an article edit waits in the queue.
    state_queue_build(state, file.path(root, "vignettes", "article.Rmd"))
    full <- list(
      success = TRUE,
      snapshot = NULL,
      last_changed = "README.md",
      last_success = Sys.time(),
      last_duration = 1,
      last_warnings = character(),
      indexes_dirty = FALSE,
      reload_count = 1L,
      publish_delta = NULL
    )
    expect_true(apply_worker_result(state, full))
    expect_identical(state$reload_pending, 1L)
    expect_identical(
      readLines(file.path(state$site_dir, "index.html")),
      "Old home"
    )
    state_take_build_queue(state)

    # The next worker only describes its own change, which may be a no-op.
    result <- full
    result$last_changed <- "vignettes/article.Rmd"
    result$reload_count <- as.integer(article_changes)
    result$publish_delta <- list(write = character(), remove = character())
    if (article_changes) {
      article <- file.path(state$build_dir, "article.html")
      writeLines("New article", article)
      result$publish_delta$write <- stats::setNames(
        unname(tools::md5sum(article)),
        "article.html"
      )
    }
    state$job <- list(data = result)
    state_begin_build(state)
    expect_true(suppressMessages(check_build_job(state)))
    expect_identical(
      readLines(file.path(state$site_dir, "index.html")),
      "New home"
    )
    expect_identical(
      readLines(file.path(state$site_dir, "article.html")),
      if (article_changes) "New article" else "Old article"
    )
    expect_identical(state$reload_pending, 0L)
    expect_true(jsonlite::fromJSON(preview_status_json(state))$reload$full)
    unlink(root, recursive = TRUE)
  }
})

test_that("failed publication restores replaced, deleted, and newly added files", {
  for (failure in c("missing", "changed", "copy")) {
    root <- tempfile("updoc-publication-rollback-")
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    state <- new_preview_state(root, "docs", FALSE, "smart", 4321, 1, TRUE)
    state$preview_dir <- file.path(root, "preview")
    state$site_dir <- file.path(state$preview_dir, "site")
    state$build_dir <- file.path(root, "work")
    fs::dir_create(c(state$site_dir, state$build_dir))
    for (name in c("one.html", "two.html", "removed.html", "untouched.html")) {
      writeLines(paste("Original", name), file.path(state$site_dir, name))
    }
    before <- tools::md5sum(list.files(state$site_dir, full.names = TRUE))
    outputs <- file.path(state$build_dir, c("new.html", "one.html", "two.html"))
    for (path in outputs) writeLines(paste("Replacement", basename(path)), path)
    delta <- list(
      write = stats::setNames(
        unname(tools::md5sum(outputs)),
        basename(outputs)
      ),
      remove = "removed.html"
    )
    if (failure == "missing") unlink(outputs[[3L]])
    if (failure == "changed") writeLines("Changed after hashing", outputs[[3L]])
    if (failure == "copy") {
      copy_file <- fs::file_copy
      local_mocked_bindings(
        file_copy = function(path, new_path, ...) {
          if (
            identical(
              as.character(fs::path_norm(path)),
              as.character(fs::path_norm(outputs[[3L]]))
            )
          ) {
            writeLines("Partial copy", new_path)
            stop("Injected copy failure")
          }
          copy_file(path, new_path, ...)
        },
        .package = "fs"
      )
    }
    message <- switch(
      failure,
      missing = "Generated preview output is missing",
      changed = "Generated preview output changed before publication",
      copy = "Injected copy failure"
    )
    expect_error(publish_preview_delta(state, delta), message, fixed = TRUE)
    expect_identical(
      tools::md5sum(list.files(state$site_dir, full.names = TRUE)),
      before
    )
    expect_identical(list.files(state$preview_dir), "site")
    expect_identical(state$publish_generation, 0L)
    unlink(root, recursive = TRUE)
  }
})

test_that("unrelated saves cannot discard a failed source batch", {
  for (document in c(FALSE, TRUE)) {
    root <- make_package()
    mirror <- tempfile("updoc-failed-batch-")
    on.exit(unlink(c(root, mirror), recursive = TRUE), add = TRUE)
    fs::dir_create(file.path(root, "vignettes"))
    r_file <- file.path(root, "R", "hello.R")
    readme <- file.path(root, "README.md")
    article <- file.path(root, "vignettes", "article.Rmd")
    removed <- file.path(root, "vignettes", "removed.Rmd")
    writeLines("# Original home", readme)
    writeLines(c("---", "title: Original article", "---"), article)
    writeLines(c("---", "title: Removed article", "---"), removed)
    mirror_package(root, mirror)
    state <- new_preview_state(
      root,
      tempfile(),
      document,
      "smart",
      4321,
      1,
      TRUE
    )
    state$build_root <- mirror
    state$worker <- TRUE
    documented <- 0L
    local_mocked_bindings(
      run_documentation = function(state) {
        documented <<- documented + 1L
        list()
      },
      run_readme = function(state) invisible(NULL),
      run_pkgdown = function(state, plan) TRUE,
      refresh_shared_indexes = function(state) invisible(NULL),
      .package = "updoc"
    )

    writeLines("hello <- function( {", r_file)
    writeLines(c("---", "title: Changed article", "---"), article)
    unlink(removed)
    expect_false(run_rebuild_sync(state, c(r_file, article, removed)))
    expect_match(state$last_error$message, "Invalid R syntax", fixed = TRUE)

    writeLines("# Unrelated home edit", readme)
    expect_false(run_rebuild_sync(state, readme, force_full = TRUE))
    expect_match(state$last_error$message, "Invalid R syntax", fixed = TRUE)
    expect_identical(state$rebuild_count, 0L)
    expect_identical(
      readLines(file.path(mirror, "README.md")),
      "# Original home"
    )

    writeLines("hello <- function() 'repaired'", r_file)
    expect_true(run_rebuild_sync(state, readme, force_full = TRUE))
    expect_null(state$last_error)
    expect_identical(state$rebuild_count, 1L)
    expect_identical(
      readLines(file.path(mirror, "R", "hello.R")),
      readLines(r_file)
    )
    expect_identical(
      readLines(file.path(mirror, "vignettes", "article.Rmd")),
      readLines(article)
    )
    expect_false(file.exists(file.path(mirror, "vignettes", "removed.Rmd")))
    expect_identical(documented, if (document) 1L else 0L)
    unlink(c(root, mirror), recursive = TRUE)
  }
})

test_that("manual rebuilds reconcile unobserved edits and deletions", {
  root <- make_package()
  mirror <- tempfile("updoc-manual-sync-")
  on.exit(unlink(c(root, mirror), recursive = TRUE), add = TRUE)
  writeLines("old <- function() NULL", file.path(root, "R", "old.R"))
  fs::dir_create(file.path(root, "inst", "site"))
  writeLines("Source output", file.path(root, "inst", "site", "index.html"))
  mirror_package(root, mirror, file.path(root, "inst", "site"))
  state <- new_preview_state(root, tempfile(), FALSE, "smart", 4321, 1, TRUE)
  state$build_root <- mirror
  state$source_site_dir <- file.path(root, "inst", "site")
  state$worker <- TRUE
  plan <- NULL
  local_mocked_bindings(
    run_readme = function(state) invisible(NULL),
    run_pkgdown = function(state, value) {
      plan <<- value
      TRUE
    },
    refresh_shared_indexes = function(state) invisible(NULL),
    .package = "updoc"
  )
  writeLines("hello <- function() 'new value'", file.path(root, "R", "hello.R"))
  unlink(file.path(root, "R", "old.R"))
  fs::dir_create(file.path(root, "vignettes"))
  article <- file.path(root, "vignettes", "new.Rmd")
  writeLines(c("---", "title: Missing close"), article)
  expect_false(run_rebuild_sync(state, file.path(root, "<rebuild>")))
  expect_match(state$last_error$message, "Unclosed YAML", fixed = TRUE)
  writeLines(c("---", "title: New article", "---"), article)
  expect_true(run_rebuild_sync(state, file.path(root, "<rebuild>")))
  expect_false(plan$lazy)
  expect_identical(
    readLines(file.path(mirror, "R", "hello.R")),
    "hello <- function() 'new value'"
  )
  expect_false(file.exists(file.path(mirror, "R", "old.R")))
  expect_identical(
    readLines(file.path(mirror, "vignettes", "new.Rmd")),
    readLines(article)
  )
  expect_false(dir.exists(file.path(mirror, "inst", "site")))
})

test_that("shared indexes publish from the resolved site directory", {
  cleanup <- character()
  on.exit(unlink(cleanup, recursive = TRUE), add = TRUE)
  for (development in c("release", "dev", "preview/development")) {
    root <- make_package()
    work <- tempfile("shared-indexes-")
    cleanup <- c(cleanup, root, work)
    config <- c(
      "url: https://example.test",
      "llm-docs: false",
      "template:",
      "  bootstrap: 5"
    )
    config <- c(
      config,
      "development:",
      if (development == "release") {
        "  mode: release"
      } else {
        c("  mode: devel", paste0("  destination: ", development))
      }
    )
    writeLines(config, file.path(root, "_pkgdown.yml"))
    dir.create(file.path(root, "vignettes", "articles"), recursive = TRUE)
    writeLines(
      c("---", "title: Guide", "output: html_document", "---"),
      file.path(root, "vignettes", "articles", "guide.Rmd")
    )

    state <- new_preview_state(
      root,
      file.path(work, "output"),
      FALSE,
      "smart",
      4321,
      .1,
      TRUE
    )
    pkg <- pkgdown::as_pkgdown(
      root,
      override = list(
        destination = state$destination
      )
    )
    state$site_dir <- as.character(pkg$dst_path)
    state$published_site_dir <- file.path(work, "published")
    fs::dir_create(c(
      file.path(state$site_dir, "articles"),
      state$published_site_dir
    ))
    writeLines("{}", file.path(state$site_dir, "pkgdown.yml"))
    writeLines(
      c(
        '<html><head><meta property="og:title" content="Guide"></head>',
        '<body><main><div class="section" id="updated">',
        '<h1>Updated guide</h1><p>Fresh searchable content.</p>',
        '</div></main></body></html>'
      ),
      file.path(state$site_dir, "articles", "guide.html")
    )
    writeLines(
      '<html><body><main><h1>Home</h1></main></body></html>',
      file.path(state$site_dir, "index.html")
    )
    writeLines(
      '[{"old":true}]',
      file.path(state$published_site_dir, "search.json")
    )
    state$publish_delta <- list(write = character(), remove = character())
    state$indexes_dirty <- TRUE

    refresh_shared_indexes(state)

    expected <- c("search.json", "articles/articles/guide.html")
    expect_setequal(names(state$publish_delta$write), expected)
    expect_length(state$publish_delta$remove, 0L)
    expect_false(state$indexes_dirty)
    search <- file.path(state$site_dir, "search.json")
    expect_match(
      paste(readLines(search, warn = FALSE), collapse = "\n"),
      "Fresh searchable content",
      fixed = TRUE
    )

    state$build_dir <- state$site_dir
    state$site_dir <- state$published_site_dir
    state$preview_dir <- work
    publish_preview_delta(state, state$publish_delta)
    expect_true(file.exists(file.path(state$site_dir, "search.json")))
    expect_true(file.exists(file.path(state$site_dir, expected[[2L]])))
    expect_identical(
      unname(tools::md5sum(search)),
      unname(tools::md5sum(
        file.path(state$site_dir, "search.json")
      ))
    )
  }
})
