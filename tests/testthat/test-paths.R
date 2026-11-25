test_that("preview isolation excludes generated and private package data", {
  root <- make_package()
  mirror <- tempfile("updoc-mirror-")
  on.exit(unlink(c(root, mirror), recursive = TRUE), add = TRUE)
  site_dir <- file.path(root, "inst", "site")
  dir.create(site_dir, recursive = TRUE)
  writeLines("generated", file.path(site_dir, "index.html"))
  dir.create(file.path(root, ".git"))
  writeLines("private", file.path(root, ".git", "HEAD"))
  dir.create(file.path(root, "renv", "library", "fixture"), recursive = TRUE)
  writeLines(
    "library data",
    file.path(root, "renv", "library", "fixture", "data")
  )
  writeLines("activate", file.path(root, "renv", "activate.R"))
  configure <- file.path(root, "configure")
  writeLines(c("#!/bin/sh", "exit 0"), configure)
  Sys.chmod(configure, "0755")

  mirror_package(root, mirror, site_dir)

  expect_true(file.exists(file.path(mirror, "DESCRIPTION")))
  expect_true(file.exists(file.path(mirror, "R", "hello.R")))
  expect_false(dir.exists(file.path(mirror, "inst", "site")))
  expect_false(dir.exists(file.path(mirror, ".git")))
  expect_false(dir.exists(file.path(mirror, "renv", "library")))
  expect_true(file.exists(file.path(mirror, "renv", "activate.R")))
  if (.Platform$OS.type != "windows") {
    mode <- as.integer(file.info(file.path(mirror, "configure"))$mode)
    expect_true(bitwAnd(mode, as.integer(as.octmode("0111"))) > 0L)
  }
})

test_that("preview synchronization preserves edits and deletions", {
  root <- make_package()
  mirror <- tempfile("updoc-mirror-")
  on.exit(unlink(c(root, mirror), recursive = TRUE), add = TRUE)
  mirror_package(root, mirror)
  source <- file.path(root, "R", "hello.R")
  target <- file.path(mirror, "R", "hello.R")

  writeLines("hello <- function() 'changed'", source)
  sync_package_changes(root, mirror, source)
  expect_identical(readLines(target), "hello <- function() 'changed'")

  expect_true(file.remove(source))
  sync_package_changes(root, mirror, source)
  expect_false(file.exists(target))
})

test_that("initial preview reuses only a usable generated site", {
  root <- make_package()
  target <- tempfile("updoc-initial-site-")
  missing_target <- tempfile("updoc-empty-site-")
  on.exit(unlink(c(root, target, missing_target), recursive = TRUE), add = TRUE)
  source <- file.path(root, "docs")
  dir.create(file.path(source, "assets"), recursive = TRUE)
  writeLines("<h1>Last usable site</h1>", file.path(source, "index.html"))
  writeLines("body {}", file.path(source, "assets", "site.css"))

  expect_true(prepare_initial_preview(root, source, target))
  expect_identical(
    readLines(file.path(target, "index.html")),
    "<h1>Last usable site</h1>"
  )
  expect_true(file.exists(file.path(target, "assets", "site.css")))

  expect_false(prepare_initial_preview(
    root,
    file.path(root, "missing-docs"),
    missing_target
  ))
  expect_true(dir.exists(missing_target))
})

test_that("preview builds refuse destinations outside their work directory", {
  root <- make_package()
  work <- tempfile("preview-output-safety-")
  on.exit(unlink(c(root, work), recursive = TRUE), add = TRUE)
  destination <- file.path(work, "output")
  external <- file.path(work, "external")
  fs::dir_create(external)
  sentinel <- file.path(external, "keep.txt")
  writeLines("Keep this external file unchanged.", sentinel)
  original <- unname(tools::md5sum(sentinel))
  state <- new_preview_state(root, destination, FALSE, "smart", 4321, .1, TRUE)
  full <- build_plan("smart", "<initial>")
  section <- build_plan("smart", "README.md")

  for (development in c("../external", "..", external)) {
    writeLines(c(
      "template:", "  bootstrap: 5", "development:", "  mode: devel",
      paste0("  destination: ", development)
    ), file.path(root, "_pkgdown.yml"))
    expect_error(run_pkgdown(state, full), "temporary work directory",
                 fixed = TRUE)
    expect_error(run_pkgdown(state, section), "temporary work directory",
                 fixed = TRUE)
    state$indexes_dirty <- TRUE
    state$pkgdown <- NULL
    expect_error(refresh_shared_indexes(state), "temporary work directory",
                 fixed = TRUE)
    expect_identical(unname(tools::md5sum(sentinel)), original)
    expect_identical(list.files(external), "keep.txt")
  }

  # Cached metadata must pass the same guard before shared outputs are written.
  state$pkgdown <- pkgdown::as_pkgdown(root, override = list(
    development = list(mode = "release"), destination = external
  ))
  expect_error(refresh_shared_indexes(state), "temporary work directory",
               fixed = TRUE)
  expect_identical(unname(tools::md5sum(sentinel)), original)
})

test_that("preview output rejects symlink roots and ancestors", {
  root <- make_package()
  work <- tempfile("preview-output-links-")
  destination <- file.path(work, "output")
  on.exit({
    if (fs::is_link(destination)) try(fs::link_delete(destination), silent = TRUE)
    unlink(c(root, work), recursive = TRUE)
  }, add = TRUE)
  external <- file.path(work, "external")
  fs::dir_create(c(external, destination))
  sentinel <- file.path(external, "keep.txt")
  writeLines("Keep this external file unchanged.", sentinel)
  original <- unname(tools::md5sum(sentinel))
  link <- file.path(destination, "linked")
  linked <- suppressWarnings(file.symlink(external, link))
  skip_if_not(linked, "Creating directory symlinks is unavailable")
  state <- new_preview_state(root, destination, FALSE, "smart", 4321, .1, TRUE)
  full <- build_plan("smart", "<initial>")

  for (development in c("linked", "linked/not-created")) {
    writeLines(c(
      "template:", "  bootstrap: 5", "development:", "  mode: devel",
      paste0("  destination: ", development)
    ), file.path(root, "_pkgdown.yml"))
    expect_error(run_pkgdown(state, full), "temporary work directory",
                 fixed = TRUE)
    expect_identical(unname(tools::md5sum(sentinel)), original)
    expect_identical(list.files(external), "keep.txt")
  }

  # The allowed work root itself may have been replaced by a symlink.
  unlink(link)
  fs::dir_delete(destination)
  expect_true(file.symlink(external, destination))
  writeLines(c("development:", "  mode: release"),
             file.path(root, "_pkgdown.yml"))
  expect_error(run_pkgdown(state, full), "temporary work directory",
               fixed = TRUE)
  expect_identical(unname(tools::md5sum(sentinel)), original)
  expect_identical(list.files(external), "keep.txt")
})

test_that("relative nested preview directories remain valid", {
  root <- make_package()
  work <- tempfile("preview-output-nested-")
  on.exit(unlink(c(root, work), recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, file.path(work, "output"), FALSE,
                             "smart", 4321, .1, TRUE)
  writeLines(c(
    "development:", "  mode: devel", "  destination: preview/development"
  ), file.path(root, "_pkgdown.yml"))
  pkg <- pkgdown::as_pkgdown(root, override = list(
    destination = state$destination
  ))
  expected <- as.character(fs::path_norm(fs::path(
    state$destination, "preview", "development"
  )))
  expect_identical(checked_preview_output_dir(state, pkg), expected)
  fs::dir_create(expected)
  expect_identical(checked_preview_output_dir(state, pkg), expected)
})
