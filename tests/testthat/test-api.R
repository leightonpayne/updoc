test_that("exported functions validate package paths and documented inputs", {
  not_package <- tempfile("not-a-package-")
  dir.create(not_package)
  malformed <- tempfile("malformed-package-")
  dir.create(malformed)
  root <- make_package()
  on.exit(unlink(c(not_package, malformed, root), recursive = TRUE), add = TRUE)
  writeLines(
    "Description: missing Package field",
    file.path(malformed, "DESCRIPTION")
  )

  expect_error(preview(not_package, browse = FALSE), "Not an R package")
  expect_error(
    preview(malformed, browse = FALSE),
    "Invalid package DESCRIPTION"
  )
  expect_error(
    preview(tempfile("missing-package-"), browse = FALSE),
    "does not exist"
  )

  expect_error(preview(root, strategy = "fast", browse = FALSE), "arg")
  expect_error(preview(root, port = -1, browse = FALSE), "0 through 65535")
  expect_error(preview(root, port = 65536, browse = FALSE), "0 through 65535")
  expect_error(preview(root, document = NA, browse = FALSE), "document")
  expect_error(preview(root, browse = 1), "browse")
  expect_error(preview(root, quiet = NA, browse = FALSE), "TRUE.*FALSE")
  expect_error(preview(root, theme = "sepia", browse = FALSE), "arg")
  expect_error(serve(root, port = -1, browse = FALSE), "0 through 65535")
  expect_error(
    preview(root, pandoc = NA_character_, browse = FALSE),
    "one non-empty directory path"
  )
  expect_error(
    serve(root, pandoc = "", browse = FALSE),
    "one non-empty directory path"
  )

  expect_error(build(root, document = NA), "TRUE.*FALSE")
  expect_error(build(root, lazy = NA), "TRUE.*FALSE")
  expect_error(build(root, quiet = NA), "TRUE.*FALSE")
  expect_error(build(root, destination = ""), "non-empty")
  expect_error(
    build(root, pandoc = NA_character_),
    "one non-empty directory path"
  )

  expect_error(check(root, document = NA), "TRUE.*FALSE")
  expect_error(check(root, quiet = NA), "TRUE.*FALSE")
  expect_error(check(root, error = NA), "TRUE.*FALSE")
  expect_error(check(root, format = "xml"), "arg")
  expect_error(check(root, pandoc = ""), "one non-empty directory path")

  expect_error(preview_wait(root, timeout = -1), "non-negative")
  expect_error(preview_wait(root, after = -1), "non-negative integer")
})

test_that("preview validates global Pandoc and theme options", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  old_pandoc <- options(updoc.pandoc = NA_character_)
  on.exit(options(old_pandoc), add = TRUE)
  expect_error(preview(root, browse = FALSE), "one non-empty directory path")
  options(old_pandoc)

  old_theme <- options(updoc.theme = "sepia")
  on.exit(options(old_theme), add = TRUE)
  expect_error(preview(root, browse = FALSE), "arg")
})

test_that("preview state transitions enforce lifecycle invariants", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, "docs", FALSE, "smart", 4321L, 1, TRUE)

  # Valid full lifecycle passes all transition checks.
  state_begin_build(state, file.path(root, "README.md"))
  state_set_build_job(state, list(id = 1L), "README.md", initial = FALSE)
  expect_length(preview_state_problems(state), 0L)
  state_clear_build_job(state)
  state_publish_site(state, root)
  state_complete_build(state, reload = TRUE)
  state_end_build(state)
  expect_length(preview_state_problems(state), 0L)

  # Registering a worker job while not building violates state invariants.
  expect_error(
    state_set_build_job(state, list(id = 2L), "README.md", initial = FALSE),
    "worker job exists while no build is active"
  )
  state_clear_build_job(state)

  # Ending a build while a worker job is still registered violates invariants.
  state_begin_build(state)
  state_set_build_job(state, list(id = 3L), "README.md", initial = FALSE)
  expect_error(
    state_end_build(state),
    "worker job exists while no build is active"
  )
  state_clear_build_job(state)

  # Worker states may hold supersede_path without a parent job; parent states may not.
  state$supersede_path <- tempfile("supersede-")
  expect_true(any(grepl(
    "job metadata exists without a worker job",
    preview_state_problems(state),
    fixed = TRUE
  )))
  state$worker <- TRUE
  expect_length(preview_state_problems(state), 0L)
  state$worker <- FALSE
  state$supersede_path <- NULL

  # Corrupted counters or orphaned job metadata are flagged.
  state$rebuild_count <- -1L
  state$job_initial <- TRUE
  problems <- preview_state_problems(state)
  expect_true(any(grepl("non-negative integers", problems, fixed = TRUE)))
  expect_true(any(grepl(
    "job metadata exists without a worker job",
    problems,
    fixed = TRUE
  )))
})
