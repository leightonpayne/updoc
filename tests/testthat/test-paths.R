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
    writeLines(
      c(
        "template:",
        "  bootstrap: 5",
        "development:",
        "  mode: devel",
        paste0("  destination: ", development)
      ),
      file.path(root, "_pkgdown.yml")
    )
    expect_error(
      run_pkgdown(state, full),
      "temporary work directory",
      fixed = TRUE
    )
    expect_error(
      run_pkgdown(state, section),
      "temporary work directory",
      fixed = TRUE
    )
    state$indexes_dirty <- TRUE
    state$pkgdown <- NULL
    expect_error(
      refresh_shared_indexes(state),
      "temporary work directory",
      fixed = TRUE
    )
    expect_identical(unname(tools::md5sum(sentinel)), original)
    expect_identical(list.files(external), "keep.txt")
  }

  # Cached metadata must pass the same guard before shared outputs are written.
  state$pkgdown <- pkgdown::as_pkgdown(
    root,
    override = list(
      development = list(mode = "release"),
      destination = external
    )
  )
  expect_error(
    refresh_shared_indexes(state),
    "temporary work directory",
    fixed = TRUE
  )
  expect_identical(unname(tools::md5sum(sentinel)), original)
})

test_that("preview output rejects symlink roots and ancestors", {
  root <- make_package()
  work <- tempfile("preview-output-links-")
  destination <- file.path(work, "output")
  on.exit(
    {
      if (fs::is_link(destination))
        try(fs::link_delete(destination), silent = TRUE)
      unlink(c(root, work), recursive = TRUE)
    },
    add = TRUE
  )
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
    writeLines(
      c(
        "template:",
        "  bootstrap: 5",
        "development:",
        "  mode: devel",
        paste0("  destination: ", development)
      ),
      file.path(root, "_pkgdown.yml")
    )
    expect_error(
      run_pkgdown(state, full),
      "temporary work directory",
      fixed = TRUE
    )
    expect_identical(unname(tools::md5sum(sentinel)), original)
    expect_identical(list.files(external), "keep.txt")
  }

  # The allowed work root itself may have been replaced by a symlink.
  unlink(link)
  fs::dir_delete(destination)
  expect_true(file.symlink(external, destination))
  writeLines(
    c("development:", "  mode: release"),
    file.path(root, "_pkgdown.yml")
  )
  expect_error(
    run_pkgdown(state, full),
    "temporary work directory",
    fixed = TRUE
  )
  expect_identical(unname(tools::md5sum(sentinel)), original)
  expect_identical(list.files(external), "keep.txt")
})

test_that("relative nested preview directories remain valid", {
  root <- make_package()
  work <- tempfile("preview-output-nested-")
  on.exit(unlink(c(root, work), recursive = TRUE), add = TRUE)
  state <- new_preview_state(
    root,
    file.path(work, "output"),
    FALSE,
    "smart",
    4321,
    .1,
    TRUE
  )
  writeLines(
    c(
      "development:",
      "  mode: devel",
      "  destination: preview/development"
    ),
    file.path(root, "_pkgdown.yml")
  )
  pkg <- pkgdown::as_pkgdown(
    root,
    override = list(
      destination = state$destination
    )
  )
  expected <- as.character(fs::path_norm(fs::path(
    state$destination,
    "preview",
    "development"
  )))
  expect_identical(checked_preview_output_dir(state, pkg), expected)
  fs::dir_create(expected)
  expect_identical(checked_preview_output_dir(state, pkg), expected)
})

test_that("mirroring and syncing handle chained, broken, and cyclic symlinks safely", {
  root <- make_package()
  mirror <- tempfile("updoc-mirror-symlinks-")
  external <- tempfile("updoc-symlink-external-")
  on.exit(unlink(c(root, mirror, external), recursive = TRUE), add = TRUE)

  dir.create(file.path(external, "target_dir", "nested"), recursive = TRUE)
  ext_file <- file.path(external, "target.txt")
  ext_nested <- file.path(external, "target_dir", "nested", "data.txt")
  writeLines("chained file content", ext_file)
  writeLines("chained dir content", ext_nested)
  old_time <- as.POSIXct(Sys.time() - 300, tz = "UTC")
  Sys.setFileTime(ext_file, old_time)
  Sys.setFileTime(ext_nested, old_time)

  extdata <- file.path(root, "inst", "extdata")
  dir.create(file.path(extdata, "sub"), recursive = TRUE)
  dir.create(file.path(extdata, "cycle_a"), recursive = TRUE)
  dir.create(file.path(extdata, "cycle_b"), recursive = TRUE)
  writeLines("in cycle a", file.path(extdata, "cycle_a", "a.txt"))
  writeLines("in cycle b", file.path(extdata, "cycle_b", "b.txt"))

  # Chained file symlink: file_chain2 -> file_chain1 -> ext_file
  ok <- suppressWarnings(file.symlink(
    ext_file,
    file.path(extdata, "file_chain1")
  ))
  skip_if_not(ok, "Creating symlinks is unavailable")
  expect_true(file.symlink("file_chain1", file.path(extdata, "file_chain2")))

  # Chained directory symlink: dir_chain2 -> dir_chain1 -> external/target_dir
  expect_true(file.symlink(
    file.path(external, "target_dir"),
    file.path(extdata, "dir_chain1")
  ))
  expect_true(file.symlink("dir_chain1", file.path(extdata, "dir_chain2")))

  # Broken symlinks (single and chained)
  expect_true(file.symlink(
    file.path(external, "does-not-exist"),
    file.path(extdata, "broken1")
  ))
  expect_true(file.symlink("broken1", file.path(extdata, "broken2")))

  # Direct and mutual symlink loops
  expect_true(file.symlink("self_loop", file.path(extdata, "self_loop")))
  expect_true(file.symlink("loop_b", file.path(extdata, "loop_a")))
  expect_true(file.symlink("loop_a", file.path(extdata, "loop_b")))

  # Directory cycles (parent, root, and mutual)
  expect_true(file.symlink("..", file.path(extdata, "sub", "to_parent")))
  expect_true(file.symlink(root, file.path(extdata, "sub", "to_root")))
  expect_true(file.symlink("../cycle_b", file.path(extdata, "cycle_a", "to_b")))
  expect_true(file.symlink("../cycle_a", file.path(extdata, "cycle_b", "to_a")))

  mirror_package(root, mirror)

  copied_file <- file.path(mirror, "inst", "extdata", "file_chain2")
  copied_dir_file <- file.path(
    mirror,
    "inst",
    "extdata",
    "dir_chain2",
    "nested",
    "data.txt"
  )
  expect_true(file.exists(copied_file))
  expect_false(fs::is_link(copied_file))
  expect_identical(readLines(copied_file), "chained file content")
  expect_true(
    abs(
      as.numeric(file.info(copied_file)$mtime) -
        as.numeric(file.info(ext_file)$mtime)
    ) <
      2
  )

  expect_true(file.exists(copied_dir_file))
  expect_identical(readLines(copied_dir_file), "chained dir content")
  expect_true(
    abs(
      as.numeric(file.info(copied_dir_file)$mtime) -
        as.numeric(file.info(ext_nested)$mtime)
    ) <
      2
  )

  # Broken symlinks and loops are skipped without failing or hanging.
  expect_false(file.exists(file.path(mirror, "inst", "extdata", "broken1")))
  expect_false(file.exists(file.path(mirror, "inst", "extdata", "broken2")))
  expect_false(file.exists(file.path(mirror, "inst", "extdata", "self_loop")))
  expect_false(file.exists(file.path(mirror, "inst", "extdata", "loop_a")))
  expect_false(dir.exists(file.path(
    mirror,
    "inst",
    "extdata",
    "sub",
    "to_parent"
  )))
  expect_false(dir.exists(file.path(
    mirror,
    "inst",
    "extdata",
    "sub",
    "to_root"
  )))
  expect_true(file.exists(file.path(
    mirror,
    "inst",
    "extdata",
    "cycle_a",
    "a.txt"
  )))
  expect_true(file.exists(file.path(
    mirror,
    "inst",
    "extdata",
    "cycle_b",
    "b.txt"
  )))
  expect_false(dir.exists(
    file.path(mirror, "inst", "extdata", "cycle_a", "to_b", "to_a")
  ))

  # sync_package_changes() also handles chained file/dir links and broken links.
  writeLines("updated chained content", ext_file)
  sync_package_changes(
    root,
    mirror,
    c(
      file.path(extdata, "file_chain2"),
      file.path(extdata, "dir_chain2"),
      file.path(extdata, "broken2"),
      file.path(extdata, "loop_a")
    )
  )
  expect_identical(readLines(copied_file), "updated chained content")

  unlink(ext_file)
  sync_package_changes(root, mirror, file.path(extdata, "file_chain2"))
  expect_false(file.exists(copied_file))
})

test_that("mirroring honors .Rbuildignore and .venv while preserving pkgdown sources", {
  root <- make_package()
  mirror <- tempfile("updoc-mirror-ignore-")
  on.exit(unlink(c(root, mirror), recursive = TRUE), add = TRUE)

  writeLines(
    c(
      "^_pkgdown\\.yml$",
      "^_pkgdown\\.yaml$",
      "^pkgdown$",
      "^vignettes/articles$",
      "^README\\.Rmd$",
      "^README\\.md$",
      "^index\\.md$",
      "^NEWS\\.md$",
      "^CONTRIBUTING\\.md$",
      "^LICENSE\\.md$",
      "^CODE_OF_CONDUCT\\.md$",
      "^SUPPORT\\.md$",
      "^\\.github$",
      "^src/.*\\.so$",
      "^benchmarks$",
      "^codebase-docs$",
      "^vendor($|/)",
      "^tools/vendor-build$",
      "^tools/slash-only/",
      "^cran-comments\\.md$",
      "[unclosed-invalid-regex"
    ),
    file.path(root, ".Rbuildignore")
  )

  # Pkgdown/docs/package files listed in .Rbuildignore must still be mirrored.
  writeLines("url: https://example.test", file.path(root, "_pkgdown.yml"))
  writeLines("title: fixture", file.path(root, "README.Rmd"))
  writeLines("# README", file.path(root, "README.md"))
  writeLines("# Home", file.path(root, "index.md"))
  writeLines("# fixture 0.1.0", file.path(root, "NEWS.md"))
  writeLines("How to contribute", file.path(root, "CONTRIBUTING.md"))
  writeLines("MIT License", file.path(root, "LICENSE.md"))
  writeLines("Be kind", file.path(root, "CODE_OF_CONDUCT.md"))
  writeLines("Get help", file.path(root, "SUPPORT.md"))
  writeLines("CRAN notes", file.path(root, "cran-comments.md"))
  dir.create(file.path(root, "pkgdown", "assets"), recursive = TRUE)
  writeLines("body {}", file.path(root, "pkgdown", "extra.css"))
  writeLines("asset", file.path(root, "pkgdown", "assets", "app.js"))
  dir.create(file.path(root, "vignettes", "articles"), recursive = TRUE)
  writeLines(
    "---\ntitle: Guide\n---",
    file.path(root, "vignettes", "articles", "guide.Rmd")
  )
  dir.create(file.path(root, "src"))
  writeLines("binary", file.path(root, "src", "fixture.so"))
  dir.create(file.path(root, ".github", "workflows"), recursive = TRUE)
  writeLines(
    "GitHub contributing",
    file.path(root, ".github", "CONTRIBUTING.md")
  )
  writeLines("name: ci", file.path(root, ".github", "workflows", "ci.yaml"))

  # Excluded repo junk and virtualenvs
  dir.create(file.path(root, "benchmarks", "data"), recursive = TRUE)
  writeLines("big", file.path(root, "benchmarks", "data", "run.csv"))
  dir.create(file.path(root, "codebase-docs", ".venv", "bin"), recursive = TRUE)
  writeLines("py", file.path(root, "codebase-docs", ".venv", "bin", "python"))
  dir.create(file.path(root, "vendor", "lib"), recursive = TRUE)
  writeLines("vendor", file.path(root, "vendor", "lib", "vendored.c"))
  dir.create(file.path(root, "tools", "vendor-build"), recursive = TRUE)
  writeLines("build", file.path(root, "tools", "vendor-build", "out.o"))
  dir.create(file.path(root, "tools", "slash-only"), recursive = TRUE)
  writeLines("ignored", file.path(root, "tools", "slash-only", "tmp.txt"))
  dir.create(file.path(root, "tools", "vendor"), recursive = TRUE)
  writeLines("keep", file.path(root, "tools", "vendor", "sources.tar.xz"))

  # Hard-excluded .venv / venv even without .Rbuildignore or under protected dirs
  dir.create(file.path(root, "scripts", ".venv", "bin"), recursive = TRUE)
  writeLines("py", file.path(root, "scripts", ".venv", "bin", "python"))
  dir.create(file.path(root, "scripts", "venv", "bin"), recursive = TRUE)
  writeLines("py", file.path(root, "scripts", "venv", "bin", "python"))
  writeLines("ok", file.path(root, "scripts", "run.R"))
  dir.create(file.path(root, "vignettes", ".venv", "bin"), recursive = TRUE)
  writeLines("py", file.path(root, "vignettes", ".venv", "bin", "python"))

  # Site output under a protected directory must still be hard-excluded.
  site_dir <- file.path(root, "inst", "site")
  dir.create(site_dir, recursive = TRUE)
  writeLines("<h1>site</h1>", file.path(site_dir, "index.html"))

  mirror_package(root, mirror, site_dir)

  # Protected pkgdown/package files are present.
  expect_true(file.exists(file.path(mirror, "_pkgdown.yml")))
  expect_true(file.exists(file.path(mirror, "README.Rmd")))
  expect_true(file.exists(file.path(mirror, "README.md")))
  expect_true(file.exists(file.path(mirror, "index.md")))
  expect_true(file.exists(file.path(mirror, "NEWS.md")))
  expect_true(file.exists(file.path(mirror, "CONTRIBUTING.md")))
  expect_true(file.exists(file.path(mirror, "LICENSE.md")))
  expect_true(file.exists(file.path(mirror, "CODE_OF_CONDUCT.md")))
  expect_true(file.exists(file.path(mirror, "SUPPORT.md")))
  expect_true(file.exists(file.path(mirror, "pkgdown", "extra.css")))
  expect_true(file.exists(file.path(mirror, "pkgdown", "assets", "app.js")))
  expect_true(file.exists(file.path(
    mirror,
    "vignettes",
    "articles",
    "guide.Rmd"
  )))
  expect_true(file.exists(file.path(mirror, "src", "fixture.so")))
  expect_true(file.exists(file.path(mirror, ".github", "CONTRIBUTING.md")))
  expect_true(file.exists(file.path(
    mirror,
    "tools",
    "vendor",
    "sources.tar.xz"
  )))
  expect_true(file.exists(file.path(mirror, "scripts", "run.R")))

  # Ignored repo junk, workflows, virtualenvs, and site_dir are excluded.
  expect_false(file.exists(file.path(mirror, "cran-comments.md")))
  expect_false(dir.exists(file.path(mirror, ".github", "workflows")))
  expect_false(dir.exists(file.path(mirror, "benchmarks")))
  expect_false(dir.exists(file.path(mirror, "codebase-docs")))
  expect_false(dir.exists(file.path(mirror, "vendor")))
  expect_false(dir.exists(file.path(mirror, "tools", "vendor-build")))
  expect_false(dir.exists(file.path(mirror, "tools", "slash-only")))
  expect_false(dir.exists(file.path(mirror, "scripts", ".venv")))
  expect_false(dir.exists(file.path(mirror, "scripts", "venv")))
  expect_false(dir.exists(file.path(mirror, "vignettes", ".venv")))
  expect_false(dir.exists(file.path(mirror, "inst", "site")))

  # Direct is_excluded_path() checks match mirroring behavior.
  expect_false(is_excluded_path(
    file.path(root, "_pkgdown.yml"),
    root,
    site_dir
  ))
  expect_false(is_excluded_path(
    file.path(root, "vignettes", "articles", "guide.Rmd"),
    root,
    site_dir
  ))
  expect_false(is_excluded_path(
    file.path(root, ".github", "CONTRIBUTING.md"),
    root,
    site_dir
  ))
  expect_true(is_excluded_path(
    file.path(root, ".github", "workflows", "ci.yaml"),
    root,
    site_dir
  ))
  expect_true(is_excluded_path(
    file.path(root, "benchmarks", "data", "run.csv"),
    root,
    site_dir
  ))
  expect_true(is_excluded_path(
    file.path(root, "scripts", ".venv", "bin", "python"),
    root,
    site_dir
  ))
  expect_true(is_excluded_path(
    file.path(site_dir, "index.html"),
    root,
    site_dir
  ))
})

test_that("mirroring and syncing preserve mtimes so compiled packages avoid spurious rebuilds", {
  skip_if_not_installed("pkgbuild")
  root <- make_package()
  mirror <- tempfile("updoc-mirror-mtime-")
  on.exit(unlink(c(root, mirror), recursive = TRUE), add = TRUE)

  dir.create(file.path(root, "src"), recursive = TRUE)
  dir.create(file.path(root, "inst", "include"), recursive = TRUE)
  src_before <- file.path(root, "src", "alpha.c")
  dll <- file.path(root, "src", paste0("fixture", .Platform$dynlib.ext))
  src_after <- file.path(root, "src", "zeta.c")
  src_header <- file.path(root, "src", "zeta.h")
  inst_header <- file.path(root, "inst", "include", "zeta.h")

  writeLines("int alpha(void) { return 1; }", src_before)
  writeLines("compiled shared library placeholder", dll)
  writeLines("int zeta(void) { return 2; }", src_after)
  writeLines("int zeta(void);", src_header)
  writeLines("int inst_zeta(void);", inst_header)

  now <- Sys.time()
  src_time <- now - 120
  dll_time <- now - 60
  for (path in c(src_before, src_after, src_header, inst_header)) {
    Sys.setFileTime(path, src_time)
  }
  Sys.setFileTime(dll, dll_time)

  expect_false(pkgbuild::needs_compile(root))

  mirror_package(root, mirror)

  mirror_dll <- file.path(mirror, "src", basename(dll))
  mirror_after <- file.path(mirror, "src", "zeta.c")
  mirror_inst_header <- file.path(mirror, "inst", "include", "zeta.h")
  expect_true(file.info(mirror_dll)$mtime > file.info(mirror_after)$mtime)
  expect_true(file.info(mirror_dll)$mtime > file.info(mirror_inst_header)$mtime)
  expect_false(pkgbuild::needs_compile(mirror))

  # Full reconciliation sync (as used on <initial>/<rebuild>) also preserves mtimes.
  snapshot <- source_snapshot(root)
  sync_paths <- package_sync_paths(root, mirror, snapshot)
  sync_package_changes(root, mirror, sync_paths)
  expect_false(pkgbuild::needs_compile(mirror))

  # Editing a C source file flips needs_compile() to TRUE in the mirror.
  writeLines("int zeta(void) { return 3; }", src_after)
  Sys.setFileTime(src_after, now)
  updated_snapshot <- source_snapshot(root)
  changed <- snapshot_changes(snapshot, updated_snapshot)
  expect_true(src_after %in% changed)
  sync_package_changes(root, mirror, changed)
  expect_true(pkgbuild::needs_compile(mirror))
})
