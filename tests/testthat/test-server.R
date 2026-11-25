test_that("preview server rejects traversal, untrusted rebuilds, and unsafe paths", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  site <- file.path(root, "docs")
  dir.create(site)
  writeLines("site", file.path(site, "index.html"))
  state <- new_preview_state(root, site, FALSE, "smart", 4321, 1, TRUE)
  state$site_dir <- site
  state$active <- TRUE
  state_begin_build(state)
  app <- preview_app(state)

  for (path in c("/missing.css", "/search.json", "/figure.png",
                 "/__updoc__/missing.html", "/../private.html",
                 "/%2e%2e/DESCRIPTION", "/%2e%2e/private.html",
                 "/..%5cprivate.html")) {
    response <- app$call(list(PATH_INFO = path, REQUEST_METHOD = "GET"))
    expect_identical(response$status, 404L, info = path)
    expect_identical(response$headers[["Content-Type"]], "text/plain; charset=utf-8")
    expect_identical(response$body, "Not found")
  }

  untrusted <- app$call(list(
    REQUEST_METHOD = "POST", PATH_INFO = "/__updoc__/rebuild"
  ))
  wrong_method <- app$call(list(
    REQUEST_METHOD = "GET", PATH_INFO = "/__updoc__/rebuild"
  ))
  expect_identical(untrusted$status, 403L)
  expect_identical(wrong_method$status, 405L)
  state_end_build(state)
})

test_that("missing documentation keeps live recovery through initial and later builds", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  site <- file.path(root, "published")
  dir.create(site)
  state <- new_preview_state(root, site, FALSE, "smart", 4321, 1, TRUE)
  state$site_dir <- site
  state$active <- TRUE
  app <- preview_app(state)
  request <- function(path, method = "GET") {
    app$call(list(PATH_INFO = path, REQUEST_METHOD = method))
  }

  state_begin_build(state)
  for (path in c("/articles", "/articles%2f", "//articles/", "/reference")) {
    redirect <- request(path)
    expect_identical(redirect$status, 308L)
    expected <- if (path == "/reference") "/reference/" else "/articles/"
    expect_identical(redirect$headers$Location, expected)
    expect_match(request(expected)$body, "window.__updocPreviewBootstrap=",
                 fixed = TRUE)
  }
  for (path in c("/articles/guide.html", "/reference/hello.html", "/articles/")) {
    response <- request(path)
    expect_identical(response$status, 404L)
    expect_identical(response$headers[["Content-Type"]], "text/html; charset=utf-8")
    expect_identical(response$headers[["Cache-Control"]], "no-store")
    expect_match(response$body, "window.__updocPreviewBootstrap=", fixed = TRUE)
    expect_match(response$body, '"phase":"building"', fixed = TRUE)
    expect_length(request(path, "HEAD")$body, 0L)
  }

  state_end_build(state)
  state_complete_build(state, reload = TRUE)
  missing <- request("/articles/guide.html")
  expect_identical(missing$status, 404L)
  expect_match(missing$body, "Page unavailable", fixed = TRUE)
  expect_match(missing$body, "window.__updocPreviewBootstrap=", fixed = TRUE)
  expect_match(missing$body, 'href="/"', fixed = TRUE)

  dir.create(file.path(site, "articles"))
  article <- file.path(site, "articles", "guide.html")
  writeLines("<html><body>Restored guide</body></html>", article)
  state_complete_build(state, reload = TRUE, reload_paths = "articles/guide.html")
  restored <- request("/articles/guide.html")
  expect_identical(restored$status, 200L)
  expect_match(restored$body, "Restored guide", fixed = TRUE)
  expect_match(restored$body, '"paths":["articles/guide.html"]', fixed = TRUE)
  unlink(article)
  expect_match(request("/articles/guide.html")$body,
               "window.__updocPreviewBootstrap=", fixed = TRUE)
})

test_that("directory indexes redirect to a usable relative-link base", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  dir.create(file.path(root, "articles"))
  writeLines('<html><body><a href="guide.html">Guide</a></body></html>',
             file.path(root, "articles", "index.html"))
  state <- new_preview_state(root, root, FALSE, "smart", 4321, 1, TRUE)
  state$site_dir <- root
  state$active <- TRUE
  app <- preview_app(state)
  for (path in c("/articles", "//articles", "/articles%2f")) {
    response <- app$call(list(PATH_INFO = path, REQUEST_METHOD = "GET",
                              QUERY_STRING = "filter=one%20two"))
    expect_identical(response$status, 308L)
    expect_identical(response$headers$Location, "/articles/?filter=one%20two")
    expect_length(response$body, 0L)
  }
  for (path in c("/articles/", "/articles/index.html")) {
    expect_identical(app$call(list(PATH_INFO = path))$status, 200L)
  }
})

test_that("reload details describe committed output, not a later worker", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, "docs", FALSE, "smart", 4321, 1, TRUE)
  state$active <- TRUE
  paths <- c("articles/one.html", "articles/old.html", "articles/one.html")
  state_complete_build(state, reload = TRUE, reload_paths = paths)
  committed <- jsonlite::fromJSON(preview_status_json(state))
  expect_identical(committed$version, 1L)
  expect_identical(committed$reload$full, FALSE)
  expect_identical(committed$reload$paths,
                   c("articles/one.html", "articles/old.html"))

  state_begin_build(state)
  result <- list(
    success = TRUE, snapshot = NULL, last_changed = "vignettes/two.Rmd",
    last_duration = 1, last_warnings = character(), indexes_dirty = FALSE,
    publish_delta = list(write = c("articles/two.html" = "new-hash"),
                         remove = "articles/unpublished.html"),
    reload_count = 1L
  )
  expect_true(apply_worker_result(state, result))
  in_flight <- jsonlite::fromJSON(preview_status_json(state))
  expect_identical(in_flight$phase, "building")
  expect_identical(in_flight$version, committed$version)
  expect_identical(in_flight$reload, committed$reload)
  result$success <- FALSE
  result$last_error <- build_error_record(simpleError("Worker failed"), root)
  expect_false(apply_worker_result(state, result))
  state_end_build(state)
  failed <- jsonlite::fromJSON(preview_status_json(state))
  expect_identical(failed$phase, "error")
  expect_identical(failed$version, committed$version)
  expect_identical(failed$reload, committed$reload)

  state_complete_build(state, reload = TRUE)
  full <- jsonlite::fromJSON(preview_status_json(state))
  expect_identical(full$version, 2L)
  expect_true(full$reload$full)
  expect_length(full$reload$paths, 0L)
  state_complete_build(state, reload = TRUE, reload_paths = character())
  empty <- jsonlite::fromJSON(preview_status_json(state))
  expect_false(empty$reload$full)
  expect_length(empty$reload$paths, 0L)
  state$reload_version <- state$reload_version + 1L
  expect_true(jsonlite::fromJSON(preview_status_json(state))$reload$full)
})

test_that("publication reports written and removed outputs only after success", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, "docs", FALSE, "smart", 4321, 1, TRUE)
  state$active <- TRUE
  state$preview_dir <- file.path(root, "preview")
  state$build_dir <- file.path(root, "work")
  state$site_dir <- file.path(state$preview_dir, "published")
  dir.create(state$build_dir)
  dir.create(state$site_dir, recursive = TRUE)
  writeLines("New article", file.path(state$build_dir, "article.html"))
  writeLines("Old article", file.path(state$site_dir, "article.html"))
  writeLines("Removed article", file.path(state$site_dir, "removed.html"))
  write <- stats::setNames(unname(tools::md5sum(
    file.path(state$build_dir, "article.html")
  )), "article.html")
  result <- list(
    success = TRUE, snapshot = NULL, last_changed = "vignettes/article.Rmd",
    last_success = Sys.time(), last_duration = 1, last_warnings = character(),
    indexes_dirty = FALSE, reload_count = 1L,
    publish_delta = list(write = write, remove = "removed.html")
  )
  state$job <- list(data = result)
  state_begin_build(state)
  expect_true(suppressMessages(check_build_job(state)))
  committed <- jsonlite::fromJSON(preview_status_json(state))
  expect_false(committed$reload$full)
  expect_identical(committed$reload$paths, c("article.html", "removed.html"))
  expect_identical(readLines(file.path(state$site_dir, "article.html")),
                   "New article")
  expect_false(file.exists(file.path(state$site_dir, "removed.html")))

  result$publish_delta <- list(write = c("missing.html" = "missing-hash"),
                               remove = character())
  state$job <- list(data = result)
  state_begin_build(state)
  expect_false(suppressMessages(check_build_job(state)))
  failed <- jsonlite::fromJSON(preview_status_json(state))
  expect_identical(failed$version, committed$version)
  expect_identical(failed$reload, committed$reload)
  expect_identical(readLines(file.path(state$site_dir, "article.html")),
                   "New article")

  result$publish_delta <- list(write = write, remove = character())
  state$job <- list(data = result)
  state_begin_build(state)
  expect_true(suppressMessages(check_build_job(state)))
  expect_true(jsonlite::fromJSON(preview_status_json(state))$reload$full)
  state$job <- list(data = result)
  state$job_initial <- TRUE
  state_begin_build(state)
  expect_true(suppressMessages(check_build_job(state)))
  expect_true(jsonlite::fromJSON(preview_status_json(state))$reload$full)
})

test_that("preview HTML hydrates status and styles without more requests", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, "docs", FALSE, "smart", 4321, 1, TRUE,
                             theme = "dark")
  state$active <- TRUE
  hostile <- paste0("</script><script>alert('diagnostic')</script>",
                    "\u2028\u2029", "\\1")
  state$last_changed <- hostile
  html <- inject_preview_html(
    "<!doctype html><html><head><title>Demo</title></head><body>Page</body></html>",
    state$theme, state
  )
  marker <- "<script>window.__updocPreviewBootstrap="
  start <- regexpr(marker, html, fixed = TRUE)[[1L]] + nchar(marker)
  tail <- substring(html, start)
  end <- regexpr(";</script>", tail, fixed = TRUE)[[1L]]
  payload <- substr(tail, 1L, end - 1L)
  bootstrap <- jsonlite::fromJSON(payload)
  expect_identical(bootstrap$status$changed, hostile)
  expect_identical(bootstrap$css, preview_panel_css())
  expect_false(grepl("<", payload, fixed = TRUE))
  expect_false(grepl("\u2028", payload, fixed = TRUE))
  expect_false(grepl("\u2029", payload, fixed = TRUE))
  expect_match(payload, "\\u003c", fixed = TRUE)
  expect_match(html, 'data-updoc-theme="dark"', fixed = TRUE)
  expect_false(grepl('<script defer src="/__updoc__/', html, fixed = TRUE))
  expect_match(html, paste0("<body>", marker), fixed = TRUE)
  expect_true(endsWith(html, "</script>Page</body></html>"))
  expect_identical(lengths(regmatches(html, gregexpr("</script>", html,
                                                    fixed = TRUE))), 5L)
  expect_match(html, preview_asset_text("doc.js", "mascot"), fixed = TRUE)
  expect_lt(regexpr(preview_asset_text("doc.js", "mascot"), html,
                   fixed = TRUE)[[1L]],
            regexpr(preview_asset_text("panel-primitives.js"), html,
                    fixed = TRUE)[[1L]])
  for (asset in c("panel-primitives.js", "panel.js", "reload.js")) {
    expect_match(html, preview_asset_text(asset), fixed = TRUE)
  }
  expect_true(startsWith(inject_preview_html("Page", state = state), "Page"))
  fallback <- inject_preview_html("<head></head><body>Page</body>")
  expect_match(fallback, '<script defer src="/__updoc__/reload.js">', fixed = TRUE)
  expect_lt(regexpr('/__updoc__/doc.js', fallback, fixed = TRUE)[[1L]],
            regexpr('/__updoc__/panel-primitives.js', fallback,
                    fixed = TRUE)[[1L]])

  app <- preview_app(state)
  asset <- app$call(list(PATH_INFO = "/__updoc__/doc.js", REQUEST_METHOD = "GET"))
  expect_identical(asset$status, 200L)
  expect_identical(gsub("\r\n", "\n", rawToChar(asset$body), fixed = TRUE), paste0(
    preview_asset_text("doc.js", "mascot"), "\n"
  ))
  css <- app$call(list(PATH_INFO = "/__updoc__/panel.css", REQUEST_METHOD = "GET"))
  expect_identical(rawToChar(css$body), preview_panel_css())
  head <- app$call(list(PATH_INFO = "/__updoc__/panel.css", REQUEST_METHOD = "HEAD"))
  expect_length(head$body, 0L)

  opening <- '<BoDy class="demo" data-note="x > y" data-pattern="\\1">'
  prefix <- paste0("<!doctype html><html><head></head>", opening)
  content <- '<script src="blocking.js"></script><main>Page</main></BoDy></html>'
  attributed <- inject_preview_html(paste0(prefix, content), state = state)
  expect_true(startsWith(attributed, paste0(prefix, marker)))
  expect_true(endsWith(attributed, paste0("</script>", content)))
})

test_that("served documents and initial placeholders include bootstrap state", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  site <- file.path(root, "published")
  dir.create(site)
  state <- new_preview_state(root, site, FALSE, "smart", 4321, 1, TRUE)
  state$site_dir <- site
  state$active <- TRUE
  state_begin_build(state)
  app <- preview_app(state)
  placeholder <- app$call(list(PATH_INFO = "/", REQUEST_METHOD = "GET"))
  expect_identical(placeholder$status, 200L)
  expect_match(placeholder$body,
               "<body><script>window.__updocPreviewBootstrap=", fixed = TRUE)
  expect_match(placeholder$body, '"phase":"building"', fixed = TRUE)
  writeLines("<html><head></head><body>Published page</body></html>",
             file.path(site, "index.html"))
  state_end_build(state)
  state_complete_build(state, reload = TRUE)
  published <- app$call(list(PATH_INFO = "/", REQUEST_METHOD = "GET"))
  expect_identical(published$status, 200L)
  expect_match(published$body, '"version":1,"reload":{"full":true,"paths":[]}',
               fixed = TRUE)
  expect_match(published$body,
               "<body><script>window.__updocPreviewBootstrap=", fixed = TRUE)
  expect_true(endsWith(published$body, "</script>Published page</body></html>"))
})

test_that("status JSON, log endpoint, and rebuild full flag work cleanly", {
  root <- make_package()
  log_file <- tempfile("updoc-log-", fileext = ".log")
  on.exit(unlink(c(root, log_file), recursive = TRUE), add = TRUE)

  state <- new_preview_state(root, root, FALSE, "smart", 0L, 1, TRUE)
  state$site_dir <- root
  state$active <- TRUE
  state$url <- "http://127.0.0.1:4321"
  state$rebuild_count <- 2L
  state$last_success <- as.POSIXct("2026-11-25 12:00:00", tz = "UTC")
  app <- preview_app(state)

  missing_log <- app$call(list(PATH_INFO = "/__updoc__/log", REQUEST_METHOD = "GET"))
  expect_identical(missing_log$status, 404L)
  expect_identical(missing_log$headers[["Cache-Control"]], "no-store")
  missing_head <- app$call(list(PATH_INFO = "/__updoc__/log", REQUEST_METHOD = "HEAD"))
  expect_identical(missing_head$status, 404L)
  expect_length(missing_head$body, 0L)

  writeLines(c("line 1", "line 2"), log_file)
  state$build_log <- log_file

  status_resp <- app$call(list(PATH_INFO = "/__updoc__/status", REQUEST_METHOD = "GET"))
  expect_identical(status_resp$status, 200L)
  payload <- jsonlite::fromJSON(status_resp$body)
  expect_identical(payload$phase, "ready")
  expect_identical(payload$url, "http://127.0.0.1:4321")
  expect_identical(payload$rebuild_count, 2L)
  expect_identical(payload$last_success, "2026-11-25T12:00:00Z")
  expect_identical(payload$build_log, log_file)

  log_resp <- app$call(list(PATH_INFO = "/__updoc__/log", REQUEST_METHOD = "GET"))
  expect_identical(log_resp$status, 200L)
  expect_identical(log_resp$headers[["Content-Type"]], "text/plain; charset=utf-8")
  expect_match(rawToChar(log_resp$body), "line 1", fixed = TRUE)
  expect_match(rawToChar(log_resp$body), "line 2", fixed = TRUE)

  log_head <- app$call(list(PATH_INFO = "/__updoc__/log", REQUEST_METHOD = "HEAD"))
  expect_identical(log_head$status, 200L)
  expect_length(log_head$body, 0L)

  expect_true(query_flag("?full=true", "full"))
  expect_true(query_flag("full=1&wait=yes", "full"))
  expect_true(query_flag("wait", "wait"))
  expect_false(query_flag("full=false", "full"))
  expect_false(query_flag("full=0", "full"))
  expect_identical(query_number("timeout=999", "timeout", 30, 0, 300), 300)
  expect_identical(query_number("timeout=-5", "timeout", 30, 0, 300), 0)
  expect_identical(query_number("timeout=bad", "timeout", 30, 0, 300), 30)
  expect_identical(query_integer("after=4", "after"), 4L)
  expect_null(query_integer("after=-1", "after"))
})

test_that("POST /__updoc__/rebuild forwards the full query flag", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, root, FALSE, "smart", 4321L, 1, TRUE)
  state$site_dir <- root
  state$active <- TRUE
  app <- preview_app(state)

  captured <- list()
  local_mocked_bindings(
    request_rebuild = function(state, changed, force_full = FALSE, ...) {
      captured[[length(captured) + 1L]] <<- list(
        changed = changed, force_full = force_full
      )
      invisible(TRUE)
    }
  )

  r1 <- app$call(list(
    PATH_INFO = "/__updoc__/rebuild",
    REQUEST_METHOD = "POST",
    HTTP_X_UPDOC_REQUEST = "rebuild",
    QUERY_STRING = ""
  ))
  r2 <- app$call(list(
    PATH_INFO = "/__updoc__/rebuild",
    REQUEST_METHOD = "POST",
    HTTP_X_UPDOC_REQUEST = "rebuild",
    QUERY_STRING = "?full=true"
  ))
  expect_identical(r1$status, 202L)
  expect_identical(r2$status, 202L)
  expect_false(captured[[1L]]$force_full)
  expect_true(captured[[2L]]$force_full)
})

test_that("GET /__updoc__/status?wait=1 handles settled, async, and timeout states", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, root, FALSE, "smart", 4321L, 1, TRUE)
  state$site_dir <- root
  state$active <- TRUE
  app <- preview_app(state)

  settled <- app$call(list(
    PATH_INFO = "/__updoc__/status",
    REQUEST_METHOD = "GET",
    QUERY_STRING = "?wait=1&timeout=5"
  ))
  expect_true(is.list(settled))
  expect_identical(settled$status, 200L)
  expect_identical(jsonlite::fromJSON(settled$body)$phase, "ready")

  state_begin_build(state)
  pending <- app$call(list(
    PATH_INFO = "/__updoc__/status",
    REQUEST_METHOD = "GET",
    QUERY_STRING = "?wait=1&timeout=5"
  ))
  expect_true(promises::is.promise(pending))

  resolved <- NULL
  promises::then(pending, function(value) { resolved <<- value })
  later::later(function() {
    state_end_build(state)
    state_complete_build(state, reload = TRUE)
  }, delay = 0.08)

  expect_true(wait_until(function() !is.null(resolved), timeout = 5))
  expect_identical(resolved$status, 200L)
  body <- jsonlite::fromJSON(resolved$body)
  expect_identical(body$phase, "ready")
  expect_identical(body$rebuild_count, 1L)

  state_begin_build(state)
  timed_out <- app$call(list(
    PATH_INFO = "/__updoc__/status",
    REQUEST_METHOD = "GET",
    QUERY_STRING = "?wait=1&timeout=0.1"
  ))
  expect_true(promises::is.promise(timed_out))
  resolved_timeout <- NULL
  promises::then(timed_out, function(value) { resolved_timeout <<- value })
  expect_true(wait_until(function() !is.null(resolved_timeout), timeout = 5))
  expect_identical(jsonlite::fromJSON(resolved_timeout$body)$phase, "building")
  state_end_build(state)
})

test_that("port = 0 binds an available ephemeral local port", {
  root <- make_package()
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  state <- new_preview_state(root, root, FALSE, "smart", 0L, 1, TRUE)
  state$site_dir <- root
  on.exit(stop_state(state), add = TRUE, after = FALSE)

  start_preview_server(state)
  expect_true(state$active)
  expect_true(is.integer(state$port) && state$port >= 1L && state$port <= 65535L)
  expect_identical(state$url, sprintf("http://127.0.0.1:%d", state$port))
})
