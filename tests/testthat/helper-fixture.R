make_package <- function(path = tempfile("package ")) {
  dir.create(path, recursive = TRUE)
  dir.create(file.path(path, "R"))
  writeLines(
    c(
      "Package: fixture",
      "Title: Fixture Package",
      "Version: 0.0.0.9000",
      "Authors@R: person('A', 'B', email='a@example.com', role=c('aut','cre'))",
      "Description: A fixture package used for focused tests.",
      "License: MIT",
      "Encoding: UTF-8",
      "URL: https://example.test"
    ),
    file.path(path, "DESCRIPTION")
  )
  writeLines("export(hello)", file.path(path, "NAMESPACE"))
  writeLines("hello <- function() 'hello'", file.path(path, "R", "hello.R"))
  normalizePath(path, winslash = "/")
}

wait_until <- function(predicate, timeout = 20) {
  deadline <- Sys.time() + timeout
  while (!predicate() && Sys.time() < deadline) {
    later::run_now(.1)
    httpuv::service(50)
  }
  predicate()
}

service_for <- function(seconds) {
  deadline <- Sys.time() + seconds
  while (Sys.time() < deadline) {
    later::run_now(.05)
    httpuv::service(20)
  }
}

fetch_preview <- function(url, timeout = 10) {
  job <- callr::r_bg(
    function(url) {
      tryCatch(
        {
          connection <- base::url(url, open = "rb")
          on.exit(close(connection), add = TRUE)
          list(ok = TRUE, body = readBin(connection, "raw", n = 1e7))
        },
        error = function(e) list(ok = FALSE, error = conditionMessage(e))
      )
    },
    args = list(url = url),
    supervise = TRUE
  )
  deadline <- Sys.time() + timeout
  while (job$is_alive() && Sys.time() < deadline) {
    httpuv::service(20)
    later::run_now(0)
  }
  if (job$is_alive()) {
    job$kill()
    testthat::fail(sprintf("Timed out fetching %s", url))
  }
  job$get_result()
}
