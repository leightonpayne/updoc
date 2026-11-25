browser_suites <- list(
  list(file = "panel.test.cjs", assets = "preview",
       label = "preview controls support recovery, retry feedback and keyboard focus"),
  list(file = "reload.test.cjs", assets = c("preview", "mascot"),
       label = "browser refresh preserves popup state and reloads relevant output"),
  list(file = "doc.test.cjs", assets = "mascot",
       label = "Doc preserves expressions, respects motion preferences and cleans up"),
  list(file = "doc-model.test.cjs", assets = "mascot",
       label = "Doc exports share poses, palettes and repeatable motion settings")
)

for (suite in browser_suites) {
  test_that(suite$label, {
    node <- unname(Sys.which("node"))
    skip_if(!nzchar(node), "Node.js is needed for the browser regression checks")
    version <- tryCatch(
      suppressWarnings(system2(node, "--version", stdout = TRUE, stderr = TRUE,
                               timeout = 5)),
      error = function(e) character()
    )
    major <- suppressWarnings(as.integer(sub(
      "^v([0-9]+).*", "\\1", version[1L]
    )))
    skip_if(!is.null(attr(version, "status")) || length(major) != 1L ||
              is.na(major) || major < 18L,
            "Node.js >= 18 is needed for the browser regression checks")
    script <- test_path("..", "browser", suite$file)
    assets <- vapply(suite$assets, system.file, character(1), package = "updoc")
    expect_true(all(nzchar(assets)))
    output <- system2(node, c(shQuote(script), shQuote(unname(assets))),
                      stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    expect_identical(status, 0L, info = paste(output, collapse = "\n"))
  })
}
