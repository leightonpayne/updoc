content_type <- function(path) {
  ext <- tolower(fs::path_ext(path))
  types <- c(
    html = "text/html; charset=utf-8", htm = "text/html; charset=utf-8",
    css = "text/css; charset=utf-8", js = "text/javascript; charset=utf-8",
    json = "application/json; charset=utf-8", svg = "image/svg+xml",
    png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
    gif = "image/gif", webp = "image/webp", ico = "image/x-icon",
    xml = "application/xml; charset=utf-8", txt = "text/plain; charset=utf-8",
    pdf = "application/pdf", woff = "font/woff", woff2 = "font/woff2"
  )
  if (ext %in% names(types)) unname(types[[ext]]) else
    "application/octet-stream"
}

http_response <- function(status = 200L, body = raw(), type = NULL,
                          headers = list()) {
  if (!is.null(type)) headers[["Content-Type"]] <- type
  headers[["X-Content-Type-Options"]] <- "nosniff"
  list(status = as.integer(status), headers = headers, body = body)
}

preview_asset_text <- function(name, directory = "preview") {
  file <- system.file(directory, name, package = "updoc", mustWork = TRUE)
  paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

preview_panel_css <- function() {
  paste(preview_asset_text("doc.css", "mascot"),
        preview_asset_text("panel.css"), sep = "\n")
}

escape_preview_script_json <- function(json) {
  json <- gsub("<", "\\u003c", json, fixed = TRUE)
  json <- gsub("\u2028", "\\u2028", json, fixed = TRUE)
  gsub("\u2029", "\\u2029", json, fixed = TRUE)
}

inject_preview_html <- function(html, theme = "auto", state = NULL) {
  if (!is.null(state)) {
    bootstrap <- paste0(
      '{"status":', preview_status_json(state), ',"css":',
      jsonlite::toJSON(preview_panel_css(), auto_unbox = TRUE), "}"
    )
    assets <- paste0(
      "<script>window.__updocPreviewBootstrap=",
      escape_preview_script_json(bootstrap), ";</script>",
      "<script>", preview_asset_text("doc.js", "mascot"), "</script>",
      '<script data-updoc-preview="true" data-updoc-theme="',
      escape_preview_html(theme), '">',
      preview_asset_text("panel-primitives.js"), "</script>",
      "<script>", preview_asset_text("panel.js"), "</script>",
      "<script>", preview_asset_text("reload.js"), "</script>"
    )
    opening_body <- regexpr(
      "<body(?=[[:space:]>])(?:[^>\"']|\"[^\"]*\"|'[^']*')*>",
      html, ignore.case = TRUE, perl = TRUE
    )
    if (opening_body[[1L]] < 0L) return(paste0(html, assets))
    body_start <- opening_body[[1L]] + attr(opening_body, "match.length")[[1L]]
    # Mount before the first body paint; splicing preserves literal JS escapes.
    return(paste0(
      substr(html, 1L, body_start - 1L), assets, substring(html, body_start)
    ))
  }
  assets <- paste0(
    '<script defer src="/__updoc__/doc.js"></script>',
    '<script defer src="/__updoc__/panel-primitives.js" ',
    'data-updoc-preview="true" data-updoc-theme="', escape_preview_html(theme),
    '"></script>',
    '<script defer src="/__updoc__/panel.js"></script>',
    '<script defer src="/__updoc__/reload.js"></script>'
  )
  if (grepl("</head>", html, ignore.case = TRUE)) {
    sub("</head>", paste0(assets, "</head>"), html,
        ignore.case = TRUE)
  } else {
    paste0(assets, html)
  }
}

parse_query_params <- function(query) {
  if (is.null(query) || !is.character(query) || length(query) != 1L ||
      is.na(query)) {
    return(list())
  }
  text <- sub("^\\?", "", query)
  if (!nzchar(text)) return(list())
  pairs <- strsplit(text, "&", fixed = TRUE)[[1L]]
  out <- list()
  for (pair in pairs) {
    if (!nzchar(pair)) next
    eq <- regexpr("=", pair, fixed = TRUE)[[1L]]
    if (eq < 0L) {
      key <- tryCatch(utils::URLdecode(pair), error = function(e) pair)
      value <- ""
    } else {
      raw_key <- substr(pair, 1L, eq - 1L)
      raw_val <- substring(pair, eq + 1L)
      key <- tryCatch(utils::URLdecode(raw_key), error = function(e) raw_key)
      value <- tryCatch(utils::URLdecode(raw_val), error = function(e) raw_val)
    }
    if (nzchar(key)) out[[key]] <- value
  }
  out
}

query_flag <- function(query, name) {
  params <- parse_query_params(query)
  if (!name %in% names(params)) return(FALSE)
  value <- tolower(trimws(params[[name]]))
  value %in% c("", "1", "true", "yes")
}

query_number <- function(query, name, default, min = 0, max = 300) {
  params <- parse_query_params(query)
  if (!name %in% names(params)) return(default)
  value <- suppressWarnings(as.numeric(params[[name]]))
  if (length(value) != 1L || is.na(value) || !is.finite(value)) return(default)
  min(max, max(min, value))
}

query_integer <- function(query, name) {
  params <- parse_query_params(query)
  if (!name %in% names(params)) return(NULL)
  value <- suppressWarnings(as.numeric(params[[name]]))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value < 0 || value != floor(value)) {
    return(NULL)
  }
  as.integer(value)
}

wait_for_preview_status <- function(state, timeout = 30, after = NULL) {
  deadline <- Sys.time() + timeout
  promises::promise(function(resolve, reject) {
    poll <- function() {
      if (preview_is_settled(state, after) || Sys.time() >= deadline) {
        resolve(http_response(
          200L, preview_status_json(state), "application/json; charset=utf-8",
          list("Cache-Control" = "no-store")
        ))
        return(invisible(NULL))
      }
      later::later(poll, delay = 0.05)
      invisible(NULL)
    }
    poll()
  })
}

preview_status_json <- function(state) {
  reload <- if (identical(state$reload_descriptor_version,
                          state$reload_version)) {
    state$reload_descriptor
  } else list(full = TRUE, paths = character())
  last_success <- if (is.na(state$last_success)) NULL else {
    format(state$last_success, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  }
  jsonlite::toJSON(
    list(
      version = state$reload_version,
      reload = list(full = isTRUE(reload$full), paths = I(reload$paths)),
      phase = state_phase(state),
      url = if (is.na(state$url)) NULL else state$url,
      rebuild_count = state$rebuild_count,
      last_success = last_success,
      duration = state$last_duration,
      changed = I(unname(state$last_changed)),
      progress = if (isTRUE(state$building)) state$progress else NULL,
      queued = length(state$pending) > 0L || length(state$debounced) > 0L,
      warnings = I(build_warning_payload(
        state$last_warnings, state$root, state$last_changed
      )),
      error = build_error_payload(state$last_error, state$root),
      build_log = state$build_log
    ),
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
}

escape_preview_html <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text
}

preview_package_name <- function(state) {
  value <- tryCatch(
    read.dcf(fs::path(state$root, "DESCRIPTION"), fields = "Package")[[1L]],
    error = function(e) NULL
  )
  if (is.null(value) || !nzchar(value)) "Documentation" else value
}

preview_placeholder_html <- function(state) {
  package <- escape_preview_html(preview_package_name(state))
  title <- paste("Preparing", package, "documentation")
  inject_preview_html(paste0(
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", title, "</title><style>",
    ":root{color-scheme:light dark;--background:#fff;--foreground:#09090b;",
    "--accent:#f4f4f5;--border:#e4e4e7;--radius:.5rem}",
    "*{box-sizing:border-box}html,body{min-height:100%;margin:0}",
    "body{background:var(--background);color:var(--foreground);",
    "font:14px/1.5 ui-sans-serif,system-ui,-apple-system,sans-serif}",
    ".updoc-placeholder{min-height:100vh}.updoc-shell{width:min(1120px,",
    "calc(100% - 48px));margin:0 auto}.updoc-header{border-bottom:1px solid ",
    "var(--border)}.updoc-nav{height:64px;display:flex;align-items:center;gap:28px}",
    ".updoc-brand{font-weight:600;font-size:16px;letter-spacing:-.01em;",
    "margin-right:auto}.updoc-layout{display:grid;grid-template-columns:190px ",
    "minmax(0,720px);gap:64px;padding:48px 0 96px}.updoc-sidebar{display:grid;",
    "align-content:start;gap:14px;padding-top:4px}.updoc-article{min-width:0}",
    ".ui-skeleton{display:block;height:12px;border-radius:calc(var(--radius) - 2px);",
    "background:var(--accent);animation:updoc-skeleton-pulse 2s ",
    "cubic-bezier(.4,0,.6,1) infinite}.updoc-navitem{width:58px}",
    ".updoc-search{width:168px;height:34px}",
    ".updoc-side-title{width:72%;height:14px;margin-bottom:8px}",
    ".updoc-side-row{width:88%}.updoc-side-row:nth-child(odd){width:68%}",
    ".updoc-crumb{width:120px;margin-bottom:24px}.updoc-heading{width:56%;",
    "height:38px;margin-bottom:28px}.updoc-line{height:14px;margin:12px 0}",
    ".updoc-line:nth-child(2){width:94%}.updoc-line:nth-child(3){width:78%}",
    ".updoc-card{border:1px solid var(--border);border-radius:6px;",
    "padding:24px;margin:32px 0}.updoc-card .ui-skeleton{margin:12px 0}",
    ".updoc-card .ui-skeleton:nth-child(1){width:38%;height:16px;",
    "margin-top:0}.updoc-card .ui-skeleton:nth-child(2){width:86%}",
    ".updoc-card .ui-skeleton:nth-child(3){width:64%;margin-bottom:0}",
    ".updoc-section{width:42%;height:24px;margin:40px 0 20px}",
    ".updoc-sr{position:absolute;width:1px;height:1px;padding:0;margin:-1px;",
    "overflow:hidden;clip:rect(0,0,0,0);white-space:nowrap;border:0}",
    "@keyframes updoc-skeleton-pulse{50%{opacity:.5}}",
    "@media(prefers-reduced-motion:reduce){.ui-skeleton{animation:none}}",
    "@media(prefers-color-scheme:dark){:root{--background:#09090b;",
    "--foreground:#fafafa;--accent:#27272a;--border:#27272a}}",
    "@media(max-width:760px){.updoc-shell{width:min(100% - 32px,720px)}",
    ".updoc-navitem{display:none}.updoc-search{width:96px}",
    ".updoc-layout{display:block;padding-top:36px}.updoc-sidebar{display:none}",
    ".updoc-heading{width:78%}}",
    "</style></head><body><div class=\"updoc-placeholder\" aria-busy=\"true\">",
    "<header class=\"updoc-header\"><div class=\"updoc-shell updoc-nav\">",
    "<div class=\"updoc-brand\">", package, "</div>",
    "<div data-slot=\"skeleton\" class=\"ui-skeleton updoc-navitem\"></div>",
    "<div data-slot=\"skeleton\" class=\"ui-skeleton updoc-navitem\"></div>",
    "<div data-slot=\"skeleton\" class=\"ui-skeleton updoc-search\"></div>",
    "</div></header>",
    "<main class=\"updoc-shell updoc-layout\" aria-hidden=\"true\">",
    "<aside class=\"updoc-sidebar\"><div data-slot=\"skeleton\" ",
    "class=\"ui-skeleton updoc-side-title\"></div>",
    paste(rep('<div data-slot="skeleton" class="ui-skeleton updoc-side-row"></div>', 6L),
          collapse = ""),
    "</aside><article class=\"updoc-article\">",
    "<div data-slot=\"skeleton\" class=\"ui-skeleton updoc-crumb\"></div>",
    "<div data-slot=\"skeleton\" class=\"ui-skeleton updoc-heading\"></div>",
    paste(rep('<div data-slot="skeleton" class="ui-skeleton updoc-line"></div>', 3L),
          collapse = ""),
    "<div class=\"updoc-card\">",
    paste(rep('<div data-slot="skeleton" class="ui-skeleton"></div>', 3L),
          collapse = ""),
    "</div><div data-slot=\"skeleton\" class=\"ui-skeleton ",
    "updoc-section\"></div>",
    paste(rep('<div data-slot="skeleton" class="ui-skeleton updoc-line"></div>', 3L),
          collapse = ""),
    "</article></main><p class=\"updoc-sr\">", title, ".</p>",
    "</div></body></html>"
  ), state$theme, state)
}

preview_missing_page_html <- function(state) {
  package <- escape_preview_html(preview_package_name(state))
  inject_preview_html(paste0(
    '<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>Page unavailable - ', package, '</title><style>',
    ':root{color-scheme:light dark}body{margin:0;background:#fff;color:#09090b;',
    'font:16px/1.6 ui-sans-serif,system-ui,-apple-system,sans-serif}',
    'main{max-width:640px;margin:12vh auto;padding:24px 24px 160px}',
    'h1{font-size:28px;line-height:1.2}a{color:inherit}',
    '@media(prefers-color-scheme:dark){body{background:#09090b;color:#fafafa}}',
    '</style></head><body><main><h1>Page unavailable</h1>',
    '<p>This page is not in the latest successful build. ',
    'The preview will refresh when it becomes available.</p>',
    '<p>Check the address, retry using the preview controls, or ',
    '<a href="/">return to the home page</a>.</p></main></body></html>'
  ), state$theme, state)
}

safe_site_request_path <- function(request_path) {
  decoded <- tryCatch(utils::URLdecode(request_path), error = function(e) "")
  if (!nzchar(decoded) || grepl("\\0", decoded, fixed = TRUE) ||
      grepl("\\", decoded, fixed = TRUE)) return(NULL)
  decoded <- sub("^/+", "", decoded)
  parts <- strsplit(decoded, "/", fixed = TRUE)[[1L]]
  if (any(parts %in% c("..", "."))) return(NULL)
  decoded
}

safe_site_file <- function(site_dir, request_path) {
  decoded <- safe_site_request_path(request_path)
  if (is.null(decoded)) return(NULL)
  candidate <- fs::path(site_dir, decoded)
  if (fs::dir_exists(candidate)) candidate <- fs::path(candidate, "index.html")
  if (!fs::file_exists(candidate)) return(NULL)
  actual <- fs::path_real(candidate)
  root <- fs::path_real(site_dir)
  if (!startsWith(actual, paste0(root, .Platform$file.sep))) return(NULL)
  actual
}

preview_directory_location <- function(decoded, query = NULL) {
  parts <- strsplit(decoded, "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  encoded <- vapply(parts, utils::URLencode, character(1), reserved = TRUE,
                     repeated = TRUE)
  location <- paste0("/", paste(encoded, collapse = "/"),
                     if (length(parts)) "/" else "")
  if (!is.null(query) && nzchar(query)) location <- paste0(location, "?", query)
  location
}

preview_app <- function(state) {
  list(call = function(req) {
    method <- req$REQUEST_METHOD
    if (is.null(method)) method <- "GET"
    path <- req$PATH_INFO
    if (is.null(path)) path <- "/"
    if (identical(path, "/__updoc__/rebuild")) {
      if (!identical(method, "POST")) {
        return(http_response(
          405L, "Method not allowed", "text/plain; charset=utf-8",
          list(Allow = "POST")
        ))
      }
      if (!identical(req$HTTP_X_UPDOC_REQUEST, "rebuild")) {
        return(http_response(
          403L, "Forbidden", "text/plain; charset=utf-8"
        ))
      }
      force_full <- query_flag(req$QUERY_STRING, "full")
      request_rebuild(
        state, fs::path(state$root, "<rebuild>"),
        force_full = force_full, asynchronous = TRUE
      )
      return(http_response(
        202L, preview_status_json(state), "application/json; charset=utf-8",
        list("Cache-Control" = "no-store")
      ))
    }
    if (!method %in% c("GET", "HEAD")) {
      return(http_response(405L, "Method not allowed", "text/plain; charset=utf-8",
                           list(Allow = "GET, HEAD")))
    }
    if (identical(path, "/__updoc__/version")) {
      body <- sprintf('{"version":%d}', state$reload_version)
      if (method == "HEAD") body <- raw()
      return(http_response(200L, body, "application/json; charset=utf-8",
                           list("Cache-Control" = "no-store")))
    }
    if (identical(path, "/__updoc__/status")) {
      if (identical(method, "GET") && query_flag(req$QUERY_STRING, "wait")) {
        flush_watched_changes(state)
        timeout <- query_number(req$QUERY_STRING, "timeout", default = 30,
                                min = 0, max = 300)
        after <- query_integer(req$QUERY_STRING, "after")
        if (timeout > 0 && !preview_is_settled(state, after)) {
          return(wait_for_preview_status(state, timeout = timeout, after = after))
        }
      }
      body <- preview_status_json(state)
      if (method == "HEAD") body <- raw()
      return(http_response(200L, body, "application/json; charset=utf-8",
                           list("Cache-Control" = "no-store")))
    }
    if (identical(path, "/__updoc__/log")) {
      log_path <- state$build_log
      if (!is.character(log_path) || length(log_path) != 1L ||
          is.na(log_path) || !nzchar(log_path) || !fs::file_exists(log_path)) {
        body <- if (method == "HEAD") raw() else "No build log available"
        return(http_response(404L, body, "text/plain; charset=utf-8",
                             list("Cache-Control" = "no-store")))
      }
      body <- if (method == "HEAD") raw() else {
        readBin(log_path, what = "raw", n = file.info(log_path)$size)
      }
      return(http_response(200L, body, "text/plain; charset=utf-8",
                           list("Cache-Control" = "no-store")))
    }
    if (identical(path, "/__updoc__/reload.js")) {
      file <- system.file("preview", "reload.js", package = "updoc",
                          mustWork = TRUE)
      body <- if (method == "HEAD") raw() else
        readBin(file, what = "raw", n = file.info(file)$size)
      return(http_response(200L, body, content_type(file),
                           list("Cache-Control" = "no-store")))
    }
    if (identical(path, "/__updoc__/panel.js")) {
      file <- system.file("preview", "panel.js", package = "updoc",
                          mustWork = TRUE)
      body <- if (method == "HEAD") raw() else
        readBin(file, what = "raw", n = file.info(file)$size)
      return(http_response(200L, body, content_type(file),
                           list("Cache-Control" = "no-store")))
    }
    if (identical(path, "/__updoc__/panel-primitives.js")) {
      file <- system.file("preview", "panel-primitives.js", package = "updoc",
                          mustWork = TRUE)
      body <- if (method == "HEAD") raw() else
        readBin(file, what = "raw", n = file.info(file)$size)
      return(http_response(200L, body, content_type(file),
                           list("Cache-Control" = "no-store")))
    }
    if (identical(path, "/__updoc__/doc.js")) {
      file <- system.file("mascot", "doc.js", package = "updoc",
                          mustWork = TRUE)
      body <- if (method == "HEAD") raw() else
        readBin(file, what = "raw", n = file.info(file)$size)
      return(http_response(200L, body, content_type(file),
                           list("Cache-Control" = "no-store")))
    }
    if (identical(path, "/__updoc__/panel.css")) {
      body <- if (method == "HEAD") raw() else charToRaw(preview_panel_css())
      return(http_response(200L, body, "text/css; charset=utf-8",
                           list("Cache-Control" = "no-store")))
    }
    if (startsWith(path, "/__updoc__/")) {
      return(http_response(404L, "Not found", "text/plain; charset=utf-8"))
    }

    file <- safe_site_file(state$site_dir, path)
    decoded <- safe_site_request_path(path)
    directory <- !is.null(decoded) && if (is.null(file)) {
      !nzchar(fs::path_ext(decoded)) || endsWith(decoded, "/")
    } else fs::dir_exists(fs::path(state$site_dir, decoded))
    # Canonicalize directories even before their indexes are built, so loading
    # pages recover and relative links have the same base after publication.
    if (directory && !identical(path, preview_directory_location(decoded))) {
      return(http_response(308L, headers = list(
        Location = preview_directory_location(decoded, req$QUERY_STRING),
        "Cache-Control" = "no-store"
      )))
    }
    if (is.null(file)) {
      document <- !is.null(decoded) &&
        (directory || tolower(fs::path_ext(decoded)) %in% c("html", "htm"))
      if (document) {
        preparing <- isTRUE(state$building) || !is.null(state$last_error)
        body <- if (preparing) preview_placeholder_html(state) else
          preview_missing_page_html(state)
        if (method == "HEAD") body <- raw()
        status <- if (identical(path, "/") && preparing) 200L else 404L
        return(http_response(status, body, "text/html; charset=utf-8",
                             list("Cache-Control" = "no-store")))
      }
      return(http_response(404L, "Not found", "text/plain; charset=utf-8"))
    }
    type <- content_type(file)
    if (startsWith(type, "text/html")) {
      html <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"),
                    collapse = "\n")
      body <- inject_preview_html(html, state$theme, state)
    } else {
      body <- readBin(file, what = "raw", n = file.info(file)$size)
    }
    if (method == "HEAD") body <- raw()
    http_response(200L, body, type, list("Cache-Control" = "no-cache"))
  })
}

start_preview_server <- function(state) {
  app <- preview_app(state)
  requested <- state$requested_port
  server <- if (requested > 0L) {
    tryCatch(
      httpuv::startServer(state$host, requested, app, quiet = TRUE),
      error = function(e) NULL
    )
  } else NULL
  if (is.null(server)) {
    for (attempt in seq_len(10L)) {
      candidate <- tryCatch(httpuv::randomPort(), error = function(e) NA_integer_)
      if (is.na(candidate)) candidate <- sample.int(16000L, 1L) + 49151L
      server <- tryCatch(
        httpuv::startServer(state$host, candidate, app, quiet = TRUE),
        error = function(e) NULL
      )
      if (!is.null(server)) break
    }
  }
  if (is.null(server)) {
    detail <- if (requested > 0L) {
      sprintf("requested port %d and fallback ports were unavailable.", requested)
    } else {
      "no available local port was found."
    }
    updoc_abort(sprintf(
      "Could not start a local preview server for %s; %s",
      state$root, detail
    ))
  }
  state$server <- server
  state$port <- as.integer(server$getPort())
  state$url <- sprintf("http://%s:%d", state$host, state$port)
  state$active <- TRUE
  invisible(server)
}
