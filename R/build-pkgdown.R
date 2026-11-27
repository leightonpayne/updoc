relay_build_messages <- function(code, quiet) {
  log <- getOption("updoc.capture_log")
  if (isTRUE(quiet) && !is.null(log)) {
    value <- NULL
    # Retain quiet R output in the log too. Warnings continue to the build's
    # condition handler, and native subprocess stderr is captured outside R.
    invisible(utils::capture.output(
      value <- withCallingHandlers(code, message = function(condition) {
        log_build_condition(condition)
        invokeRestart("muffleMessage")
      }),
      file = log,
      append = TRUE,
      type = "output"
    ))
    return(value)
  }
  if (isTRUE(quiet)) {
    value <- NULL
    invisible(utils::capture.output(
      invisible(utils::capture.output(
        value <- suppressMessages(code),
        type = "message"
      )),
      type = "output"
    ))
    return(value)
  }
  if (!interactive()) return(code)

  withCallingHandlers(
    code,
    message = function(condition) {
      text <- conditionMessage(condition)
      text <- sub("[\r\n]+$", "", text)
      if (nzchar(text)) cli::cli_inform("{text}", .envir = environment())
      invokeRestart("muffleMessage")
    }
  )
}

run_documentation <- function(state) {
  before <- documentation_signature(state$build_root)
  report_build_progress(state, "Documenting package", "R/ and man/")
  relay_build_messages(
    roxygen2::roxygenise(
      state$build_root,
      roclets = c("collate", "namespace", "rd")
    ),
    quiet = isTRUE(state$quiet) || isTRUE(state$worker)
  )
  after <- documentation_signature(state$build_root)
  changed <- signature_changes(before, after)
  invisible(list(
    changed_rd = basename(grep(
      "^man/.*[.]Rd$",
      changed$changed,
      value = TRUE
    )),
    removed_rd = basename(grep(
      "^man/.*[.]Rd$",
      changed$removed,
      value = TRUE
    )),
    namespace_changed = "NAMESPACE" %in% changed$changed
  ))
}

run_readme <- function(state) {
  input <- fs::path(state$build_root, "README.Rmd")
  if (!fs::file_exists(input)) return(invisible(FALSE))
  report_build_progress(state, "Rendering README", "README.Rmd")
  rmarkdown::render(
    input = input,
    output_file = "README.md",
    output_dir = state$build_root,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  invisible(TRUE)
}

files_signature <- function(files, root) {
  files <- unique(as.character(files))
  if (!length(files)) return(character())
  relative <- display_paths(files, root)
  exists <- fs::file_exists(files)
  hashes <- rep(NA_character_, length(files))
  hashes[exists] <- unname(tools::md5sum(files[exists]))
  stats::setNames(hashes, relative)
}

documentation_signature <- function(root) {
  man <- fs::path(root, "man")
  rd <- if (dir.exists(man)) {
    fs::dir_ls(man, type = "file", glob = "*.Rd", fail = FALSE)
  } else character()
  files <- c(fs::path(root, c("DESCRIPTION", "NAMESPACE")), rd)
  files_signature(files[fs::file_exists(files)], root)
}

signature_changes <- function(before, after) {
  paths <- union(names(before), names(after))
  old <- unname(before[paths])
  new <- unname(after[paths])
  different <- xor(is.na(old), is.na(new)) |
    (!is.na(old) & !is.na(new) & old != new)
  list(
    changed = paths[different & !is.na(new)],
    removed = paths[different & is.na(new)]
  )
}

record_publish_changes <- function(state, before, after) {
  if (is.null(state$publish_delta)) return(invisible(FALSE))
  delta <- signature_changes(before, after)
  write <- delta$changed
  remove <- delta$removed
  if (length(write)) {
    state$publish_delta$write[write] <- after[write]
    state$publish_delta$remove <- setdiff(state$publish_delta$remove, write)
  }
  if (length(remove)) {
    state$publish_delta$write <- state$publish_delta$write[
      !names(state$publish_delta$write) %in% remove
    ]
    state$publish_delta$remove <- union(state$publish_delta$remove, remove)
  }
  invisible(length(write) > 0L || length(remove) > 0L)
}

section_output_files <- function(plan, pkg, site_dir) {
  files <- character()
  scan_directory <- function(directory) {
    if (!dir.exists(directory)) return(character())
    display_paths(
      fs::dir_ls(
        directory,
        recurse = TRUE,
        type = "file",
        fail = FALSE
      ),
      site_dir
    )
  }

  if ("home" %in% plan$targets) files <- c(files, "index.html")
  if ("reference" %in% plan$targets) {
    files <- c(files, "reference/index.html")
    if (!isTRUE(plan$reference_index_only)) {
      topic_outputs <- pkg$topics$file_out[
        pkg$topics$file_in %in% plan$reference_files
      ]
      if (
        !isTRUE(plan$reference_unscoped) &&
          length(plan$reference_files) &&
          length(topic_outputs)
      ) {
        files <- c(files, fs::path("reference", topic_outputs))
        figures <- fs::path(site_dir, "reference", "figures")
        if (dir.exists(figures)) {
          stems <- tools::file_path_sans_ext(topic_outputs)
          figure_files <- fs::dir_ls(
            figures,
            recurse = TRUE,
            type = "file",
            fail = FALSE
          )
          figure_names <- fs::path_file(figure_files)
          keep <- vapply(
            stems,
            function(stem) {
              startsWith(figure_names, paste0(stem, "-"))
            },
            logical(length(figure_names))
          )
          if (length(stems)) {
            figure_files <- figure_files[rowSums(keep) > 0L]
            files <- c(files, display_paths(figure_files, site_dir))
          }
        }
      } else {
        files <- c(files, scan_directory(fs::path(site_dir, "reference")))
      }
    }
  }
  if ("articles" %in% plan$targets) {
    files <- c(files, "articles/index.html")
    if (length(plan$article_names)) {
      outputs <- pkg$vignettes$file_out[
        pkg$vignettes$name %in% plan$article_names
      ]
      files <- c(files, outputs)
      support <- fs::path(
        site_dir,
        paste0(tools::file_path_sans_ext(outputs), "_files")
      )
      for (directory in support) {
        files <- c(files, scan_directory(directory))
      }
    } else {
      files <- c(files, scan_directory(fs::path(site_dir, "articles")))
    }
  }
  if ("tutorials" %in% plan$targets) {
    files <- c(files, scan_directory(fs::path(site_dir, "tutorials")))
  }
  if ("news" %in% plan$targets) {
    files <- c(files, scan_directory(fs::path(site_dir, "news")))
  }
  unique(gsub("\\\\", "/", files))
}

section_output_signature <- function(plan, pkg, site_dir) {
  relative <- section_output_files(plan, pkg, site_dir)
  files_signature(fs::path(site_dir, relative), site_dir)
}

searchable_output_signature <- function(signature) {
  signature[grepl("[.]html?$", names(signature), ignore.case = TRUE)]
}

apply_documentation_changes <- function(plan, documentation) {
  if (
    !is.list(documentation) ||
      !identical(plan$kind, "sections") ||
      !"reference" %in% plan$targets
  ) {
    return(plan)
  }
  changed <- if (is.null(documentation$changed_rd)) {
    character()
  } else unique(documentation$changed_rd)
  removed <- if (is.null(documentation$removed_rd)) {
    character()
  } else unique(documentation$removed_rd)
  namespace_changed <- isTRUE(documentation$namespace_changed)

  if (length(removed)) {
    plan$reference_files <- character()
    plan$reference_lazy <- FALSE
  } else if (length(changed)) {
    plan$reference_files <- unique(c(plan$reference_files, changed))
  } else if (namespace_changed && !length(plan$reference_files)) {
    plan$reference_index_only <- TRUE
  } else if (!length(plan$reference_files)) {
    plan$targets <- setdiff(plan$targets, "reference")
  }
  plan
}

track_pkgdown_reads <- function(state, operation, total, code) {
  current <- 0L
  withCallingHandlers(
    code,
    message = function(condition) {
      lines <- strsplit(
        cli::ansi_strip(conditionMessage(condition)),
        "[\r\n]+"
      )[[1L]]
      lines <- trimws(lines)
      reads <- grep("^Reading[[:space:]]+", lines, value = TRUE)
      for (line in reads) {
        current <<- current + 1L
        target <- sub("^Reading[[:space:]]+", "", line)
        report_build_progress(
          state,
          operation,
          target,
          current,
          max(as.integer(total), current),
          announce = FALSE
        )
      }
    }
  )
}

check_preview_site <- function(pkg) {
  index <- fs::path(pkg$dst_path, "index.html")
  if (!fs::file_exists(index)) {
    updoc_abort(sprintf(
      "pkgdown did not produce the expected home page at %s.",
      index
    ))
  }
  invisible(TRUE)
}

checked_preview_output_dir <- function(state, pkg) {
  base <- as.character(fs::path_norm(fs::path_abs(
    state$destination,
    start = state$build_root
  )))
  output <- as.character(fs::path_norm(fs::path_abs(pkg$dst_path)))
  within <- function(path, root) {
    identical(path, root) || startsWith(path, paste0(root, "/"))
  }
  unsafe <- function() {
    updoc_abort(paste(
      "Preview output must stay within its temporary work directory.",
      "Use a relative `development.destination` without `..` or symbolic links."
    ))
  }

  if (isTRUE(pkg$development$in_dev)) {
    development <- gsub("\\\\", "/", pkg$development$destination)
    parts <- strsplit(development, "/", fixed = TRUE)[[1L]]
    if (
      fs::is_absolute_path(development) ||
        grepl("^[A-Za-z]:", development) ||
        any(parts == "..")
    ) {
      unsafe()
    }
  }
  if (!within(output, base)) unsafe()

  # Reject links in the managed output path, including a replaced work root.
  # Checking existing ancestors also covers a link above a not-yet-created leaf.
  candidate <- output
  repeat {
    if (fs::is_link(candidate)) unsafe()
    if (identical(candidate, base)) break
    candidate <- as.character(fs::path_dir(candidate))
  }

  resolve_existing <- function(path) {
    ancestor <- path
    while (
      !fs::file_exists(ancestor) &&
        !dir.exists(ancestor) &&
        !fs::is_link(ancestor)
    ) {
      ancestor <- as.character(fs::path_dir(ancestor))
    }
    actual <- tryCatch(fs::path_real(ancestor), error = function(e) unsafe())
    as.character(fs::path_norm(fs::path(
      actual,
      fs::path_rel(path, start = ancestor)
    )))
  }
  # Resolve the parent separately so a redirected work root cannot redefine
  # the allowed directory. This also catches filesystem junctions on Windows.
  expected <- as.character(fs::path(
    resolve_existing(as.character(fs::path_dir(base))),
    fs::path_file(base)
  ))
  if (!within(resolve_existing(output), expected)) unsafe()
  output
}

run_pkgdown_site <- function(state, plan, override) {
  report_build_progress(state, "Loading pkgdown configuration")
  pkg <- pkgdown::as_pkgdown(state$build_root, override = override)
  site_dir <- checked_preview_output_dir(state, pkg)
  state$pkgdown <- pkg
  state$site_dir <- site_dir
  # Full builds remove stale output left by deleted sources.
  if (!isTRUE(plan$lazy) && dir.exists(pkg$dst_path)) {
    fs::dir_delete(pkg$dst_path)
  }
  destination_empty <- !dir.exists(pkg$dst_path) ||
    !length(fs::dir_ls(pkg$dst_path, all = TRUE, fail = FALSE))

  if (!isTRUE(plan$lazy) || destination_empty) {
    report_build_progress(
      state,
      "Initializing site",
      "assets, template, and metadata"
    )
    pkgdown::init_site(pkg, override = override)
  }

  report_build_progress(state, "Building home page", "README and index")
  pkgdown::build_home(pkg, override = override, preview = FALSE, quiet = TRUE)

  topic_count <- nrow(pkg$topics)
  report_build_progress(
    state,
    "Building reference",
    sprintf("%d topic%s", topic_count, if (topic_count == 1L) "" else "s")
  )
  track_pkgdown_reads(
    state,
    "Building reference topic",
    topic_count,
    pkgdown::build_reference(
      pkg,
      lazy = plan$lazy,
      preview = FALSE,
      devel = TRUE
    )
  )

  article_count <- nrow(pkg$vignettes)
  if (article_count) {
    if (isTRUE(plan$lazy)) {
      report_build_progress(
        state,
        "Building articles",
        sprintf(
          "%d article%s",
          article_count,
          if (article_count == 1L) "" else "s"
        )
      )
      track_pkgdown_reads(
        state,
        "Building article",
        article_count,
        pkgdown::build_articles(
          pkg,
          lazy = TRUE,
          preview = FALSE,
          quiet = TRUE
        )
      )
    } else {
      report_build_progress(state, "Building articles index")
      pkgdown::build_articles_index(pkg)
      build_targeted_articles(
        state,
        pkg,
        pkg$vignettes$name,
        override
      )
    }
  }

  tutorial_count <- nrow(pkg$tutorials)
  report_build_progress(
    state,
    "Building tutorials",
    sprintf(
      "%d tutorial%s",
      tutorial_count,
      if (tutorial_count == 1L) "" else "s"
    )
  )
  pkgdown::build_tutorials(pkg, override = override, preview = FALSE)

  report_build_progress(state, "Building news")
  pkgdown::build_news(pkg, override = override, preview = FALSE)

  if (isTRUE(pkg$bs_version > 3)) {
    report_build_progress(state, "Building LLM metadata")
    pkgdown::build_llm_docs(pkg)
  }
  report_build_progress(state, "Building redirects")
  pkgdown::build_redirects(pkg, override = override)
  if (isTRUE(pkg$bs_version > 3)) {
    report_build_progress(state, "Building search index")
    pkgdown::build_search(pkg, override = override)
  }
  report_build_progress(state, "Checking generated site")
  check_preview_site(pkg)

  state$indexes_dirty <- FALSE
  state$reload_dirty <- TRUE
  invisible(TRUE)
}

run_pkgdown <- function(state, plan) {
  state$markdown_paths <- character()
  override <- destination_override(state$build_root, state$destination)
  relay_build_messages(
    if (identical(plan$kind, "site")) {
      run_pkgdown_site(state, plan, override)
    } else if (!length(plan$targets)) {
      invisible(NULL)
    } else {
      report_build_progress(state, "Loading pkgdown configuration")
      pkg <- pkgdown::as_pkgdown(state$build_root, override = override)
      site_dir <- checked_preview_output_dir(state, pkg)
      state$pkgdown <- pkg
      state$site_dir <- site_dir
      if ("assets" %in% plan$targets) {
        report_build_progress(
          state,
          "Refreshing site assets",
          "CSS, JavaScript, logo, and favicons"
        )
        pkgdown::init_site(pkg)
        state$reload_dirty <- TRUE
      }
      section_targets <- setdiff(plan$targets, "assets")
      if (!length(section_targets)) {
        invisible(NULL)
      } else if (!isTRUE(pkg$bs_version > 3)) {
        report_build_progress(
          state,
          "Building pkgdown site",
          "Bootstrap 3 incremental site"
        )
        pkgdown::build_site(
          pkg = pkg,
          lazy = TRUE,
          preview = FALSE,
          devel = TRUE,
          quiet = TRUE
        )
        state$indexes_dirty <- FALSE
        state$reload_dirty <- TRUE
      } else {
        baseline <- if (
          !is.null(state$publish_delta) &&
            !is.null(state$published_site_dir) &&
            dir.exists(state$published_site_dir)
        ) {
          state$published_site_dir
        } else state$site_dir
        before <- section_output_signature(plan, pkg, baseline)
        if ("home" %in% plan$targets) {
          report_build_progress(state, "Building home page", "index")
          pkgdown::build_home_index(pkg, quiet = TRUE)
        }
        if ("reference" %in% plan$targets) {
          if (isTRUE(plan$reference_index_only)) {
            report_build_progress(state, "Building reference index")
            pkgdown::build_reference_index(pkg)
          } else {
            topic_names <- pkg$topics$name[
              pkg$topics$file_in %in% plan$reference_files
            ]
            if (
              !isTRUE(plan$reference_unscoped) &&
                length(plan$reference_files) &&
                length(topic_names)
            ) {
              report_build_progress(
                state,
                "Building reference",
                paste(plan$reference_files, collapse = ", ")
              )
              pkgdown::build_reference(
                pkg,
                preview = FALSE,
                devel = TRUE,
                topics = unique(topic_names)
              )
            } else {
              report_build_progress(
                state,
                "Building reference",
                if (isTRUE(plan$reference_lazy)) "changed topics" else
                  "all topics"
              )
              pkgdown::build_reference(
                pkg,
                lazy = plan$reference_lazy,
                preview = FALSE,
                devel = TRUE
              )
            }
          }
        }
        if ("articles" %in% plan$targets) {
          if (length(plan$article_names)) {
            if (isTRUE(plan$article_index)) {
              report_build_progress(state, "Building articles index")
              pkgdown::build_articles_index(pkg)
            }
            completed <- build_targeted_articles(
              state,
              pkg,
              plan$article_names,
              override
            )
            if (!isTRUE(completed)) return(invisible(FALSE))
          } else {
            report_build_progress(
              state,
              "Building articles",
              if (isTRUE(plan$articles_lazy)) "changed articles" else
                "all articles"
            )
            pkgdown::build_articles(
              pkg,
              lazy = plan$articles_lazy,
              preview = FALSE,
              quiet = TRUE
            )
          }
        }
        if ("tutorials" %in% plan$targets) {
          report_build_progress(state, "Building tutorials")
          pkgdown::build_tutorials(pkg, preview = FALSE)
        }
        if ("news" %in% plan$targets) {
          report_build_progress(state, "Building news", "NEWS.md")
          pkgdown::build_news(pkg, preview = FALSE)
        }
        after <- section_output_signature(plan, pkg, state$site_dir)
        record_publish_changes(state, before, after)
        output_changed <- !identical(before, after)
        search_output_changed <- !identical(
          searchable_output_signature(before),
          searchable_output_signature(after)
        )
        state$markdown_paths <- signature_changes(
          searchable_output_signature(before),
          searchable_output_signature(after)
        )$changed
        state$indexes_dirty <- isTRUE(state$indexes_dirty) ||
          search_output_changed
        state$reload_dirty <- isTRUE(state$reload_dirty) || output_changed
      }
    },
    quiet = isTRUE(state$quiet) || isTRUE(state$worker)
  )
  invisible(TRUE)
}

shared_output_signature <- function(
  pkg,
  site_dir,
  markdown_paths = character()
) {
  # pkgdown also creates legacy nested URLs for vignettes/articles content.
  # New articles need these redirects published alongside the search index.
  articles <- pkg$vignettes$file_out[
    startsWith(pkg$vignettes$name, "articles/")
  ]
  markdown <- fs::path_ext_set(as.character(markdown_paths), "md")
  files <- c(
    "search.json",
    "llms.txt",
    fs::path("articles", articles),
    markdown
  )
  files_signature(fs::path(site_dir, files), site_dir)
}

refresh_markdown_outputs <- function(pkg, site_dir, paths) {
  if (isFALSE(pkg$meta[["llm-docs"]]) || !length(paths)) {
    return(invisible(NULL))
  }
  paths <- unique(as.character(paths))
  if (!manifest_paths_are_safe(paths)) {
    updoc_abort("Cannot refresh Markdown for unsafe preview output paths.")
  }
  stage <- tempfile("updoc-markdown-")
  fs::dir_create(stage)
  on.exit(fs::dir_delete(stage), add = TRUE)
  copy_outputs <- function(paths, from, to) {
    for (path in paths) {
      source <- fs::path(from, path)
      if (!fs::file_exists(source)) next
      target <- fs::path(to, path)
      fs::dir_create(fs::path_dir(target))
      fs::file_copy(source, target, overwrite = TRUE)
    }
  }

  # The public builder converts every HTML file it sees. Give it only changed
  # pages, plus existing index Markdown needed to reconstruct llms.txt.
  indexes <- c("index.md", "reference/index.md", "articles/index.md")
  copy_outputs(c(paths, indexes), site_dir, stage)
  staged_pkg <- pkg
  staged_pkg$dst_path <- fs::path(stage)
  pkgdown::build_llm_docs(staged_pkg)
  copy_outputs(c(fs::path_ext_set(paths, "md"), "llms.txt"), stage, site_dir)
  invisible(NULL)
}

refresh_shared_indexes <- function(state) {
  if (!isTRUE(state$indexes_dirty)) return(invisible(TRUE))
  report_build_progress(
    state,
    "Refreshing preview metadata",
    "search, redirects, and Markdown"
  )
  override <- destination_override(state$build_root, state$destination)
  pkg <- state$pkgdown
  if (is.null(pkg)) {
    pkg <- pkgdown::as_pkgdown(state$build_root, override = override)
    state$pkgdown <- pkg
  }
  # The configured destination can resolve to a development subdirectory.
  # Publication paths must be relative to the directory pkgdown actually uses.
  site_dir <- checked_preview_output_dir(state, pkg)
  baseline <- if (
    !is.null(state$publish_delta) &&
      !is.null(state$published_site_dir) &&
      dir.exists(state$published_site_dir)
  ) {
    state$published_site_dir
  } else site_dir
  before <- shared_output_signature(pkg, baseline, state$markdown_paths)
  relay_build_messages(
    {
      pkgdown::build_search(pkg)
      pkgdown::build_redirects(pkg)
      refresh_markdown_outputs(pkg, site_dir, state$markdown_paths)
    },
    quiet = isTRUE(state$quiet) || isTRUE(state$worker)
  )
  after <- shared_output_signature(pkg, site_dir, state$markdown_paths)
  record_publish_changes(state, before, after)
  state$indexes_dirty <- FALSE
  invisible(TRUE)
}
