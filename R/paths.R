package_root <- function(pkg = ".") {
  if (!is.character(pkg) || length(pkg) != 1L || is.na(pkg) || !nzchar(pkg)) {
    updoc_abort("`pkg` must be one non-empty path.")
  }
  path <- fs::path_abs(fs::path_expand(pkg))
  if (!dir.exists(path) && fs::file_exists(path)) path <- fs::path_dir(path)
  if (!dir.exists(path)) {
    updoc_abort(sprintf("Package path does not exist: %s", path))
  }

  candidate <- path
  repeat {
    desc <- fs::path(candidate, "DESCRIPTION")
    if (fs::file_exists(desc)) break
    parent <- fs::path_dir(candidate)
    if (identical(parent, candidate)) {
      updoc_abort(sprintf("Not an R package (no DESCRIPTION found): %s", path))
    }
    candidate <- parent
  }
  fields <- tryCatch(
    read.dcf(fs::path(candidate, "DESCRIPTION")),
    error = function(e) NULL
  )
  if (
    is.null(fields) ||
      !"Package" %in% colnames(fields) ||
      !nzchar(fields[1L, "Package"])
  ) {
    updoc_abort(sprintf("Invalid package DESCRIPTION: %s", candidate))
  }
  as.character(fs::path_real(candidate))
}

destination_override <- function(root, destination) {
  if (is.null(destination)) return(list())
  if (
    !is.character(destination) ||
      length(destination) != 1L ||
      is.na(destination) ||
      !nzchar(destination)
  ) {
    updoc_abort("`destination` must be NULL or one non-empty path.")
  }
  destination <- fs::path_abs(fs::path_expand(destination), start = root)
  list(destination = destination)
}

resolve_site_dir <- function(root, destination = NULL) {
  override <- destination_override(root, destination)
  configured <- tryCatch(
    pkgdown::as_pkgdown(root, override = override),
    error = function(e) {
      updoc_abort(sprintf(
        "Could not read pkgdown configuration for %s: %s",
        root,
        conditionMessage(e)
      ))
    }
  )
  site <- configured$dst_path
  if (is.null(site) || length(site) != 1L || !nzchar(site)) {
    site <- if (is.null(destination)) fs::path(root, "docs") else
      fs::path_abs(destination, start = root)
  }
  as.character(fs::path_abs(site))
}

resolve_symlink_target <- function(path) {
  tryCatch(
    as.character(fs::path_real(path)),
    error = function(e) NULL
  )
}

remove_path_safe <- function(path) {
  if (fs::is_link(path)) {
    unlink(path)
  } else if (dir.exists(path)) {
    fs::dir_delete(path)
  } else if (file.exists(path)) {
    unlink(path)
  }
  invisible(NULL)
}

copy_file_preserving_mtime <- function(source, target, mtime = NULL) {
  if (is.null(mtime)) {
    mtime <- file.info(source)$mtime
  }
  if (fs::is_link(target) || dir.exists(target)) {
    remove_path_safe(target)
  }
  fs::dir_create(fs::path_dir(target))
  fs::file_copy(source, target, overwrite = TRUE)
  if (!is.null(mtime) && length(mtime) == 1L && !is.na(mtime)) {
    suppressWarnings(try(Sys.setFileTime(target, mtime), silent = TRUE))
  }
  invisible(target)
}

copy_directory_safe <- function(
  source,
  destination,
  filter = NULL,
  only_protected = FALSE,
  ancestors = character(),
  rel_prefix = "",
  create_empty = TRUE
) {
  source_real <- resolve_symlink_target(source)
  if (
    is.null(source_real) ||
      !dir.exists(source_real) ||
      source_real %in% ancestors
  ) {
    return(invisible(destination))
  }
  ancestors <- c(ancestors, source_real)
  if (isTRUE(create_empty)) {
    fs::dir_create(destination)
  }

  # Use fs::dir_info() (follow = FALSE) and base dir.exists() to avoid the
  # fs 2.1.0 infinite loop in fs::file_info(follow = TRUE) on chained symlinks.
  info <- fs::dir_info(source_real, all = TRUE, fail = FALSE)
  if (!nrow(info)) return(invisible(destination))

  entries <- as.character(info$path)
  types <- as.character(info$type)
  mtimes <- info$modification_time

  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    type <- types[[i]]
    if (is.na(type)) next
    name <- basename(entry)
    rel <- if (nzchar(rel_prefix)) paste0(rel_prefix, "/", name) else name
    target <- fs::path(destination, name)

    is_link <- identical(type, "symlink")
    resolved <- if (is_link) resolve_symlink_target(entry) else entry
    if (is_link && is.null(resolved)) next

    entry_is_dir <- if (is_link) {
      dir.exists(resolved)
    } else {
      identical(type, "directory")
    }

    child_only_protected <- isTRUE(only_protected)
    if (is.function(filter)) {
      decision <- filter(rel, entry_is_dir, child_only_protected)
      if (identical(decision, "skip")) next
      if (identical(decision, "descend_protected")) {
        child_only_protected <- TRUE
      } else if (identical(decision, "include")) {
        child_only_protected <- FALSE
      }
    }

    if (entry_is_dir) {
      copy_directory_safe(
        source = resolved,
        destination = target,
        filter = filter,
        only_protected = child_only_protected,
        ancestors = ancestors,
        rel_prefix = rel,
        create_empty = !child_only_protected
      )
    } else {
      if (is_link) {
        target_info <- fs::file_info(resolved, fail = FALSE, follow = FALSE)
        if (
          !nrow(target_info) ||
            !identical(as.character(target_info$type[[1L]]), "file")
        ) {
          next
        }
        mtime <- target_info$modification_time[[1L]]
      } else {
        if (!identical(type, "file")) next
        mtime <- mtimes[[i]]
      }
      copy_file_preserving_mtime(resolved, target, mtime = mtime)
    }
  }
  invisible(destination)
}

prepare_initial_preview <- function(root, source_site, target) {
  source_site <- fs::path_abs(source_site)
  target <- fs::path_abs(target)
  reusable <- !identical(source_site, fs::path_abs(root)) &&
    dir.exists(source_site) &&
    fs::file_exists(fs::path(source_site, "index.html"))

  if (reusable) {
    copy_directory_safe(source_site, target)
  } else {
    fs::dir_create(target)
  }
  invisible(reusable)
}

relative_to_root <- function(path, root) {
  path <- as.character(fs::path_norm(fs::path_abs(path)))
  root <- as.character(fs::path_norm(fs::path_abs(root)))
  prefix <- paste0(root, "/")
  if (identical(path, root)) return("")
  if (!startsWith(path, prefix)) return(path)
  substring(path, nchar(prefix) + 1L)
}

resolve_root_relative_site <- function(root_abs, site_dir) {
  if (is.null(site_dir)) return(NULL)
  site_abs <- as.character(fs::path_norm(fs::path_abs(
    site_dir,
    start = root_abs
  )))
  root_real <- resolve_symlink_target(root_abs)
  if (!is.null(root_real) && !identical(root_real, root_abs)) {
    site_real <- resolve_symlink_target(site_abs)
    if (!is.null(site_real)) {
      rel <- relative_to_root(site_real, root_real)
      if (!fs::is_absolute_path(rel)) {
        site_abs <- as.character(fs::path_norm(fs::path(root_abs, rel)))
      }
    }
  }
  site_abs
}

read_rbuildignore <- function(root) {
  path <- fs::path(root, ".Rbuildignore")
  if (!file.exists(path) || dir.exists(path)) return(character())
  lines <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character()
  )
  lines <- sub("\r$", "", lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(character())
  valid <- vapply(
    lines,
    function(pattern) {
      suppressWarnings(tryCatch(
        {
          grepl(pattern, "", perl = TRUE, ignore.case = TRUE)
          TRUE
        },
        error = function(e) FALSE
      ))
    },
    logical(1)
  )
  lines[valid]
}

matches_rbuildignore <- function(rel, patterns, is_dir = FALSE) {
  if (!length(patterns) || !nzchar(rel)) return(FALSE)
  candidates <- if (isTRUE(is_dir)) c(rel, paste0(rel, "/")) else rel
  for (pattern in patterns) {
    matched <- suppressWarnings(tryCatch(
      any(grepl(pattern, candidates, perl = TRUE, ignore.case = TRUE)),
      error = function(e) FALSE
    ))
    if (matched) return(TRUE)
  }
  FALSE
}

matches_rbuildignore_or_ancestor <- function(rel, patterns, is_dir = FALSE) {
  if (!length(patterns) || !nzchar(rel)) return(FALSE)
  parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
  if (length(parts) > 1L) {
    prefix <- ""
    for (i in seq_len(length(parts) - 1L)) {
      prefix <- if (nzchar(prefix)) paste0(prefix, "/", parts[[i]]) else
        parts[[i]]
      if (matches_rbuildignore(prefix, patterns, is_dir = TRUE)) return(TRUE)
    }
  }
  matches_rbuildignore(rel, patterns, is_dir = is_dir)
}

protected_root_files <- function() {
  c(
    "DESCRIPTION",
    "NAMESPACE",
    "LICENSE",
    "LICENCE",
    "LICENSE.md",
    "LICENCE.md",
    "_pkgdown.yml",
    "_pkgdown.yaml",
    "README.Rmd",
    "README.rmd",
    "README.md",
    "index.Rmd",
    "index.rmd",
    "index.md",
    "NEWS.md",
    "NEWS",
    "CONTRIBUTING.md",
    "CODE_OF_CONDUCT.md",
    "SUPPORT.md",
    "404.md",
    "logo.svg",
    "logo.png",
    "configure",
    "configure.ac",
    "configure.in",
    "configure.win",
    "configure.ucrt",
    "cleanup",
    "cleanup.win",
    "cleanup.ucrt",
    "INDEX",
    "CITATION"
  )
}

protected_github_files <- function() {
  c(
    ".github/CONTRIBUTING.md",
    ".github/CODE_OF_CONDUCT.md",
    ".github/SUPPORT.md",
    ".github/404.md"
  )
}

protected_root_dirs <- function() {
  c("R", "man", "vignettes", "pkgdown", "inst", "data", "src")
}

is_protected_package_path <- function(rel) {
  if (!nzchar(rel)) return(TRUE)
  if (rel %in% protected_root_files() || rel %in% protected_github_files()) {
    return(TRUE)
  }
  dirs <- protected_root_dirs()
  rel %in% dirs || any(startsWith(rel, paste0(dirs, "/")))
}

has_protected_descendant <- function(rel) {
  identical(rel, ".github")
}

is_hard_excluded_rel <- function(rel, absolute = NULL, site_abs = NULL) {
  if (!is.null(site_abs) && !is.null(absolute)) {
    if (
      identical(absolute, site_abs) ||
        startsWith(absolute, paste0(site_abs, "/"))
    ) {
      return(TRUE)
    }
  }
  if (!nzchar(rel)) return(FALSE)
  parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
  excluded_dir <- parts %in%
    c(
      ".git",
      ".Rproj.user",
      "packrat",
      ".cache",
      ".quarto",
      ".knitr",
      "node_modules",
      ".venv",
      "venv"
    )
  if (any(excluded_dir)) return(TRUE)
  if (
    length(parts) >= 2L &&
      any(parts[-length(parts)] == "renv" & parts[-1L] == "library")
  ) {
    return(TRUE)
  }
  if (any(grepl("\\.Rcheck$", parts))) return(TRUE)
  name <- basename(rel)
  grepl(
    "(^#.*#$)|(~$)|(\\.sw[a-p]$)|(\\.tmp$)|(\\.temp$)|(\\.bak$)|(^\\.#[^/]+$)",
    name,
    ignore.case = TRUE
  )
}

is_excluded_path <- function(
  path,
  root,
  site_dir = NULL,
  rbuildignore = NULL,
  is_dir = FALSE
) {
  root_abs <- as.character(fs::path_norm(fs::path_abs(root)))
  absolute <- as.character(fs::path_norm(fs::path_abs(path, start = root_abs)))
  rel <- gsub("\\\\", "/", relative_to_root(absolute, root_abs))
  site_abs <- resolve_root_relative_site(root_abs, site_dir)
  if (is_hard_excluded_rel(rel, absolute, site_abs)) return(TRUE)
  if (is_protected_package_path(rel)) return(FALSE)
  if (is.null(rbuildignore)) rbuildignore <- read_rbuildignore(root_abs)
  matches_rbuildignore_or_ancestor(rel, rbuildignore, is_dir = is_dir)
}

is_relevant_path <- function(path, root, site_dir = NULL) {
  if (is_excluded_path(path, root, site_dir)) return(FALSE)
  rel <- gsub("\\\\", "/", relative_to_root(path, root))
  exact <- c(
    "DESCRIPTION",
    "NAMESPACE",
    "_pkgdown.yml",
    "_pkgdown.yaml",
    "README.Rmd",
    "README.md",
    "index.md",
    "NEWS.md"
  )
  dirs <- c("R", "man", "vignettes", "pkgdown", "inst", "data", "src")
  rel %in% exact || any(startsWith(rel, paste0(dirs, "/")))
}

watch_candidates <- function(root) {
  exact <- fs::path(
    root,
    c(
      "DESCRIPTION",
      "NAMESPACE",
      "_pkgdown.yml",
      "_pkgdown.yaml",
      "README.Rmd",
      "README.md",
      "index.md",
      "NEWS.md"
    )
  )
  dirs <- fs::path(
    root,
    c("R", "man", "vignettes", "pkgdown", "inst", "data", "src")
  )
  files <- exact[fs::file_exists(exact)]
  for (dir in dirs[dir.exists(dirs)]) {
    files <- c(
      files,
      fs::dir_ls(dir, recurse = TRUE, type = "file", all = TRUE, fail = FALSE)
    )
  }
  unique(as.character(files))
}

source_snapshot <- function(root, site_dir = NULL) {
  files <- watch_candidates(root)
  if (!length(files)) return(stats::setNames(character(), character()))

  rel <- gsub("\\\\", "/", as.character(fs::path_rel(files, start = root)))
  excluded <- grepl(
    paste0(
      "(^|/)([.]git|[.]Rproj[.]user|packrat|[.]cache|[.]quarto|",
      "[.]knitr|node_modules|[.]venv|venv)(/|$)"
    ),
    rel
  ) |
    grepl("(^|/)renv/library(/|$)", rel) |
    grepl("(^|/)[^/]+[.]Rcheck(/|$)", rel)
  names <- basename(rel)
  excluded <- excluded |
    grepl(
      "(^#.*#$)|(~$)|([.]sw[a-p]$)|([.]tmp$)|([.]temp$)|([.]bak$)|(^[.]#[^/]+$)",
      names,
      ignore.case = TRUE
    )
  if (!is.null(site_dir)) {
    site <- gsub(
      "\\\\",
      "/",
      relative_to_root(fs::path_abs(site_dir), fs::path_abs(root))
    )
    if (nzchar(site) && !fs::is_absolute_path(site)) {
      excluded <- excluded | rel == site | startsWith(rel, paste0(site, "/"))
    }
  }
  files <- sort(unique(as.character(files[!excluded])))
  if (!length(files)) return(stats::setNames(character(), character()))
  info <- file.info(files)
  values <- paste(as.numeric(info$mtime), info$size, sep = ":")
  stats::setNames(values, files)
}

snapshot_changes <- function(old, new) {
  all <- union(names(old), names(new))
  changed <- vapply(
    all,
    function(path) {
      !identical(unname(old[path]), unname(new[path]))
    },
    logical(1)
  )
  all[changed]
}

mirror_package <- function(root, destination, site_dir = NULL) {
  root_abs <- as.character(fs::path_norm(fs::path_abs(root)))
  site_abs <- resolve_root_relative_site(root_abs, site_dir)
  rbuildignore <- read_rbuildignore(root_abs)

  filter <- function(rel, entry_is_dir, only_protected) {
    absolute <- as.character(fs::path_norm(fs::path(root_abs, rel)))
    if (is_hard_excluded_rel(rel, absolute, site_abs)) return("skip")
    if (is_protected_package_path(rel)) return("include")
    if (isTRUE(only_protected)) {
      if (isTRUE(entry_is_dir) && has_protected_descendant(rel)) {
        return("descend_protected")
      }
      return("skip")
    }
    if (matches_rbuildignore(rel, rbuildignore, is_dir = entry_is_dir)) {
      if (isTRUE(entry_is_dir) && has_protected_descendant(rel)) {
        return("descend_protected")
      }
      return("skip")
    }
    "include"
  }

  copy_directory_safe(root, destination, filter = filter)
  invisible(destination)
}

sync_package_changes <- function(root, mirror, changed) {
  root_abs <- as.character(fs::path_norm(fs::path_abs(root)))
  root_real <- resolve_symlink_target(root_abs)
  ancestors <- if (is.null(root_real)) character() else root_real
  prefix <- paste0(root_abs, "/")
  changed <- unique(as.character(changed))
  for (source in changed) {
    relative <- relative_to_root(source, root_abs)
    source_abs <- as.character(fs::path_norm(fs::path_abs(source)))
    if (
      !nzchar(relative) ||
        startsWith(relative, "<") ||
        !startsWith(source_abs, prefix)
    ) {
      next
    }
    target <- fs::path(mirror, relative)
    is_link <- fs::is_link(source)
    resolved <- if (is_link) resolve_symlink_target(source) else source
    if (is_link && is.null(resolved)) {
      remove_path_safe(target)
    } else if (dir.exists(resolved)) {
      remove_path_safe(target)
      copy_directory_safe(
        resolved,
        target,
        ancestors = ancestors,
        rel_prefix = gsub("\\\\", "/", relative)
      )
    } else if (file.exists(resolved)) {
      info <- fs::file_info(resolved, fail = FALSE, follow = FALSE)
      if (nrow(info) && identical(as.character(info$type[[1L]]), "file")) {
        copy_file_preserving_mtime(
          resolved,
          target,
          mtime = info$modification_time[[1L]]
        )
      } else {
        remove_path_safe(target)
      }
    } else {
      remove_path_safe(target)
    }
  }
  invisible(mirror)
}

package_sync_paths <- function(root, mirror, snapshot, site_dir = NULL) {
  source <- names(snapshot)
  mirror_site <- NULL
  if (!is.null(site_dir)) {
    relative <- relative_to_root(site_dir, root)
    if (!fs::is_absolute_path(relative)) {
      mirror_site <- fs::path(mirror, relative)
    }
  }
  mirrored <- names(source_snapshot(mirror, mirror_site))
  mirrored <- fs::path(root, fs::path_rel(mirrored, start = mirror))
  unique(c(source, as.character(mirrored)))
}
