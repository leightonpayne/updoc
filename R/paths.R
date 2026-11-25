package_root <- function(pkg = ".") {
  if (!is.character(pkg) || length(pkg) != 1L || is.na(pkg) || !nzchar(pkg)) {
    updoc_abort("`pkg` must be one non-empty path.")
  }
  path <- fs::path_abs(fs::path_expand(pkg))
  if (!fs::dir_exists(path) && fs::file_exists(path)) path <- fs::path_dir(path)
  if (!fs::dir_exists(path)) {
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
  fields <- tryCatch(read.dcf(fs::path(candidate, "DESCRIPTION")),
                     error = function(e) NULL)
  if (is.null(fields) || !"Package" %in% colnames(fields) ||
      !nzchar(fields[1L, "Package"])) {
    updoc_abort(sprintf("Invalid package DESCRIPTION: %s", candidate))
  }
  as.character(fs::path_real(candidate))
}

destination_override <- function(root, destination) {
  if (is.null(destination)) return(list())
  if (!is.character(destination) || length(destination) != 1L ||
      is.na(destination) || !nzchar(destination)) {
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
      updoc_abort(sprintf("Could not read pkgdown configuration for %s: %s",
                           root, conditionMessage(e)))
    }
  )
  site <- configured$dst_path
  if (is.null(site) || length(site) != 1L || !nzchar(site)) {
    site <- if (is.null(destination)) fs::path(root, "docs") else
      fs::path_abs(destination, start = root)
  }
  as.character(fs::path_abs(site))
}

prepare_initial_preview <- function(root, source_site, target) {
  source_site <- fs::path_abs(source_site)
  target <- fs::path_abs(target)
  reusable <- !identical(source_site, fs::path_abs(root)) &&
    fs::dir_exists(source_site) &&
    fs::file_exists(fs::path(source_site, "index.html"))

  if (reusable) {
    fs::dir_copy(source_site, target)
  } else {
    fs::dir_create(target)
  }
  invisible(reusable)
}

relative_to_root <- function(path, root) {
  path <- fs::path_abs(path)
  root <- fs::path_abs(root)
  prefix <- paste0(root, .Platform$file.sep)
  if (identical(path, root)) return("")
  if (!startsWith(path, prefix)) return(path)
  substring(path, nchar(prefix) + 1L)
}

is_excluded_path <- function(path, root, site_dir = NULL) {
  absolute <- fs::path_abs(path, start = root)
  rel <- gsub("\\\\", "/", relative_to_root(absolute, root))
  if (!is.null(site_dir)) {
    site_abs <- fs::path_abs(site_dir)
    if (identical(absolute, site_abs) ||
        startsWith(absolute, paste0(site_abs, .Platform$file.sep))) return(TRUE)
  }
  parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
  excluded_dir <- parts %in% c(".git", ".Rproj.user", "packrat", ".cache",
                                ".quarto", ".knitr", "node_modules")
  if (any(excluded_dir)) return(TRUE)
  if (length(parts) >= 2L && identical(parts[1L], "renv") &&
      identical(parts[2L], "library")) return(TRUE)
  if (any(grepl("\\.Rcheck$", parts))) return(TRUE)
  name <- basename(rel)
  grepl("(^#.*#$)|(~$)|(\\.sw[a-p]$)|(\\.tmp$)|(\\.temp$)|(\\.bak$)|(^\\.#[^/]+$)",
        name, ignore.case = TRUE)
}

is_relevant_path <- function(path, root, site_dir = NULL) {
  if (is_excluded_path(path, root, site_dir)) return(FALSE)
  rel <- gsub("\\\\", "/", relative_to_root(path, root))
  exact <- c("DESCRIPTION", "NAMESPACE", "_pkgdown.yml", "_pkgdown.yaml",
             "README.Rmd", "README.md", "index.md", "NEWS.md")
  dirs <- c("R", "man", "vignettes", "pkgdown", "inst", "data", "src")
  rel %in% exact || any(startsWith(rel, paste0(dirs, "/")))
}

watch_candidates <- function(root) {
  exact <- fs::path(root, c("DESCRIPTION", "NAMESPACE", "_pkgdown.yml",
                            "_pkgdown.yaml", "README.Rmd", "README.md",
                            "index.md", "NEWS.md"))
  dirs <- fs::path(root, c("R", "man", "vignettes", "pkgdown", "inst", "data", "src"))
  files <- exact[fs::file_exists(exact)]
  for (dir in dirs[fs::dir_exists(dirs)]) {
    files <- c(files, fs::dir_ls(dir, recurse = TRUE, type = "file",
                                 all = TRUE, fail = FALSE))
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
      "[.]knitr|node_modules)(/|$)"
    ),
    rel
  ) |
    grepl("(^|/)renv/library(/|$)", rel) |
    grepl("(^|/)[^/]+[.]Rcheck(/|$)", rel)
  names <- basename(rel)
  excluded <- excluded | grepl(
    "(^#.*#$)|(~$)|([.]sw[a-p]$)|([.]tmp$)|([.]temp$)|([.]bak$)|(^[.]#[^/]+$)",
    names,
    ignore.case = TRUE
  )
  if (!is.null(site_dir)) {
    site <- gsub(
      "\\\\", "/",
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
  changed <- vapply(all, function(path) {
    !identical(unname(old[path]), unname(new[path]))
  }, logical(1))
  all[changed]
}

mirror_package <- function(root, destination, site_dir = NULL) {
  fs::dir_create(destination)

  copy_directory <- function(source) {
    entries <- fs::dir_ls(source, all = TRUE, fail = FALSE)
    for (entry in entries) {
      if (is_excluded_path(entry, root, site_dir)) next
      relative <- relative_to_root(entry, root)
      target <- fs::path(destination, relative)
      if (fs::is_link(entry)) {
        if (fs::is_dir(entry)) {
          fs::dir_copy(entry, target)
        } else {
          fs::dir_create(fs::path_dir(target))
          fs::file_copy(entry, target, overwrite = TRUE)
        }
      } else if (fs::is_dir(entry)) {
        fs::dir_create(target)
        copy_directory(entry)
      } else {
        fs::dir_create(fs::path_dir(target))
        fs::file_copy(entry, target, overwrite = TRUE)
      }
    }
  }

  copy_directory(root)
  invisible(destination)
}

sync_package_changes <- function(root, mirror, changed) {
  changed <- unique(as.character(changed))
  for (source in changed) {
    relative <- relative_to_root(source, root)
    if (!nzchar(relative) || startsWith(relative, "<") ||
        !startsWith(fs::path_abs(source), paste0(root, .Platform$file.sep))) {
      next
    }
    target <- fs::path(mirror, relative)
    if (fs::file_exists(source)) {
      if (fs::dir_exists(target)) fs::dir_delete(target)
      fs::dir_create(fs::path_dir(target))
      fs::file_copy(source, target, overwrite = TRUE)
    } else if (fs::dir_exists(source)) {
      if (fs::dir_exists(target)) fs::dir_delete(target)
      if (fs::file_exists(target) || fs::is_link(target)) fs::file_delete(target)
      fs::dir_copy(source, target)
    } else if (fs::dir_exists(target)) {
      fs::dir_delete(target)
    } else if (fs::file_exists(target) || fs::is_link(target)) {
      fs::file_delete(target)
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
