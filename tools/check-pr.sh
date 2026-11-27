#!/usr/bin/env bash
set -euo pipefail

# Canonical pre-PR verification for humans and agents.
# Usage:
#   ./tools/check-pr.sh          # fast checks (Air format, assets, browser, roxygen drift, unit tests)
#   ./tools/check-pr.sh --full   # fast checks + integration tests + R CMD check --as-cran

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

FULL=false
for arg in "$@"; do
  case "$arg" in
    --full) FULL=true ;;
    -h|--help)
      sed -n '3,7p' "$0"
      exit 0
      ;;
    *)
      echo "Unknown option: $arg" >&2
      exit 2
      ;;
  esac
done

echo "==> R formatting (Air)"
if ! command -v air >/dev/null 2>&1; then
  echo "air is not installed or not on PATH." >&2
  exit 1
fi
air format --check .

echo "==> Browser regression tests"
node --test tests/browser/*.test.cjs

echo "==> Generated mascot and preview assets"
node tools/generate-doc-assets.cjs --check

echo "==> Roxygen drift check (non-mutating)"
Rscript -e '
tmp <- tempfile("updoc-roxygen-")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
pkg_files <- c("DESCRIPTION", "NAMESPACE", "R", "man")
for (item in pkg_files) {
  if (!file.exists(item)) next
  if (dir.exists(item)) {
    fs::dir_copy(item, file.path(tmp, item))
  } else {
    fs::file_copy(item, file.path(tmp, item))
  }
}
suppressMessages(roxygen2::roxygenise(tmp, roclets = c("collate", "namespace", "rd")))
check_diff <- function(path) {
  status <- system2("diff", c("-ru", path, file.path(tmp, path)))
  if (!identical(status, 0L)) {
    stop(sprintf("Roxygen output differs for %s; run roxygen2::roxygenise().", path), call. = FALSE)
  }
}
check_diff("NAMESPACE")
check_diff("man")
'

if $FULL; then
  echo "==> R unit and integration tests"
  UPDOC_INTEGRATION_TESTS=true Rscript -e '
    res <- testthat::test_local()
    df <- as.data.frame(res)
    stopifnot(sum(df$failed) == 0L, sum(df$warning) == 0L, sum(df$error) == 0L)
  '

  echo "==> R CMD check --as-cran"
  Rscript -e '
    rcmdcheck::rcmdcheck(
      args = c("--no-manual", "--as-cran"),
      build_args = "--no-manual",
      error_on = "warning"
    )
  '
else
  echo "==> R unit tests"
  Rscript -e '
    res <- testthat::test_local()
    df <- as.data.frame(res)
    stopifnot(sum(df$failed) == 0L, sum(df$warning) == 0L, sum(df$error) == 0L)
  '
fi

echo "==> All requested checks passed"
