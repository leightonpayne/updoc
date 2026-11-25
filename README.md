# updoc

<p align="center">
  <picture>
    <source media="(prefers-reduced-motion: reduce)" srcset="man/figures/readme-mascots-still.svg">
    <source media="(prefers-color-scheme: dark)" srcset="man/figures/readme-mascots-dark.svg">
    <img src="man/figures/readme-mascots-light.svg" width="224" height="80" alt="Doc, updoc's mascot, in ready, warning, and error states">
  </picture>
</p>

Live previews for your pkgdown site. Edit your package, hit save, and updoc rebuilds what changed and refreshes your browser.

## Installation

Install updoc from GitHub:

```r
# install.packages("pak")
pak::pak("leightonpayne/updoc")
```

You'll need Pandoc 2.10.1 or newer installed. To point updoc at a specific copy, pass `pandoc = "/path/to/bin"`.

## Quick start

From inside your R package, run:

```r
updoc::serve()
```

Your site opens in the browser. Edit your R files, vignettes, README, or `_pkgdown.yml` as usual and hit save. updoc rebuilds only what changed and leaves your package's generated files untouched while previewing. If a build breaks, your last working preview stays up while you fix it. Press Ctrl+C when you're done.

Want to keep using the R console while the preview runs? Start it in the background instead:

```r
site <- updoc::preview()
site$wait()
site$stop()
```

For a one-shot check without touching any files in your package (handy in CI or AI agent loops), run:

```r
updoc::check()
```

When you're ready to build the real site to publish, run:

```r
updoc::build()
```
