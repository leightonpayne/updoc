# What's updoc?

<link rel="stylesheet" href="doc-assets/doc.css">

<div class="doc-intro">
<div class="doc-intro-states" aria-label="Doc's build states">
<figure><span data-doc-intro-state="ready"></span></figure>
<figure class="doc-intro-warning"><span data-doc-intro-state="warning"></span></figure>
<figure class="doc-intro-error"><span data-doc-intro-state="error"></span></figure>
<script src="doc-assets/doc.js"></script>
<script src="doc-assets/homepage.js"></script>
</div>
</div>

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

Your site opens in the browser. Edit your files and save as usual: updoc rebuilds only what changed, leaves your package's generated files untouched while previewing, and keeps the last working preview up if something breaks. Press Ctrl+C when you're done.

## Learn more

- [Get started](articles/get-started.html): walk through live previews, keeping your R console free, checking your site, and building to publish.
- [Options](articles/configuration.html): tune rebuild behavior, popup theme, Pandoc path, ports, and automation for CI or AI agents.
- [Function reference](reference/index.html): full details on `serve()`, `preview()`, `check()`, and `build()`.
