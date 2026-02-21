# Browse and search codebook

Usable as an Addin in RStudio. You can select it from a menu at the top,
when this package is installed. If you're currently selecting the name
of a data frame in your source code, this will be the dataset shown by
default. If you don't select text, you can pick a dataset from a
dropdown. You can add a keyboard shortcut for this command by following
the
[instructions](https://docs.posit.co/ide/user/ide/guide/productivity/custom-shortcuts.html)
by RStudio. How about Cmd+Ctrl+C?

## Usage

``` r
codebook_browser(
  data = NULL,
  labels_only = FALSE,
  title = "Codebook metadata",
  viewer = rstudioapi::viewer
)
```

## Arguments

- data:

  the dataset to display. If left empty will try to use selected text in
  RStudio or offer a dropdown

- labels_only:

  defaults to false called with TRUE from
  [`label_browser()`](https://rubenarslan.github.io/codebook/reference/label_browser.md)

- title:

  title of the gadget

- viewer:

  defaults to displaying in the RStudio viewer
