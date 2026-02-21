# Browse and search variable and value labels

Same as the
[`codebook_browser()`](https://rubenarslan.github.io/codebook/reference/codebook_browser.md),
but doesn't show data summaries and additional attributes.

## Usage

``` r
label_browser(data = NULL, viewer = rstudioapi::viewer)
```

## Arguments

- data:

  the dataset to display. If left empty will try to use selected text in
  RStudio or offer a dropdown

- viewer:

  defaults to displaying in the RStudio viewer
