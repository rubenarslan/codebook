# Tabular codebook

Renders a tabular codebook including attributes and data summaries. The
table is generated using
[`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) and can
be exported to CSV, Excel, etc.

## Usage

``` r
codebook_items(results, indent = "##")
```

## Arguments

- results:

  a data frame, ideally with attributes set on variables

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h2

## Examples

``` r
data("bfi")
if (FALSE) { # \dontrun{
# doesn't show interactively, because a html widget needs to be registered
codebook_items(bfi)
} # }
```
