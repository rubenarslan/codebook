# Compact Codebook

Generate only the tabular codebook and the machine-readable JSON-LD
metadata.

## Usage

``` r
compact_codebook(results)
```

## Arguments

- results:

  the data frame

## Examples

``` r
# will generate figures in a figure/ subdirectory
if (FALSE) { # \dontrun{
data("bfi")
bfi <- bfi[, c("BFIK_open_1", "BFIK_open_2")]
compact_codebook(bfi)
} # }
```
