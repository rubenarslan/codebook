# Briefly summarise available reliability results

One-line summary

## Usage

``` r
pull_reliability(rels)
```

## Arguments

- rels:

  the result of a call to compute_reliabilities

## Examples

``` r
if (FALSE) { # \dontrun{
data("bfi", package = "codebook")
bfi <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_agree"))
reliabilities <- compute_reliabilities(bfi)
pull_reliability(reliabilities$BFIK_agree)
} # }
```
