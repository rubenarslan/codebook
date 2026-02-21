# Aggregate variables and remember which variables this were

The resulting variables will have the attribute `scale_item_names`
containing the basis for aggregation. Its `label` attribute will refer
to the common stem of the aggregated variable names (if any), the number
of variables, and the aggregation function.

## Usage

``` r
aggregate_and_document_scale(items, fun = rowMeans, stem = NULL)
```

## Arguments

- items:

  data.frame of the items that should be aggregated

- fun:

  aggregation function, defaults to rowMeans with na.rm = FALSE

- stem:

  common stem for the variables, specify if it should not be
  auto-detected as the longest common stem of the variable names

## Examples

``` r
testdf <- data.frame(bfi_neuro_1 = rnorm(20), bfi_neuro_2 = rnorm(20),
                    bfi_neuro_3R = rnorm(20), age = rpois(20, 30))
item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R')
testdf$bfi_neuro <- aggregate_and_document_scale(testdf[, item_names])
testdf$bfi_neuro
#>  [1] -1.263924018  0.160240530  0.984951589  1.268794692 -0.903711211
#>  [6]  0.104811152 -0.317597895 -1.235608530  0.414583429  0.187411304
#> [11]  0.700874388  0.003912358  0.218008596 -0.482393654 -1.568738280
#> [16]  0.783572677 -0.043890297  0.122196267  0.537511718 -0.313753439
#> attr(,"scale_item_names")
#> [1] "bfi_neuro_1"  "bfi_neuro_2"  "bfi_neuro_3R"
#> attr(,"label")
#> [1] "3 bfi_neuro items aggregated by rowMeans"
```
