# Codebook missingness

An overview table of missingness patterns generated using
[`md_pattern()`](https://rubenarslan.github.io/codebook/reference/md_pattern.md).

## Usage

``` r
codebook_missingness(results, indent = "##")
```

## Arguments

- results:

  a data frame

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h2

## Examples

``` r
data("bfi")
codebook_missingness(bfi)
#> No viewer found, probably documenting or testing
#> 
#> 
#> 
#> 
#> ### Missingness report
#> 
#> <div data-pagedtable="false">
#>   <script data-pagedtable-source type="application/json">
#> {"columns":[{"label":["description"],"name":[1],"type":["chr"],"align":["left"]},{"label":["expired"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["var_miss"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["n_miss"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Missing values in 1 variables","2":"0","3":"1","4":"28"},{"1":"Missing values per variable","2":"28","3":"28","4":"28"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
#>   </script>
#> </div>
```
