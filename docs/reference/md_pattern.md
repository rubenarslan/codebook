# Missing data patterns

Generate missingness patterns using a function borrowed from mice, with
options to reduce the complexity of the output.

## Usage

``` r
md_pattern(data, omit_complete = TRUE, min_freq = 0.01)
```

## Arguments

- data:

  the dataset

- omit_complete:

  defaults to TRUE, omitting variables without missing values

- min_freq:

  minimum number of rows to have this missingness pattern

## Examples

``` r
data("bfi", package = 'psych')
md_pattern(bfi)
#> # A tibble: 4 × 28
#>   description     E4    N3    O4    A1    A5    C5    E2    A4    C3    O5    C1
#>   <chr>        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Missing val…     1     1     1     1     1     1     1     1     1     1     1
#> 2 Missing val…     9    11    14    16    16    16    16    19    20    20    21
#> 3 Missing val…     1     1     1     1     1     1     1     1     1     1     1
#> 4 99 other, l…    95    92    93    90    93    93    92    87    87    88    93
#> # ℹ 16 more variables: E5 <dbl>, N2 <dbl>, N1 <dbl>, O1 <dbl>, E1 <dbl>,
#> #   C2 <dbl>, E3 <dbl>, A3 <dbl>, C4 <dbl>, A2 <dbl>, O3 <dbl>, N5 <dbl>,
#> #   N4 <dbl>, education <dbl>, var_miss <dbl>, n_miss <dbl>
md_pattern(bfi, omit_complete = FALSE, min_freq = 0.2)
#> # A tibble: 3 × 31
#>   description    O2 gender   age    E4    N3    O4    A1    A5    C5    E2    A4
#>   <chr>       <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Missing va…     1      1     1     1     1     1     1     1     1     1     1
#> 2 Missing va…     0      0     0     9    11    14    16    16    16    16    19
#> 3 100 other,…   100    100   100    96    93    94    91    94    94    93    88
#> # ℹ 19 more variables: C3 <dbl>, O5 <dbl>, C1 <dbl>, E5 <dbl>, N2 <dbl>,
#> #   N1 <dbl>, O1 <dbl>, E1 <dbl>, C2 <dbl>, E3 <dbl>, A3 <dbl>, C4 <dbl>,
#> #   A2 <dbl>, O3 <dbl>, N5 <dbl>, N4 <dbl>, education <dbl>, var_miss <dbl>,
#> #   n_miss <dbl>
```
