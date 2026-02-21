# Append R to string, if it doesn't end in R already.

Use this function to conveniently rename reverse-coded variables, so
that they end in R.

## Usage

``` r
add_R(x)
```

## Arguments

- x:

  a string

## Examples

``` r
data('bfi')
bfi %>% dplyr::select(BFIK_open_2,BFIK_agree_2) %>% dplyr::rename_at(1, add_R) %>% head()
#>   BFIK_open_2R BFIK_agree_2
#> 1            5            5
#> 2            4            4
#> 3            5            5
#> 4            4            4
#> 5            4            4
#> 6            4            4
```
