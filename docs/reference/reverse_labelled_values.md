# Reverse labelled values reverse the underlying values for a numeric [`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html) vector while keeping the labels correct

Reverse labelled values reverse the underlying values for a numeric
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
vector while keeping the labels correct

Reverse labelled values reverse the underlying values for a numeric
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
vector while keeping the labels correct

## Usage

``` r
reverse_labelled_values(x)

reverse_labelled_values(x)
```

## Arguments

- x:

  a labelled vector

## Value

return the labelled vector with the underlying values having been
reversed

return the labelled vector with the underlying values having been
reversed

## Examples

``` r
x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
x
#> <labelled<integer>[9]>
#> [1] 1 1 1 2 2 2 3 3 3
#> 
#> Labels:
#>  value label
#>      1   Bad
#>      5  Good
reverse_labelled_values(x)
#> <labelled<integer>[9]>
#> [1] 5 5 5 4 4 4 3 3 3
#> 
#> Labels:
#>  value label
#>      5   Bad
#>      1  Good
x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
x
#> <labelled<integer>[9]>
#> [1] 1 1 1 2 2 2 3 3 3
#> 
#> Labels:
#>  value label
#>      1   Bad
#>      5  Good
reverse_labelled_values(x)
#> <labelled<integer>[9]>
#> [1] 5 5 5 4 4 4 3 3 3
#> 
#> Labels:
#>  value label
#>      5   Bad
#>      1  Good
```
