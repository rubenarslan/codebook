# Data description default

If you do not define a dataset description yourself, this will be the
automatically generated default.

## Usage

``` r
data_description_default(data)
```

## Arguments

- data:

  the data frame

## Examples

``` r
data('bfi')
data_description_default(bfi)
#> The dataset has N=28 rows and 29 columns.
#> 0 rows have no missing values on any column.
```
