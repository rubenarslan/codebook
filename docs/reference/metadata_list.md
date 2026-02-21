# Metadata from dataframe

Returns a list containing variable metadata (attributes) and data
summaries.

## Usage

``` r
metadata_list(results, only_existing = TRUE)
```

## Arguments

- results:

  a data frame, ideally with attributes set on variables

- only_existing:

  whether to drop helpful metadata to comply with the list of currently
  defined schema.org properties

## Examples

``` r
data("bfi")
md_list <- metadata_list(bfi)
md_list$variableMeasured[[20]]
#> $name
#> [1] "N5"
#> 
#> $`@type`
#> [1] "propertyValue"
#> 
```
