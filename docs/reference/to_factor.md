# To factor

Convert a labelled vector to a factor, even if it doesn't have the
proper class, as long as it has the "labels" attribute. You can have
this attribute without, for example, the haven_labelled class, when
importing data with
[`rio::import()`](http://gesistsa.github.io/rio/reference/import.md) for
example.

## Usage

``` r
to_factor(x, ...)
```

## Arguments

- x:

  a vector

- ...:

  passed to
  [`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)

## Examples

``` r
example("labelled", "haven")
#> 
#> lablld> s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
#> 
#> lablld> s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))
#> 
#> lablld> s3 <- labelled(
#> lablld+   c(1, 1, 2),
#> lablld+   c(Male = 1, Female = 2),
#> lablld+   label = "Assigned sex at birth"
#> lablld+ )
#> 
#> lablld> # Unfortunately it's not possible to make as.factor work for labelled objects
#> lablld> # so instead use as_factor. This works for all types of labelled vectors.
#> lablld> as_factor(s1)
#> [1] Male   Male   Female
#> Levels: Female Male
#> 
#> lablld> as_factor(s1, levels = "values")
#> [1] M M F
#> Levels: M F
#> 
#> lablld> as_factor(s2)
#> [1] Male   Male   Female
#> Levels: Male Female
#> 
#> lablld> # Other statistical software supports multiple types of missing values
#> lablld> s3 <- labelled(
#> lablld+   c("M", "M", "F", "X", "N/A"),
#> lablld+   c(Male = "M", Female = "F", Refused = "X", "Not applicable" = "N/A")
#> lablld+ )
#> 
#> lablld> s3
#> <labelled<character>[5]>
#> [1] M   M   F   X   N/A
#> 
#> Labels:
#>  value          label
#>      M           Male
#>      F         Female
#>      X        Refused
#>    N/A Not applicable
#> 
#> lablld> as_factor(s3)
#> [1] Male           Male           Female         Refused        Not applicable
#> Levels: Female Male Not applicable Refused
#> 
#> lablld> # Often when you have a partially labelled numeric vector, labelled values
#> lablld> # are special types of missing. Use zap_labels to replace labels with missing
#> lablld> # values
#> lablld> x <- labelled(c(1, 2, 1, 2, 10, 9), c(Unknown = 9, Refused = 10))
#> 
#> lablld> zap_labels(x)
#> [1]  1  2  1  2 10  9
to_factor(x)
#> [1] 1       2       1       2       Refused Unknown
#> Levels: 1 2 Unknown Refused
to_factor(zap_labelled(x))
#> [1] 1       2       1       2       Refused Unknown
#> Levels: 1 2 Unknown Refused
to_factor(as_factor(x))
#> [1] 1       2       1       2       Refused Unknown
#> Levels: 1 2 Unknown Refused
to_factor(1:4)
#> [1] 1 2 3 4
#> Levels: 1 2 3 4
```
