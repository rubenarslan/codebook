# Has labels

Has labels

## Usage

``` r
has_labels(x)
```

## Arguments

- x:

  a vector

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
has_labels(x)
#> [1] TRUE
```
