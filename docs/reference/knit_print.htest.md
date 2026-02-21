# Print a [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) object for knitr

Just prints the normal output of
[`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html).

## Usage

``` r
knit_print.htest(x, ...)
```

## Arguments

- x:

  a psych alpha object

- ...:

  ignored

## Examples

``` r
knitr::knit_print(cor.test(rnorm(100), rnorm(100)))
#> No viewer found, probably documenting or testing
#> 
#> 
#> 
#> ```
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  rnorm(100) and rnorm(100)
#> t = -1.2779, df = 98, p-value = 0.2043
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.31648544  0.07015646
#> sample estimates:
#>        cor 
#> -0.1280259 
#> 
#> ```
#> 
#> 
```
