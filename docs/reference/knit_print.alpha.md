# Pretty-print a Cronbach's alpha object

Turn a [`psych::alpha()`](https://rdrr.io/pkg/psych/man/alpha.html)
object into HTML tables.

## Usage

``` r
knit_print.alpha(x, indent = "######", ...)
```

## Arguments

- x:

  a psych alpha object

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h5

- ...:

  ignored

## Examples

``` r
example("alpha", "psych")
#> 
#> Attaching package: ‘psych’
#> The following object is masked _by_ ‘.GlobalEnv’:
#> 
#>     bfi
#> The following object is masked from ‘package:codebook’:
#> 
#>     bfi
#> 
#> alpha> set.seed(42) #keep the same starting values
#> 
#> alpha> #four congeneric measures
#> alpha> r4 <- sim.congeneric()
#> 
#> alpha> alpha(r4)
#> 
#> Reliability analysis   
#> Call: alpha(x = r4)
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N median_r
#>       0.74      0.74    0.69      0.42 2.9     0.41
#> 
#>     95% confidence boundaries 
#>       lower alpha upper
#> Feldt -0.31  0.74  0.98
#> 
#>  Reliability if an item is dropped:
#>    raw_alpha std.alpha G6(smc) average_r S/N  var.r med.r
#> V1      0.62      0.62    0.53      0.36 1.7 0.0036  0.35
#> V2      0.66      0.66    0.57      0.39 1.9 0.0081  0.40
#> V3      0.70      0.70    0.62      0.44 2.3 0.0120  0.40
#> V4      0.74      0.74    0.66      0.49 2.8 0.0049  0.48
#> 
#>  Item statistics 
#>       r r.cor r.drop
#> V1 0.81  0.74   0.64
#> V2 0.78  0.67   0.57
#> V3 0.73  0.59   0.51
#> V4 0.68  0.50   0.43
#> 
#> alpha> #nine hierarchical measures -- should actually use omega
#> alpha> r9 <- sim.hierarchical()
#> 
#> alpha> alpha(r9)
#> 
#> Reliability analysis   
#> Call: alpha(x = r9)
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N median_r
#>       0.76      0.76    0.76      0.26 3.2     0.25
#> 
#>     95% confidence boundaries 
#>       lower alpha upper
#> Feldt  0.43  0.76  0.94
#> 
#>  Reliability if an item is dropped:
#>    raw_alpha std.alpha G6(smc) average_r S/N  var.r med.r
#> V1      0.71      0.71    0.70      0.24 2.5 0.0067  0.22
#> V2      0.72      0.72    0.71      0.25 2.6 0.0085  0.23
#> V3      0.74      0.74    0.73      0.26 2.8 0.0101  0.25
#> V4      0.73      0.73    0.72      0.25 2.7 0.0106  0.23
#> V5      0.74      0.74    0.73      0.26 2.9 0.0112  0.24
#> V6      0.75      0.75    0.74      0.27 3.0 0.0113  0.25
#> V7      0.75      0.75    0.74      0.27 3.0 0.0129  0.25
#> V8      0.76      0.76    0.75      0.28 3.1 0.0118  0.26
#> V9      0.77      0.77    0.76      0.29 3.3 0.0099  0.28
#> 
#>  Item statistics 
#>       r r.cor r.drop
#> V1 0.72  0.71   0.61
#> V2 0.67  0.63   0.54
#> V3 0.61  0.55   0.47
#> V4 0.65  0.59   0.51
#> V5 0.59  0.52   0.45
#> V6 0.53  0.43   0.38
#> V7 0.56  0.46   0.40
#> V8 0.50  0.39   0.34
#> V9 0.45  0.32   0.28
#> 
#> alpha> # examples of two independent factors that produce reasonable alphas
#> alpha> #this is a case where alpha is a poor indicator of unidimensionality
#> alpha> 
#> alpha> two.f <- sim.item(8)$item
#> 
#> alpha> #specify which items to reverse key by name
#> alpha>  alpha(two.f,keys=c("V3","V4","V5","V6"))
#> Number of categories should be increased  in order to count frequencies. 
#> 
#> Reliability analysis   
#> Call: alpha(x = two.f, keys = c("V3", "V4", "V5", "V6"))
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N   ase  mean   sd median_r
#>       0.58      0.58    0.62      0.15 1.4 0.029 0.072 0.51    0.051
#> 
#>     95% confidence boundaries 
#>          lower alpha upper
#> Feldt     0.52  0.58  0.63
#> Duhachek  0.53  0.58  0.64
#> 
#>  Reliability if an item is dropped:
#>     raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
#> V1       0.55      0.55    0.58      0.15 1.2    0.031 0.032 0.056
#> V2       0.53      0.53    0.57      0.14 1.1    0.033 0.034 0.029
#> V3-      0.55      0.55    0.58      0.15 1.2    0.031 0.031 0.056
#> V4-      0.55      0.54    0.58      0.15 1.2    0.032 0.032 0.056
#> V5-      0.56      0.56    0.59      0.15 1.3    0.031 0.030 0.056
#> V6-      0.56      0.56    0.59      0.15 1.3    0.031 0.030 0.056
#> V7       0.53      0.53    0.56      0.14 1.1    0.033 0.031 0.041
#> V8       0.56      0.57    0.59      0.16 1.3    0.030 0.030 0.047
#> 
#>  Item statistics 
#>       n raw.r std.r r.cor r.drop    mean   sd
#> V1  500  0.50  0.50  0.38   0.28  0.0117 1.01
#> V2  500  0.55  0.55  0.46   0.34 -0.0018 1.00
#> V3- 500  0.50  0.50  0.39   0.27  0.1443 1.05
#> V4- 500  0.51  0.51  0.41   0.30  0.1502 0.99
#> V5- 500  0.48  0.48  0.36   0.26  0.1030 1.01
#> V6- 500  0.48  0.48  0.36   0.26  0.1128 1.00
#> V7  500  0.56  0.56  0.48   0.35  0.0222 1.00
#> V8  500  0.45  0.46  0.33   0.23  0.0320 0.96
#> 
#> alpha>  cov.two <- cov(two.f)
#> 
#> alpha>  alpha(cov.two,check.keys=TRUE)
#> Warning: Some items were negatively correlated with the first principal component and were automatically reversed.
#>  This is indicated by a negative sign for the variable name.
#> 
#> Reliability analysis   
#> Call: alpha(x = cov.two, check.keys = TRUE)
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N median_r
#>       0.58      0.58    0.62      0.15 1.4    0.051
#> 
#>     95% confidence boundaries 
#>       lower alpha upper
#> Feldt -0.07  0.58   0.9
#> 
#>  Reliability if an item is dropped:
#>     raw_alpha std.alpha G6(smc) average_r S/N var.r med.r
#> V1       0.55      0.55    0.58      0.15 1.2 0.032 0.056
#> V2       0.53      0.53    0.57      0.14 1.1 0.034 0.029
#> V3-      0.55      0.55    0.58      0.15 1.2 0.031 0.056
#> V4-      0.55      0.54    0.58      0.15 1.2 0.032 0.056
#> V5-      0.56      0.56    0.59      0.15 1.3 0.030 0.056
#> V6-      0.56      0.56    0.59      0.15 1.3 0.030 0.056
#> V7       0.53      0.53    0.56      0.14 1.1 0.031 0.041
#> V8       0.56      0.57    0.59      0.16 1.3 0.030 0.047
#> 
#>  Item statistics 
#>        r r.cor r.drop
#> V1  0.50  0.38   0.28
#> V2  0.55  0.46   0.34
#> V3- 0.50  0.39   0.27
#> V4- 0.51  0.41   0.30
#> V5- 0.48  0.36   0.26
#> V6- 0.48  0.36   0.26
#> V7  0.56  0.48   0.35
#> V8  0.46  0.33   0.23
#> 
#> alpha>  #automatic reversal base upon first component
#> alpha> alpha(two.f,check.keys=TRUE)    #note that the median is much less than the average R
#> Number of categories should be increased  in order to count frequencies. 
#> Warning: Some items were negatively correlated with the first principal component and were automatically reversed.
#>  This is indicated by a negative sign for the variable name.
#> 
#> Reliability analysis   
#> Call: alpha(x = two.f, check.keys = TRUE)
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N   ase  mean   sd median_r
#>       0.58      0.58    0.62      0.15 1.4 0.029 0.072 0.51    0.051
#> 
#>     95% confidence boundaries 
#>          lower alpha upper
#> Feldt     0.52  0.58  0.63
#> Duhachek  0.53  0.58  0.64
#> 
#>  Reliability if an item is dropped:
#>     raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
#> V1       0.55      0.55    0.58      0.15 1.2    0.031 0.032 0.056
#> V2       0.53      0.53    0.57      0.14 1.1    0.033 0.034 0.029
#> V3-      0.55      0.55    0.58      0.15 1.2    0.031 0.031 0.056
#> V4-      0.55      0.54    0.58      0.15 1.2    0.032 0.032 0.056
#> V5-      0.56      0.56    0.59      0.15 1.3    0.031 0.030 0.056
#> V6-      0.56      0.56    0.59      0.15 1.3    0.031 0.030 0.056
#> V7       0.53      0.53    0.56      0.14 1.1    0.033 0.031 0.041
#> V8       0.56      0.57    0.59      0.16 1.3    0.030 0.030 0.047
#> 
#>  Item statistics 
#>       n raw.r std.r r.cor r.drop    mean   sd
#> V1  500  0.50  0.50  0.38   0.28  0.0117 1.01
#> V2  500  0.55  0.55  0.46   0.34 -0.0018 1.00
#> V3- 500  0.50  0.50  0.39   0.27  0.1443 1.05
#> V4- 500  0.51  0.51  0.41   0.30  0.1502 0.99
#> V5- 500  0.48  0.48  0.36   0.26  0.1030 1.01
#> V6- 500  0.48  0.48  0.36   0.26  0.1128 1.00
#> V7  500  0.56  0.56  0.48   0.35  0.0222 1.00
#> V8  500  0.45  0.46  0.33   0.23  0.0320 0.96
#> 
#> alpha> #this suggests (correctly) that the 1 factor model is probably wrong 
#> alpha> #an example with discrete item responses  -- show the frequencies
#> alpha> items <- sim.congeneric(N=500,short=FALSE,low=-2,high=2,
#> alpha+         categorical=TRUE) #500 responses to 4 discrete items with 5 categories
#> 
#> alpha> a4 <- alpha(items$observed)  #item response analysis of congeneric measures
#> 
#> alpha> a4
#> 
#> Reliability analysis   
#> Call: alpha(x = items$observed)
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N  ase   mean   sd median_r
#>       0.73      0.73    0.68       0.4 2.7 0.02 -0.013 0.76      0.4
#> 
#>     95% confidence boundaries 
#>          lower alpha upper
#> Feldt     0.69  0.73  0.76
#> Duhachek  0.69  0.73  0.77
#> 
#>  Reliability if an item is dropped:
#>    raw_alpha std.alpha G6(smc) average_r S/N alpha se  var.r med.r
#> V1      0.61      0.61    0.52      0.34 1.6    0.031 0.0063  0.32
#> V2      0.64      0.64    0.55      0.37 1.8    0.028 0.0097  0.37
#> V3      0.68      0.68    0.60      0.41 2.1    0.025 0.0134  0.37
#> V4      0.73      0.73    0.65      0.48 2.8    0.021 0.0036  0.47
#> 
#>  Item statistics 
#>      n raw.r std.r r.cor r.drop   mean   sd
#> V1 500  0.80  0.80  0.73   0.62  0.050 1.00
#> V2 500  0.77  0.77  0.67   0.57 -0.022 1.03
#> V3 500  0.72  0.73  0.58   0.50 -0.028 0.99
#> V4 500  0.67  0.66  0.46   0.40 -0.050 1.05
#> 
#> Non missing response frequency for each item
#>      -2   -1    0    1    2 miss
#> V1 0.06 0.24 0.38 0.25 0.07    0
#> V2 0.07 0.26 0.35 0.25 0.07    0
#> V3 0.05 0.27 0.38 0.22 0.07    0
#> V4 0.10 0.22 0.39 0.22 0.07    0
#> 
#> alpha> #summary just gives Alpha
#> alpha> summary(a4)
#> 
#> Reliability analysis   
#>  raw_alpha std.alpha G6(smc) average_r S/N  ase   mean   sd median_r
#>       0.73      0.73    0.68       0.4 2.7 0.02 -0.013 0.76      0.4
#> 
#> alpha> alpha2r(alpha = .74,n.var=4)
#> [1] 0.4157303
#> 
#> alpha> #because alpha.ci returns an invisible object, you need to print it
#> alpha> print(alpha.ci(.74, 100,p.val=.05,n.var=4))
#> 
#>    95% confidence boundaries (Feldt)
#>  lower alpha upper
#>   0.65  0.74  0.81
knitr::knit_print(a4)
#> 
#> Reliability analysis   
#> Call: alpha(x = items$observed)
#> 
#>   raw_alpha std.alpha G6(smc) average_r S/N  ase   mean   sd median_r
#>       0.73      0.73    0.68       0.4 2.7 0.02 -0.013 0.76      0.4
#> 
#>     95% confidence boundaries 
#>          lower alpha upper
#> Feldt     0.69  0.73  0.76
#> Duhachek  0.69  0.73  0.77
#> 
#>  Reliability if an item is dropped:
#>    raw_alpha std.alpha G6(smc) average_r S/N alpha se  var.r med.r
#> V1      0.61      0.61    0.52      0.34 1.6    0.031 0.0063  0.32
#> V2      0.64      0.64    0.55      0.37 1.8    0.028 0.0097  0.37
#> V3      0.68      0.68    0.60      0.41 2.1    0.025 0.0134  0.37
#> V4      0.73      0.73    0.65      0.48 2.8    0.021 0.0036  0.47
#> 
#>  Item statistics 
#>      n raw.r std.r r.cor r.drop   mean   sd
#> V1 500  0.80  0.80  0.73   0.62  0.050 1.00
#> V2 500  0.77  0.77  0.67   0.57 -0.022 1.03
#> V3 500  0.72  0.73  0.58   0.50 -0.028 0.99
#> V4 500  0.67  0.66  0.46   0.40 -0.050 1.05
#> 
#> Non missing response frequency for each item
#>      -2   -1    0    1    2 miss
#> V1 0.06 0.24 0.38 0.25 0.07    0
#> V2 0.07 0.26 0.35 0.25 0.07    0
#> V3 0.05 0.27 0.38 0.22 0.07    0
#> V4 0.10 0.22 0.39 0.22 0.07    0
```
