# Print a [`psych::multilevel.reliability()`](https://rdrr.io/pkg/psych/man/multilevel.reliability.html) object for knitr

Just prints the normal output of
[`psych::multilevel.reliability()`](https://rdrr.io/pkg/psych/man/multilevel.reliability.html).

## Usage

``` r
knit_print.multilevel(x, ...)
```

## Arguments

- x:

  a psych alpha object

- ...:

  ignored

## Examples

``` r
example("mlr", "psych")
#> 
#> mlr> #data from Shrout and Lane, 2012.
#> mlr> 
#> mlr> shrout <- structure(list(Person = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 
#> mlr+ 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L), Time = c(1L, 1L, 
#> mlr+ 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 
#> mlr+ 4L, 4L), Item1 = c(2L, 3L, 6L, 3L, 7L, 3L, 5L, 6L, 3L, 8L, 4L, 
#> mlr+ 4L, 7L, 5L, 6L, 1L, 5L, 8L, 8L, 6L), Item2 = c(3L, 4L, 6L, 4L, 
#> mlr+ 8L, 3L, 7L, 7L, 5L, 8L, 2L, 6L, 8L, 6L, 7L, 3L, 9L, 9L, 7L, 8L
#> mlr+ ), Item3 = c(6L, 4L, 5L, 3L, 7L, 4L, 7L, 8L, 9L, 9L, 5L, 7L, 
#> mlr+ 9L, 7L, 8L, 4L, 7L, 9L, 9L, 6L)), .Names = c("Person", "Time", 
#> mlr+ "Item1", "Item2", "Item3"), class = "data.frame", row.names = c(NA, 
#> mlr+ -20L))
#> 
#> mlr> #make shrout super wide
#> mlr> #Xwide <- reshape(shrout,v.names=c("Item1","Item2","Item3"),timevar="Time", 
#> mlr> #direction="wide",idvar="Person")
#> mlr> #add more helpful Names
#> mlr> #colnames(Xwide ) <- c("Person",c(paste0("Item",1:3,".T",1),paste0("Item",1:3,".T",2), 
#> mlr> #paste0("Item",1:3,".T",3),paste0("Item",1:3,".T",4)))
#> mlr> #make superwide into normal form  (i.e., just return it to the original shrout data
#> mlr> #Xlong <-Xlong <- reshape(Xwide,idvar="Person",2:13)
#> mlr> 
#> mlr> #Now use these data for a multilevel repliability study, use the normal wide form output
#> mlr> mg <- mlr(shrout,grp="Person",Time="Time",items=3:5) 
#> 
#> mlr> #which is the same as 
#> mlr> #mg <- multilevel.reliability(shrout,grp="Person",Time="Time",items=
#> mlr> #         c("Item1","Item2","Item3"),plot=TRUE)
#> mlr> #to show the lattice plot by subjects, set plot = TRUE
#> mlr> 
#> mlr> #Alternatively for long input (returned in this case from the prior run)
#> mlr> mlr(mg$long,grp="id",Time ="time",items="items", values="values",long=TRUE)
#> 
#> Multilevel Generalizability analysis   
#> Call: mlr(x = mg$long, grp = "id", Time = "time", items = "items", 
#>     long = TRUE, values = "values")
#> 
#> The data had  5  observations taken over  4  time intervals for  3 items.
#> 
#>  Alternative estimates of reliability based upon Generalizability theory
#> 
#> RkF  =  0.97 Reliability of average of all ratings across all items and  times (Fixed time effects)
#> R1R  =  0.6 Generalizability of a single time point across all items (Random time effects)
#> RkR  =  0.85 Generalizability of average time points across all items (Random time effects)
#> Rc   =  0.74 Generalizability of change (fixed time points, fixed items) 
#> RkRn =  0.85 Generalizability of between person differences averaged over time (time nested within people)
#> Rcn  =  0.65 Generalizability of within person variations averaged over items  (time nested within people)
#> 
#>  These reliabilities are derived from the components of variance estimated by ANOVA 
#>              variance Percent
#> ID               2.34    0.44
#> Time             0.38    0.07
#> Items            0.61    0.11
#> ID x time        0.92    0.17
#> ID x items       0.12    0.02
#> time x items     0.05    0.01
#> Residual         0.96    0.18
#> Total            5.38    1.00
#> 
#>  The nested components of variance estimated from lme are:
#>          variance Percent
#> id            2.3    0.45
#> id(time)      1.1    0.21
#> residual      1.7    0.34
#> total         5.1    1.00
#> 
#> To see the ANOVA and alpha by subject, use the short = FALSE option.
#>  To see the summaries of the ICCs by subject and time, use all=TRUE
#>  To see specific objects select from the following list:
#>  ANOVA s.lmer s.lme alpha summary.by.person summary.by.time ICC.by.person ICC.by.time lmer long Call
#> mlr> #example of mlArrange
#> mlr> #First, add two new columns to shrout and 
#> mlr> #then convert to long output using mlArrange
#> mlr> total <- rowSums(shrout[3:5])
#> 
#> mlr> caseid <- rep(paste0("ID",1:5),4)
#> 
#> mlr> new.shrout <- cbind(shrout,total=total,case=caseid)
#> 
#> mlr> #now convert to long
#> mlr> new.long <- mlArrange(new.shrout,grp="Person",Time="Time",items =3:5,extra=6:7)
#> 
#> mlr> headTail(new.long,6,6)
#>      id time values items total case
#> 1     1    1      2 Item1    11  ID1
#> 2     1    2      3 Item1    10  ID1
#> 3     1    3      4 Item1    11  ID1
#> 4     1    4      1 Item1     8  ID1
#> 5     1    1      3 Item2    11  ID1
#> 6     1    2      3 Item2    10  ID1
#> ... ...  ...    ...  <NA>   ... <NA>
#> 55    5    3      7 Item2    21  ID5
#> 56    5    4      8 Item2    20  ID5
#> 57    5    1      7 Item3    22  ID5
#> 58    5    2      9 Item3    25  ID5
#> 59    5    3      8 Item3    21  ID5
#> 60    5    4      6 Item3    20  ID5
knitr::knit_print(mg)
#> No viewer found, probably documenting or testing
#> 
#> 
#> 
#> ```
#> 
#> Multilevel Generalizability analysis   
#> Call: mlr(x = shrout, grp = "Person", Time = "Time", items = 3:5)
#> 
#> The data had  5  observations taken over  4  time intervals for  3 items.
#> 
#>  Alternative estimates of reliability based upon Generalizability theory
#> 
#> RkF  =  0.97 Reliability of average of all ratings across all items and  times (Fixed time effects)
#> R1R  =  0.6 Generalizability of a single time point across all items (Random time effects)
#> RkR  =  0.85 Generalizability of average time points across all items (Random time effects)
#> Rc   =  0.74 Generalizability of change (fixed time points, fixed items) 
#> RkRn =  0.85 Generalizability of between person differences averaged over time (time nested within people)
#> Rcn  =  0.65 Generalizability of within person variations averaged over items  (time nested within people)
#> 
#>  These reliabilities are derived from the components of variance estimated by ANOVA 
#>              variance Percent
#> ID               2.34    0.44
#> Time             0.38    0.07
#> Items            0.61    0.11
#> ID x time        0.92    0.17
#> ID x items       0.12    0.02
#> time x items     0.05    0.01
#> Residual         0.96    0.18
#> Total            5.38    1.00
#> 
#>  The nested components of variance estimated from lme are:
#>          variance Percent
#> id            2.3    0.45
#> id(time)      1.1    0.21
#> residual      1.7    0.34
#> total         5.1    1.00
#> 
#> To see the ANOVA and alpha by subject, use the short = FALSE option.
#>  To see the summaries of the ICCs by subject and time, use all=TRUE
#>  To see specific objects select from the following list:
#>  ANOVA s.lmer s.lme alpha summary.by.person summary.by.time ICC.by.person ICC.by.time lmer long Call
#> ```
#> 
#> 
```
