# Skim codebook

Implements the regular functionality of
[`skimr::skim()`](https://docs.ropensci.org/skimr/reference/skim.html)
but renames the columns p0, p50, and p100 to min, median, and max
respectively for numeric types to keep things consistent across type
(and produce a narrower wide table).

## Usage

``` r
skim_codebook(data, ...)
```

## Arguments

- data:

  the dataset to skim

- ...:

  passed to
  [`skimr::skim()`](https://docs.ropensci.org/skimr/reference/skim.html)

## Examples

``` r
skim_codebook(bfi)
#> ── Data Summary ────────────────────────
#>                            Values
#> Name                       data  
#> Number of rows             28    
#> Number of columns          29    
#> _______________________          
#> Column type frequency:           
#>   POSIXct                  3     
#>   character                1     
#>   haven_labelled           19    
#>   logical                  1     
#>   numeric                  5     
#> ________________________         
#> Group variables            None  
#> 
#> ── Variable type: POSIXct ──────────────────────────────────────────────────────
#>   skim_variable n_missing complete_rate min                 max                
#> 1 created               0             1 2016-07-08 09:54:16 2016-11-02 21:19:50
#> 2 modified              0             1 2016-07-08 09:55:43 2016-11-02 21:21:53
#> 3 ended                 0             1 2016-07-08 09:55:43 2016-11-02 21:21:53
#>   median              n_unique
#> 1 2016-07-08 12:47:07       28
#> 2 2016-07-08 14:23:22       28
#> 3 2016-07-08 14:23:22       28
#> 
#> ── Variable type: character ────────────────────────────────────────────────────
#>   skim_variable n_missing complete_rate min max empty n_unique whitespace
#> 1 session               0             1  64  64     0       28          0
#> 
#> ── Variable type: haven_labelled ───────────────────────────────────────────────
#>    skim_variable n_missing complete_rate mean    sd min median max empty
#>  1 BFIK_open_2           0             1 4.21 0.738   2    4     5    NA
#>  2 BFIK_agree_4R         0             1 2.93 1.18    1    3     5    NA
#>  3 BFIK_extra_2          0             1 4.18 1.09    1    4     5    NA
#>  4 BFIK_agree_1R         0             1 3    0.943   2    3     5    NA
#>  5 BFIK_open_1           0             1 4.39 0.832   2    5     5    NA
#>  6 BFIK_neuro_2R         0             1 3.11 0.875   2    3     5    NA
#>  7 BFIK_consc_3          0             1 3.5  1.04    1    4     5    NA
#>  8 BFIK_consc_4          0             1 3.86 0.756   2    4     5    NA
#>  9 BFIK_consc_2R         0             1 3.18 1.31    1    4     5    NA
#> 10 BFIK_agree_3R         0             1 3.04 1.29    1    3     5    NA
#> 11 BFIK_extra_3R         0             1 3.75 1.21    1    4     5    NA
#> 12 BFIK_neuro_3          0             1 3.07 1.27    1    3     5    NA
#> 13 BFIK_neuro_4          0             1 2.5  1.20    1    2     4    NA
#> 14 BFIK_agree_2          0             1 3.5  1.26    1    4     5    NA
#> 15 BFIK_consc_1          0             1 4.07 0.900   2    4     5    NA
#> 16 BFIK_open_4           0             1 4.21 0.957   1    4     5    NA
#> 17 BFIK_extra_4          0             1 3.86 1.11    1    4     5    NA
#> 18 BFIK_extra_1R         0             1 3.61 1.20    1    4     5    NA
#> 19 BFIK_open_3           0             1 4.21 0.957   2    4.5   5    NA
#>    n_unique whitespace n_value_labels hist    
#>  1       NA         NA              6 ▁▁▁▁▁▇▁▅
#>  2       NA         NA              6 ▂▇▁▃▁▅▁▂
#>  3       NA         NA              6 ▁▁▁▁▁▇▁▇
#>  4       NA         NA              6 ▇▁▅▁▁▆▁▁
#>  5       NA         NA              6 ▁▁▂▁▁▃▁▇
#>  6       NA         NA              6 ▆▁▇▁▁▇▁▁
#>  7       NA         NA              6 ▁▂▁▅▁▇▁▂
#>  8       NA         NA              6 ▁▁▃▁▁▇▁▂
#>  9       NA         NA              6 ▃▂▁▃▁▇▁▂
#> 10       NA         NA              6 ▂▇▁▃▁▇▁▃
#> 11       NA         NA              6 ▂▂▁▅▁▇▁▇
#> 12       NA         NA              6 ▃▇▁▇▁▅▁▅
#> 13       NA         NA              6 ▆▁▇▁▁▂▁▇
#> 14       NA         NA              6 ▂▅▁▅▁▇▁▆
#> 15       NA         NA              6 ▁▁▂▁▁▇▁▇
#> 16       NA         NA              6 ▁▁▁▂▁▆▁▇
#> 17       NA         NA              6 ▁▂▁▃▁▇▁▆
#> 18       NA         NA              6 ▁▅▁▆▁▇▁▇
#> 19       NA         NA              6 ▁▁▂▁▁▅▁▇
#> 
#> ── Variable type: logical ──────────────────────────────────────────────────────
#>   skim_variable n_missing complete_rate mean count
#> 1 expired              28             0  NaN ": " 
#> 
#> ── Variable type: numeric ──────────────────────────────────────────────────────
#>   skim_variable n_missing complete_rate mean    sd  min median  max hist 
#> 1 BFIK_agree            0             1 3.12 0.932 1.5    3    4.75 ▂▇▅▅▃
#> 2 BFIK_open             0             1 4.26 0.563 3      4.25 5    ▂▃▁▇▇
#> 3 BFIK_consc            0             1 3.65 0.792 2      3.75 5    ▂▃▇▇▃
#> 4 BFIK_extra            0             1 3.85 1.01  1.5    4.25 5    ▂▂▃▅▇
#> 5 BFIK_neuro            0             1 2.89 0.925 1.33   2.83 4.33 ▅▇▇▆▇
```
