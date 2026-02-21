# Codebook example with formr.org data

In this vignette, you can see what a codebook generated from a dataset
with rich metadata looks like. This dataset includes mock data for a
short German Big Five personality inventory and an age variable. The
dataset follows the format created when importing data from
[formr.org](https://formr.org). However, data imported using the `haven`
package uses similar metadata. You can also add such metadata yourself,
or use the codebook package for unannotated datasets.

As you can see below, the `codebook` package automatically computes
reliabilities for multi-item inventories, generates nicely labelled
plots and outputs summary statistics. The same information is also
stored in a table, which you can export to various formats.
Additionally, `codebook` can show you different kinds of (labelled)
missing values, and show you common missingness patterns. As *you*
cannot see, but *[search
engines](https://datasetsearch.research.google.com)* will, the
`codebook` package also generates [JSON-LD](https://json-ld.org/)
metadata for the
[dataset](https://developers.google.com/search/docs/data-types/dataset).
If you share your codebook as an HTML file online, this metadata should
make it easier for others to find your data. [See what Google sees
here](https://search.google.com/structured-data/testing-tool#url=https%3A%2F%2Frubenarslan.github.io%2Fcodebook%2Farticles%2Fcodebook.html).

``` r

knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(warning = FALSE, message = TRUE, error = FALSE)
ggplot2::theme_set(ggplot2::theme_bw())

library(codebook)
data("bfi", package = 'codebook')
if (!knit_by_pkgdown) {
  library(dplyr)
    bfi <- bfi %>% select(-starts_with("BFIK_extra"),
                        -starts_with("BFIK_open"),
                        -starts_with("BFIK_consc"))
}
set.seed(1)
bfi$age <- rpois(nrow(bfi), 30)
library(labelled)
var_label(bfi$age) <- "Alter"
```

By default, we only set the required metadata attributes `name` and
`description` to sensible values. However, there is a number of
attributes you can set to describe the data better. [Find out
more](https://developers.google.com/search/docs/data-types/dataset).

``` r

metadata(bfi)$name <- "MOCK Big Five Inventory dataset (German metadata demo)"
metadata(bfi)$description <- "a small mock Big Five Inventory dataset"
metadata(bfi)$identifier <- "doi:10.5281/zenodo.1326520"
metadata(bfi)$datePublished <- "2016-06-01"
metadata(bfi)$creator <- list(
      "@type" = "Person",
      givenName = "Ruben", familyName = "Arslan",
      email = "ruben.arslan@gmail.com", 
      affiliation = list("@type" = "Organization",
        name = "MPI Human Development, Berlin"))
metadata(bfi)$citation <- "Arslan (2016). Mock BFI data."
metadata(bfi)$url <- "https://rubenarslan.github.io/codebook/articles/codebook.html"
metadata(bfi)$temporalCoverage <- "2016" 
metadata(bfi)$spatialCoverage <- "Goettingen, Germany" 
```

``` r

# We don't want to look at the code in the codebook.
knitr::opts_chunk$set(warning = TRUE, message = TRUE, echo = FALSE)
```

### Metadata

#### Description

**Dataset name**: MOCK Big Five Inventory dataset (German metadata demo)

a small mock Big Five Inventory dataset

Metadata for search engines

- **Temporal Coverage**: 2016

- **Spatial Coverage**: Goettingen, Germany

- **Citation**: Arslan (2016). Mock BFI data.

- **URL**:
  <https://rubenarslan.github.io/codebook/articles/codebook.html>

- **Identifier**:
  [doi:10.5281/zenodo.1326520](https://dx.doi.org/10.5281/zenodo.1326520)

- **Date published**: 2016-06-01

- **Creator**:

| name        | value                                        |
|:------------|:---------------------------------------------|
| @type       | Person                                       |
| givenName   | Ruben                                        |
| familyName  | Arslan                                       |
| email       | <ruben.arslan@gmail.com>                     |
| affiliation | Organization , MPI Human Development, Berlin |

[TABLE]

### Survey overview

28 completed rows, 28 who entered any information, 0 only viewed the
first page. There are 0 expired rows (people who did not finish filling
out in the requested time frame). In total, there are 28 rows including
unfinished and expired rows.

There were 28 unique participants, of which 28 finished filling out at
least one survey.

This survey was not repeated.

The first session started on 2016-07-08 09:54:16, the last session on
2016-11-02 21:19:50.

    ## Warning: `qplot()` was deprecated in ggplot2 3.4.0.
    ## ℹ The deprecated feature was likely used in the codebook package.
    ##   Please report the issue at <https://github.com/rubenarslan/codebook/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![Starting date
times](codebook_files/figure-html/cb_bfi_overview__starting_time-1.png)

Starting date times

People took on average 127.36 minutes (median 1.48) to answer the
survey.

    ## Warning: Removed 4 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_bar()`).

![Duration people took for answering the
survey](codebook_files/figure-html/cb_bfi_overview__duration-1.png)

Duration people took for answering the survey

## Variables

### Scale: BFIK_agree

- Overview
- Reliability details
- Summary statistics

**Reliability**: Cronbach’s α \[95% CI\] = 0.8 \[0.68;0.92\].

**Missing**: 0.

    ## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the likert package.
    ##   Please report the issue at <https://github.com/jbryer/likert/issues>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![Likert plot of scale BFIK_agree
items](codebook_files/figure-html/cb_bfi_BFIK_agree_likert-1.png)

Likert plot of scale BFIK_agree items

![Distribution of scale
BFIK_agree](codebook_files/figure-html/cb_bfi_BFIK_agree_distribution-1.png)

Distribution of scale BFIK_agree

###### Reliability

###### 95% Confidence Interval

|     lower |  estimate |     upper |
|----------:|----------:|----------:|
| 0.6815382 | 0.8005842 | 0.9196302 |

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | ase | mean | sd | median_r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|  | 0.8005842 | 0.8032578 | 0.8025354 | 0.5051216 | 4.082794 | 0.0607377 | 3.116071 | 0.9316506 | 0.4955289 |

###### Reliability if an item is dropped:

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | alpha se | var.r | med.r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| BFIK_agree_4R | 0.7039106 | 0.7059214 | 0.6277253 | 0.4444909 | 2.400452 | 0.0922357 | 0.0093288 | 0.4566167 |
| BFIK_agree_1R | 0.7822898 | 0.7821901 | 0.7633114 | 0.5448449 | 3.591160 | 0.0731005 | 0.0413123 | 0.5344410 |
| BFIK_agree_3R | 0.6758982 | 0.6925559 | 0.6242359 | 0.4288568 | 2.252623 | 0.1072194 | 0.0212505 | 0.3469923 |
| BFIK_agree_2 | 0.8180575 | 0.8196006 | 0.7841113 | 0.6022937 | 4.543256 | 0.0568475 | 0.0219955 | 0.5971631 |

###### Item statistics

|               |   n |     raw.r |     std.r |     r.cor |    r.drop |     mean |       sd |
|:--------------|----:|----------:|----------:|----------:|----------:|---------:|---------:|
| BFIK_agree_4R |  28 | 0.8471206 | 0.8503385 | 0.8291974 | 0.7057555 | 2.928571 | 1.184110 |
| BFIK_agree_1R |  28 | 0.7168171 | 0.7554255 | 0.6253942 | 0.5538583 | 3.000000 | 0.942809 |
| BFIK_agree_3R |  28 | 0.8820884 | 0.8651249 | 0.8439281 | 0.7510051 | 3.035714 | 1.290482 |
| BFIK_agree_2  |  28 | 0.7205961 | 0.7010915 | 0.5430779 | 0.4825103 | 3.500000 | 1.261980 |

###### Non missing response frequency for each item

|               |         1 |         2 |         3 |         4 |         5 | miss |
|:--------------|----------:|----------:|----------:|----------:|----------:|-----:|
| BFIK_agree_4R | 0.0714286 | 0.3928571 | 0.1785714 | 0.2500000 | 0.1071429 |    0 |
| BFIK_agree_1R | 0.0000000 | 0.3928571 | 0.2500000 | 0.3214286 | 0.0357143 |    0 |
| BFIK_agree_3R | 0.1071429 | 0.3214286 | 0.1428571 | 0.2857143 | 0.1428571 |    0 |
| BFIK_agree_2  | 0.0714286 | 0.1785714 | 0.1785714 | 0.3214286 | 0.2500000 |    0 |

[TABLE]

### Scale: BFIK_open

- Overview
- Reliability details
- Summary statistics

**Reliability**: Cronbach’s α \[95% CI\] = 0.53 \[0.25;0.81\].

**Missing**: 0.

![Likert plot of scale BFIK_open
items](codebook_files/figure-html/cb_bfi_BFIK_open_likert-48-1.png)

Likert plot of scale BFIK_open items

![Distribution of scale
BFIK_open](codebook_files/figure-html/cb_bfi_BFIK_open_distribution-49-1.png)

Distribution of scale BFIK_open

###### Reliability

###### 95% Confidence Interval

|     lower |  estimate |    upper |
|----------:|----------:|---------:|
| 0.2473043 | 0.5270752 | 0.806846 |

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | ase | mean | sd | median_r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|  | 0.5270752 | 0.5130499 | 0.5384384 | 0.2084848 | 1.053598 | 0.1427402 | 4.258929 | 0.5630692 | 0.1770952 |

###### Reliability if an item is dropped:

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | alpha se | var.r | med.r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| BFIK_open_2 | 0.5186964 | 0.5019443 | 0.5306346 | 0.2514611 | 1.0078075 | 0.1551415 | 0.0955990 | 0.2626354 |
| BFIK_open_1 | 0.5786594 | 0.5593735 | 0.5130468 | 0.2973409 | 1.2694957 | 0.1307794 | 0.0504451 | 0.1947743 |
| BFIK_open_4 | 0.4119483 | 0.4100879 | 0.3217759 | 0.1881289 | 0.6951677 | 0.1885214 | 0.0042364 | 0.1594161 |
| BFIK_open_3 | 0.2202412 | 0.2437361 | 0.2041506 | 0.0970083 | 0.3222898 | 0.2572230 | 0.0195543 | 0.1594161 |

###### Item statistics

|             |   n |     raw.r |     std.r |     r.cor |    r.drop |     mean |        sd |
|:------------|----:|----------:|----------:|----------:|----------:|---------:|----------:|
| BFIK_open_2 |  28 | 0.5298378 | 0.5869036 | 0.3017297 | 0.2317746 | 4.214286 | 0.7382232 |
| BFIK_open_1 |  28 | 0.5062740 | 0.5329245 | 0.2760932 | 0.1568781 | 4.392857 | 0.8317445 |
| BFIK_open_4 |  28 | 0.7010199 | 0.6614159 | 0.5679139 | 0.3611976 | 4.214286 | 0.9567361 |
| BFIK_open_3 |  28 | 0.8041472 | 0.7686222 | 0.7253133 | 0.5379725 | 4.214286 | 0.9567361 |

###### Non missing response frequency for each item

|             |         1 |         2 |         3 |         4 |         5 | miss |
|:------------|----------:|----------:|----------:|----------:|----------:|-----:|
| BFIK_open_2 | 0.0000000 | 0.0357143 | 0.0714286 | 0.5357143 | 0.3571429 |    0 |
| BFIK_open_1 | 0.0000000 | 0.0357143 | 0.1071429 | 0.2857143 | 0.5714286 |    0 |
| BFIK_open_4 | 0.0357143 | 0.0000000 | 0.1428571 | 0.3571429 | 0.4642857 |    0 |
| BFIK_open_3 | 0.0000000 | 0.0714286 | 0.1428571 | 0.2857143 | 0.5000000 |    0 |

[TABLE]

### Scale: BFIK_consc

- Overview
- Reliability details
- Summary statistics

**Reliability**: Cronbach’s α \[95% CI\] = 0.78 \[0.66;0.9\].

**Missing**: 0.

![Likert plot of scale BFIK_consc
items](codebook_files/figure-html/cb_bfi_BFIK_consc_likert-69-1.png)

Likert plot of scale BFIK_consc items

![Distribution of scale
BFIK_consc](codebook_files/figure-html/cb_bfi_BFIK_consc_distribution-70-1.png)

Distribution of scale BFIK_consc

###### Reliability

###### 95% Confidence Interval

|     lower |  estimate |     upper |
|----------:|----------:|----------:|
| 0.6572704 | 0.7796983 | 0.9021261 |

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | ase | mean | sd | median_r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|  | 0.7796983 | 0.7870619 | 0.7839117 | 0.480263 | 3.6962 | 0.0624632 | 3.651786 | 0.7915622 | 0.4590337 |

###### Reliability if an item is dropped:

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | alpha se | var.r | med.r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| BFIK_consc_3 | 0.5979912 | 0.6118484 | 0.5247890 | 0.3444504 | 1.576313 | 0.1176371 | 0.0092555 | 0.3982579 |
| BFIK_consc_4 | 0.7626290 | 0.7705758 | 0.7158716 | 0.5282082 | 3.358737 | 0.0739794 | 0.0185680 | 0.5163541 |
| BFIK_consc_2R | 0.7283814 | 0.7272275 | 0.7111827 | 0.4705314 | 2.666058 | 0.0849270 | 0.0474736 | 0.5163541 |
| BFIK_consc_1 | 0.7813268 | 0.8041781 | 0.7720873 | 0.5778620 | 4.106681 | 0.0663018 | 0.0232879 | 0.6618601 |

###### Item statistics

|               |   n |     raw.r |     std.r |     r.cor |    r.drop |     mean |        sd |
|:--------------|----:|----------:|----------:|----------:|----------:|---------:|----------:|
| BFIK_consc_3  |  28 | 0.9085942 | 0.9115478 | 0.9168579 | 0.8120889 | 3.500000 | 1.0363755 |
| BFIK_consc_4  |  28 | 0.6874998 | 0.7351180 | 0.6337064 | 0.5256863 | 3.857143 | 0.7559289 |
| BFIK_consc_2R |  28 | 0.8411080 | 0.7904948 | 0.6957375 | 0.6208811 | 3.178571 | 1.3067792 |
| BFIK_consc_1  |  28 | 0.6732655 | 0.6874444 | 0.5201855 | 0.4656927 | 4.071429 | 0.8997354 |

###### Non missing response frequency for each item

|               |         1 |         2 |         3 |         4 |         5 | miss |
|:--------------|----------:|----------:|----------:|----------:|----------:|-----:|
| BFIK_consc_3  | 0.0357143 | 0.1428571 | 0.2500000 | 0.4285714 | 0.1428571 |    0 |
| BFIK_consc_4  | 0.0000000 | 0.0357143 | 0.2500000 | 0.5357143 | 0.1785714 |    0 |
| BFIK_consc_2R | 0.1785714 | 0.1071429 | 0.1785714 | 0.4285714 | 0.1071429 |    0 |
| BFIK_consc_1  | 0.0000000 | 0.0714286 | 0.1428571 | 0.4285714 | 0.3571429 |    0 |

[TABLE]

### Scale: BFIK_extra

- Overview
- Reliability details
- Summary statistics

**Reliability**: Cronbach’s α \[95% CI\] = 0.9 \[0.84;0.96\].

**Missing**: 0.

![Likert plot of scale BFIK_extra
items](codebook_files/figure-html/cb_bfi_BFIK_extra_likert-90-1.png)

Likert plot of scale BFIK_extra items

![Distribution of scale
BFIK_extra](codebook_files/figure-html/cb_bfi_BFIK_extra_distribution-91-1.png)

Distribution of scale BFIK_extra

###### Reliability

###### 95% Confidence Interval

|     lower |  estimate |     upper |
|----------:|----------:|----------:|
| 0.8368641 | 0.8992625 | 0.9616609 |

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | ase | mean | sd | median_r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|  | 0.8992625 | 0.8991277 | 0.9103612 | 0.6902472 | 8.913524 | 0.0318359 | 3.848214 | 1.009995 | 0.693212 |

###### Reliability if an item is dropped:

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | alpha se | var.r | med.r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| BFIK_extra_2 | 0.9049618 | 0.9049007 | 0.8810063 | 0.7602940 | 9.515329 | 0.0315012 | 0.0075063 | 0.7906075 |
| BFIK_extra_3R | 0.8656522 | 0.8666622 | 0.8458840 | 0.6842023 | 6.499751 | 0.0453083 | 0.0171133 | 0.7238529 |
| BFIK_extra_4 | 0.8526404 | 0.8505466 | 0.8225061 | 0.6548173 | 5.691049 | 0.0487973 | 0.0233309 | 0.5986022 |
| BFIK_extra_1R | 0.8524132 | 0.8543807 | 0.8025601 | 0.6616754 | 5.867224 | 0.0487065 | 0.0039225 | 0.6625711 |

###### Item statistics

|               |   n |     raw.r |     std.r |     r.cor |    r.drop |     mean |       sd |
|:--------------|----:|----------:|----------:|----------:|----------:|---------:|---------:|
| BFIK_extra_2  |  28 | 0.8073670 | 0.8162172 | 0.7344684 | 0.6733826 | 4.178571 | 1.090483 |
| BFIK_extra_3R |  28 | 0.8877202 | 0.8813510 | 0.8432063 | 0.7880184 | 3.750000 | 1.205696 |
| BFIK_extra_4  |  28 | 0.9027707 | 0.9065043 | 0.8787875 | 0.8247657 | 3.857143 | 1.112697 |
| BFIK_extra_1R |  28 | 0.9062901 | 0.9006338 | 0.8874781 | 0.8219851 | 3.607143 | 1.196888 |

###### Non missing response frequency for each item

|               |         1 |         2 |         3 |         4 |         5 | miss |
|:--------------|----------:|----------:|----------:|----------:|----------:|-----:|
| BFIK_extra_2  | 0.0714286 | 0.0000000 | 0.0714286 | 0.3928571 | 0.4642857 |    0 |
| BFIK_extra_3R | 0.0714286 | 0.0714286 | 0.2142857 | 0.3214286 | 0.3214286 |    0 |
| BFIK_extra_4  | 0.0357143 | 0.1071429 | 0.1428571 | 0.3928571 | 0.3214286 |    0 |
| BFIK_extra_1R | 0.0357143 | 0.1785714 | 0.2142857 | 0.2857143 | 0.2857143 |    0 |

[TABLE]

### Scale: BFIK_neuro

- Overview
- Reliability details
- Summary statistics

**Reliability**: Cronbach’s α \[95% CI\] = 0.75 \[0.61;0.9\].

**Missing**: 0.

![Likert plot of scale BFIK_neuro
items](codebook_files/figure-html/cb_bfi_BFIK_neuro_likert-111-1.png)

Likert plot of scale BFIK_neuro items

![Distribution of scale
BFIK_neuro](codebook_files/figure-html/cb_bfi_BFIK_neuro_distribution-112-1.png)

Distribution of scale BFIK_neuro

###### Reliability

###### 95% Confidence Interval

|     lower |  estimate |     upper |
|----------:|----------:|----------:|
| 0.6080638 | 0.7537326 | 0.8994015 |

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | ase | mean | sd | median_r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
|  | 0.7537326 | 0.7476172 | 0.7145893 | 0.496833 | 2.962235 | 0.0743208 | 2.892857 | 0.9254231 | 0.440167 |

###### Reliability if an item is dropped:

|  | raw_alpha | std.alpha | G6(smc) | average_r | S/N | alpha se | var.r | med.r |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| BFIK_neuro_2R | 0.8400000 | 0.8408387 | 0.7253854 | 0.7253854 | 5.2829328 | 0.0602555 | NA | 0.7253854 |
| BFIK_neuro_3 | 0.5904682 | 0.6112721 | 0.4401670 | 0.4401670 | 1.5724937 | 0.1456732 | NA | 0.4401670 |
| BFIK_neuro_4 | 0.4653928 | 0.4905053 | 0.3249467 | 0.3249467 | 0.9627289 | 0.1871605 | NA | 0.3249467 |

###### Item statistics

|               |   n |     raw.r |     std.r |     r.cor |    r.drop |     mean |        sd |
|:--------------|----:|----------:|----------:|----------:|----------:|---------:|----------:|
| BFIK_neuro_2R |  28 | 0.6549435 | 0.7217484 | 0.4638430 | 0.4100297 | 3.107143 | 0.8751417 |
| BFIK_neuro_3  |  28 | 0.8755182 | 0.8383732 | 0.7625955 | 0.6528603 | 3.071429 | 1.2744954 |
| BFIK_neuro_4  |  28 | 0.9046526 | 0.8854863 | 0.8409166 | 0.7420620 | 2.500000 | 1.2018504 |

###### Non missing response frequency for each item

|               |         1 |         2 |         3 |         4 |         5 | miss |
|:--------------|----------:|----------:|----------:|----------:|----------:|-----:|
| BFIK_neuro_2R | 0.0000000 | 0.2857143 | 0.3571429 | 0.3214286 | 0.0357143 |    0 |
| BFIK_neuro_3  | 0.1071429 | 0.2500000 | 0.2857143 | 0.1785714 | 0.1785714 |    0 |
| BFIK_neuro_4  | 0.2500000 | 0.3214286 | 0.1071429 | 0.3214286 | 0.0000000 |    0 |

[TABLE]

### age

Alter

- Distribution
- Summary statistics

![Distribution of values for
age](codebook_files/figure-html/cb_bfi_age_distribution-131-1.png)

Distribution of values for age

0 missing values.

| name | label | data_type | n_missing | complete_rate | min | median | max | mean |       sd | hist  |
|:-----|:------|:----------|----------:|--------------:|:----|:-------|:----|-----:|---------:|:------|
| age  | Alter | numeric   |         0 |             1 | 19  | 32     | 38  | 30.5 | 4.670633 | ▂▂▇▇▅ |

## Missingness report

## Codebook table

JSON-LD metadata

The following JSON-LD can be found by search engines, if you share this
codebook publicly on the web.

``` json
{
  "name": "MOCK Big Five Inventory dataset (German metadata demo)",
  "description": "a small mock Big Five Inventory dataset\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n|name          |label                                                                      | n_missing|\n|:-------------|:--------------------------------------------------------------------------|---------:|\n|session       |NA                                                                         |         0|\n|created       |user first opened survey                                                   |         0|\n|modified      |user last edited survey                                                    |         0|\n|ended         |user finished survey                                                       |         0|\n|expired       |NA                                                                         |        28|\n|BFIK_open_2   |__Ich bin tiefsinnig, denke gerne über Sachen nach.__                      |         0|\n|BFIK_agree_4R |__Ich kann mich schroff und abweisend anderen gegenüber verhalten.__       |         0|\n|BFIK_extra_2  |__Ich bin begeisterungsfähig und kann andere leicht mitreißen.__           |         0|\n|BFIK_agree_1R |__Ich neige dazu, andere zu kritisieren.__                                 |         0|\n|BFIK_open_1   |__Ich bin vielseitig interessiert.__                                       |         0|\n|BFIK_neuro_2R |__Ich bin entspannt, lasse mich durch Stress nicht aus der Ruhe bringen.__ |         0|\n|BFIK_consc_3  |__Ich bin tüchtig und arbeite flott.__                                     |         0|\n|BFIK_consc_4  |__Ich mache Pläne und führe sie auch durch.__                              |         0|\n|BFIK_consc_2R |__Ich bin bequem, neige zur Faulheit.__                                    |         0|\n|BFIK_agree_3R |__Ich kann mich kalt und distanziert verhalten.__                          |         0|\n|BFIK_extra_3R |__Ich bin eher der \"stille Typ\", wortkarg.__                               |         0|\n|BFIK_neuro_3  |__Ich mache mir viele Sorgen.__                                            |         0|\n|BFIK_neuro_4  |__Ich werde leicht nervös und unsicher.__                                  |         0|\n|BFIK_agree_2  |__Ich schenke anderen leicht Vertrauen, glaube an das Gute im Menschen.__  |         0|\n|BFIK_consc_1  |__Ich erledige Aufgaben gründlich.__                                       |         0|\n|BFIK_open_4   |__Ich schätze künstlerische und ästhetische Eindrücke.__                   |         0|\n|BFIK_extra_4  |__Ich gehe aus mir heraus, bin gesellig.__                                 |         0|\n|BFIK_extra_1R |__Ich bin eher zurückhaltend, reserviert.__                                |         0|\n|BFIK_open_3   |__Ich habe eine aktive Vorstellungskraft, bin phantasievoll.__             |         0|\n|BFIK_agree    |4 BFIK_agree items aggregated by aggregation_function                      |         0|\n|BFIK_open     |4 BFIK_open items aggregated by aggregation_function                       |         0|\n|BFIK_consc    |4 BFIK_consc items aggregated by aggregation_function                      |         0|\n|BFIK_extra    |4 BFIK_extra items aggregated by aggregation_function                      |         0|\n|BFIK_neuro    |3 BFIK_neuro items aggregated by aggregation_function                      |         0|\n|age           |Alter                                                                      |         0|\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.9.7).",
  "identifier": "doi:10.5281/zenodo.1326520",
  "datePublished": "2016-06-01",
  "creator": {
    "@type": "Person",
    "givenName": "Ruben",
    "familyName": "Arslan",
    "email": "ruben.arslan@gmail.com",
    "affiliation": {
      "@type": "Organization",
      "name": "MPI Human Development, Berlin"
    }
  },
  "citation": "Arslan (2016). Mock BFI data.",
  "url": "https://rubenarslan.github.io/codebook/articles/codebook.html",
  "temporalCoverage": "2016",
  "spatialCoverage": "Goettingen, Germany",
  "keywords": ["session", "created", "modified", "ended", "expired", "BFIK_open_2", "BFIK_agree_4R", "BFIK_extra_2", "BFIK_agree_1R", "BFIK_open_1", "BFIK_neuro_2R", "BFIK_consc_3", "BFIK_consc_4", "BFIK_consc_2R", "BFIK_agree_3R", "BFIK_extra_3R", "BFIK_neuro_3", "BFIK_neuro_4", "BFIK_agree_2", "BFIK_consc_1", "BFIK_open_4", "BFIK_extra_4", "BFIK_extra_1R", "BFIK_open_3", "BFIK_agree", "BFIK_open", "BFIK_consc", "BFIK_extra", "BFIK_neuro", "age"],
  "@context": "https://schema.org/",
  "@type": "Dataset",
  "variableMeasured": [
    {
      "name": "session",
      "@type": "propertyValue"
    },
    {
      "name": "created",
      "description": "user first opened survey",
      "@type": "propertyValue"
    },
    {
      "name": "modified",
      "description": "user last edited survey",
      "@type": "propertyValue"
    },
    {
      "name": "ended",
      "description": "user finished survey",
      "@type": "propertyValue"
    },
    {
      "name": "expired",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_open_2",
      "description": "__Ich bin tiefsinnig, denke gerne über Sachen nach.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_agree_4R",
      "description": "__Ich kann mich schroff und abweisend anderen gegenüber verhalten.__",
      "value": "5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_extra_2",
      "description": "__Ich bin begeisterungsfähig und kann andere leicht mitreißen.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_agree_1R",
      "description": "__Ich neige dazu, andere zu kritisieren.__",
      "value": "5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_open_1",
      "description": "__Ich bin vielseitig interessiert.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_neuro_2R",
      "description": "__Ich bin entspannt, lasse mich durch Stress nicht aus der Ruhe bringen.__",
      "value": "5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_consc_3",
      "description": "__Ich bin tüchtig und arbeite flott.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_consc_4",
      "description": "__Ich mache Pläne und führe sie auch durch.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_consc_2R",
      "description": "__Ich bin bequem, neige zur Faulheit.__",
      "value": "5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_agree_3R",
      "description": "__Ich kann mich kalt und distanziert verhalten.__",
      "value": "5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_extra_3R",
      "description": "__Ich bin eher der \"stille Typ\", wortkarg.__",
      "value": "5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_neuro_3",
      "description": "__Ich mache mir viele Sorgen.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_neuro_4",
      "description": "__Ich werde leicht nervös und unsicher.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_agree_2",
      "description": "__Ich schenke anderen leicht Vertrauen, glaube an das Gute im Menschen.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_consc_1",
      "description": "__Ich erledige Aufgaben gründlich.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_open_4",
      "description": "__Ich schätze künstlerische und ästhetische Eindrücke.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_extra_4",
      "description": "__Ich gehe aus mir heraus, bin gesellig.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_extra_1R",
      "description": "__Ich bin eher zurückhaltend, reserviert.__",
      "value": "5. 1: Trifft überhaupt nicht zu,\n4. 2,\n3. 3,\n2. 4,\n1. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_open_3",
      "description": "__Ich habe eine aktive Vorstellungskraft, bin phantasievoll.__",
      "value": "1. 1: Trifft überhaupt nicht zu,\n2. 2,\n3. 3,\n4. 4,\n5. 5: Trifft voll und ganz zu,\nNA. Item was never rendered for this user.",
      "maxValue": 5,
      "minValue": 1,
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_agree",
      "description": "4 BFIK_agree items aggregated by aggregation_function",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_open",
      "description": "4 BFIK_open items aggregated by aggregation_function",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_consc",
      "description": "4 BFIK_consc items aggregated by aggregation_function",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_extra",
      "description": "4 BFIK_extra items aggregated by aggregation_function",
      "@type": "propertyValue"
    },
    {
      "name": "BFIK_neuro",
      "description": "3 BFIK_neuro items aggregated by aggregation_function",
      "@type": "propertyValue"
    },
    {
      "name": "age",
      "description": "Alter",
      "@type": "propertyValue"
    }
  ]
}`
```
