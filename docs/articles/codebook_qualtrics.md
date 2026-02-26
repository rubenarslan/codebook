# Codebook example with Qualtrics dataset

``` r

knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(warning = FALSE, message = FALSE, error = FALSE, echo = TRUE)
ggplot2::theme_set(ggplot2::theme_bw())
library(codebook)
library(dplyr)
```

Here, I try to demonstrate that we can also use the metadata generated
by Qualtrics. Because I am not a Qualtrics user, the developer of the
`qualtRics` R package sent me two example files, one of data and one for
metadata, which I use here. Usually, you’d import these files directly
via his package.

## Load

``` r

# library(qualtRics) # currently not on CRAN, so commented out
results <- readRDS(system.file("extdata", "ryan.rds", package = "codebook"))
metadata_ex <- readRDS(system.file("extdata", "metadata.rds", package = "codebook"))
```

Now, we want the metadata not to be just independent of the data frame.
It’s easier to change metadata if it is in the form of a list at the
dataset level, so let’s use
[`rio::gather_attrs`](http://gesistsa.github.io/rio/reference/gather_attrs.md)
for that.

``` r

results <- results %>% rio::gather_attrs()
attributes(results)$label$Q7
```

    ##                                                                                  Q7 
    ## "How reasonable or unreasonable is the cost of courses/tuition at this university?"

Now, we can loop over the metadata list and put it in the right shape to
become attributes. This means given each element the name of the
variable it refers to.

``` r

names(metadata_ex$questions) <- lapply(metadata_ex$questions, function(x) {
  x$questionName
})
```

We only want the questions that we have data for.

``` r

qs <- names(metadata_ex$questions)
qs <- qs[qs %in% names(attributes(results)$label)]
```

Now, we assign our metadata list to the attributes of the data.frame.

``` r

init <- vector("list", ncol(results)) 
names(init) <- names(results)
attributes(results)$item <- init
attributes(results)$item[qs] <- metadata_ex$questions[qs]
```

And use rio to put the attributes on the question level again.

``` r

results <- results %>% rio::spread_attrs()
```

To keep this example, we select a subset of variables.

``` r

results <- results %>% select(ResponseSet, Q7, Q10)
if (!knit_by_pkgdown) knitr::opts_chunk$set(echo = FALSE)
```

Please note, that we just reuse the metadata column names from
Qualtrics. Standardising this across multiple survey providers would be
great but requires more knowledge of Qualtrics than I have.

``` r

metadata(results)$name <- "MOCK Qualtrics dataset"
metadata(results)$description <- "a MOCK dataset used to show how to import Qualtrics metadata into the codebook R package"
metadata(results)$identifier <- "doi:10.5281/zenodo.1326520"
metadata(results)$datePublished <- "2018-08-01"
metadata(results)$creator <- list(
      "@type" = "Person",
      givenName = "Ruben", familyName = "Arslan",
      email = "ruben.arslan@gmail.com", 
      affiliation = list("@type" = "Organization",
        name = "MPI Human Development, Berlin"))
metadata(results)$url <- "https://rubenarslan.github.io/codebook/articles/codebook_qualtrics.html"
metadata(results)$temporalCoverage <- "2018" 
metadata(results)$spatialCoverage <- "Nowhere" 
```

``` r

# We don't want to look at the code in the codebook.
knitr::opts_chunk$set(warning = TRUE, message = TRUE, echo = FALSE)
```

    ## No missing values.

### Metadata

#### Description

**Dataset name**: MOCK Qualtrics dataset

a MOCK dataset used to show how to import Qualtrics metadata into the
codebook R package

Metadata for search engines

- **Temporal Coverage**: 2018

- **Spatial Coverage**: Nowhere

- **URL**:
  <https://rubenarslan.github.io/codebook/articles/codebook_qualtrics.html>

- **Identifier**:
  [doi:10.5281/zenodo.1326520](https://dx.doi.org/10.5281/zenodo.1326520)

- **Date published**: 2018-08-01

- **Creator**:

| name        | value                                                    |
|:------------|:---------------------------------------------------------|
| @type       | Person                                                   |
| givenName   | Ruben                                                    |
| familyName  | Arslan                                                   |
| email       | <ruben.arslan@gmail.com>                                 |
| affiliation | @type: Organization, name: MPI Human Development, Berlin |

| name     | value                |
|:---------|:---------------------|
| keywords | ResponseSet, Q7, Q10 |

## Variables

### ResponseSet

ResponseSet

- Distribution
- Summary statistics

![Distribution of values for
ResponseSet](codebook_qualtrics_files/figure-html/cb_results_ResponseSet_distribution-1.png)

Distribution of values for ResponseSet

0 missing values.

| name | data_type | n_missing | complete_rate | n_unique | empty | min | max | whitespace | ResponseSet | label |
|:---|:---|---:|---:|---:|---:|:---|:---|---:|:---|:---|
| ResponseSet | character | 0 | 1 | 1 | 0 | 20 | 20 | 0 | ResponseSet | NA |

### Q7

How reasonable or unreasonable is the cost of courses/tuition at this
university?

- Distribution
- Summary statistics
- Item
- Value labels

![Distribution of values for
Q7](codebook_qualtrics_files/figure-html/cb_results_Q7_distribution-48-1.png)

Distribution of values for Q7

0 missing values.

[TABLE]

| type | selector | subSelector | questionText | doesForceResponse | questionName |
|:---|:---|:---|:---|:---|:---|
| MC | SAVR | TX | How reasonable or unreasonable is the cost of courses/tuition at this university? | FALSE | Q7 |

Item options {.table}

| name | value |
|:---|:---|
| 1 | 1 , Extremely reasonable, Extremely reasonable, TRUE |
| 2 | 2 , Moderately reasonable, Moderately reasonable, TRUE |
| 3 | 3 , Slightly reasonable, Slightly reasonable, TRUE |
| 4 | 4 , Neither reasonable nor unreasonable, Neither reasonable nor unreasonable, TRUE |
| 5 | 5 , Slightly unreasonable, Slightly unreasonable, TRUE |
| 6 | 6 , Moderately unreasonable, Moderately unreasonable, TRUE |
| 7 | 7 , Extremely unreasonable, Extremely unreasonable, TRUE |

Response choices {.table}

### Q10

How helpful or unhelpful is the staff at the on-campus health center?

- Distribution
- Summary statistics
- Item
- Value labels

![Distribution of values for
Q10](codebook_qualtrics_files/figure-html/cb_results_Q10_distribution-61-1.png)

Distribution of values for Q10

0 missing values.

[TABLE]

| type | selector | subSelector | questionText | doesForceResponse | questionName |
|:---|:---|:---|:---|:---|:---|
| MC | SAVR | TX | How helpful or unhelpful is the staff at the on-campus health center? | FALSE | Q10 |

Item options {.table}

| name | value |
|:---|:---|
| 1 | 1 , Extremely helpful, Extremely helpful, TRUE |
| 2 | 2 , Moderately helpful, Moderately helpful, TRUE |
| 3 | 3 , Slightly helpful, Slightly helpful, TRUE |
| 4 | 4 , Neither helpful nor unhelpful, Neither helpful nor unhelpful, TRUE |
| 5 | 5 , Slightly unhelpful, Slightly unhelpful, TRUE |
| 6 | 6 , Moderately unhelpful, Moderately unhelpful, TRUE |
| 7 | 7 , Extremely unhelpful, Extremely unhelpful, TRUE |

Response choices {.table}

## Missingness report

## Codebook table

JSON-LD metadata

The following JSON-LD can be found by search engines, if you share this
codebook publicly on the web.

``` json
{
  "name": "MOCK Qualtrics dataset",
  "description": "a MOCK dataset used to show how to import Qualtrics metadata into the codebook R package\n\n\n## Table of variables\nThis table contains variable names, labels, and number of missing values.\nSee the complete codebook for more.\n\n|name        |label | n_missing|\n|:-----------|:-----|---------:|\n|ResponseSet |NA    |         0|\n|Q7          |NA    |         0|\n|Q10         |NA    |         0|\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/) (version 0.10.0).",
  "identifier": "doi:10.5281/zenodo.1326520",
  "datePublished": "2018-08-01",
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
  "url": "https://rubenarslan.github.io/codebook/articles/codebook_qualtrics.html",
  "temporalCoverage": "2018",
  "spatialCoverage": "Nowhere",
  "keywords": ["ResponseSet", "Q7", "Q10"],
  "@context": "https://schema.org/",
  "@type": "Dataset",
  "variableMeasured": [
    {
      "name": "ResponseSet",
      "description": "ResponseSet",
      "@type": "propertyValue"
    },
    {
      "name": "Q7",
      "description": "How reasonable or unreasonable is the cost of courses/tuition at this university?",
      "value": "1. Extremely reasonable,\n2. Moderately reasonable,\n3. Slightly reasonable,\n4. Neither reasonable nor unreasonable,\n5. Slightly unreasonable,\n6. Moderately unreasonable,\n7. Extremely unreasonable",
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    },
    {
      "name": "Q10",
      "description": "How helpful or unhelpful is the staff at the on-campus health center?",
      "value": "1. Extremely helpful,\n2. Moderately helpful,\n3. Slightly helpful,\n4. Neither helpful nor unhelpful,\n5. Slightly unhelpful,\n6. Moderately unhelpful,\n7. Extremely unhelpful",
      "measurementTechnique": "self-report",
      "@type": "propertyValue"
    }
  ]
}`
```
