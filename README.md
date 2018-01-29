# Codebook Cookbook
## Generate markdown codebooks from the attributes of the variables in your data frame

Integrates with formr ([formr.org](https://formr.org)), an online survey framework and especially the data frames produced and marked up by the [formr R package](https://github.com/rubenarslan/formr) and those based on SPSS or Stata files imported via [haven](https://github.com/hadley/haven). However, it should work with un-annotated datasets too, and you can also manually add the required attributes to make it fancier.

The codebook processes single items, but also "scales", i.e. psychological questionnaires that are aggregated to extract a construct. For scales, the appropriate reliability coefficients (internal consistencies for single measurements, retest reliabilities for repeated measurements, multilevel reliability for multilevel data) are computed.

For items and scales, the distributions are summarised graphically and numerically.

The result are markdown _partials_, that can be integrated in a larger markdown document and then translated to HTML or PDF format.

## Install

Just run these two commands in R:

```r
install.packages("devtools")
devtools::install_github("rubenarslan/codebook")
```

## How to use
A simple markdown document resulting in a codebook might look like this:

```
\---
title: "Codebook"
output:
  html_document:
    toc: yes
    toc_depth: 4
    toc_float: yes
    code_folding: 'hide'
    self_contained: no
\---

\```{r}
results = readRDS("results.rds")
knitr::opts_chunk$set(error = TRUE, echo = FALSE)
\```

\```{r}
library(codebook)
codebook(results)
\```
```

The resulting codebook will be an HTML file, but you can also choose to generate PDFs or Word files in RStudio.

To generate a results file in the necessary format using the formr package, you can run something like this.

  library(formr)
  source(".passwords.R")
  formr_connect(email = credentials$email, password = credentials$password)
  results = formr_results("s3_daily")
  saveRDS(results, 'results.rds')

To get a file in this format from SPSS, run e.g. `results = haven::read_sav("path/to/file.sav")`
