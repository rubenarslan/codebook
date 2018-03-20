
<!-- README.md is generated from README.Rmd. Please edit that file -->
Codebook Cookbook
=================

[![Travis-CI Build Status](https://travis-ci.org/rubenarslan/codebook.svg?branch=master)](https://travis-ci.org/rubenarslan/codebook) [![codecov](https://codecov.io/gh/rubenarslan/codebook/branch/master/graph/badge.svg)](https://codecov.io/gh/rubenarslan/codebook)

Description
-----------

Easily automate the following tasks to describe data frames: computing reliabilities (internal consistencies, retest, multilevel) for psychological scales, summarise the distributions of scales and items graphically and using descriptive statistics, combine this information with metadata (such as item labels and labelled values) that is derived from R attributes. To do so, the package relies on 'rmarkdown' partials, so you can generate HTML, PDF, and Word documents. Codebooks are also available as tables (CSV, Excel, etc.).

Generate markdown codebooks from the attributes of the variables in your data frame
-----------------------------------------------------------------------------------

RStudio and a few of the tidyverse package already usefully display the information contained in the attributes of the variables in your data frame. The [haven](https://github.com/hadley/haven) package also manages to grab variable documentation from SPSS or Stata files.

The codebook package takes those attributes and the data and tries to produce a good-looking codebook, i.e. a place to get an overview of the variables in a dataset. The codebook processes single items, but also "scales", i.e. psychological questionnaires that are aggregated to extract a construct. For scales, the appropriate reliability coefficients (internal consistencies for single measurements, retest reliabilities for repeated measurements, multilevel reliability for multilevel data) are computed. For items and scales, the distributions are summarised graphically and numerically.

This package integrates tightly with formr ([formr.org](https://formr.org)), an online survey framework and especially the data frames produced and marked up by the [formr R package](https://github.com/rubenarslan/formr). However, codebook is completely independent of it.

Documentation
-------------

Confer the help or: <https://rubenarslan.github.io/codebook>. See the [vignette](https://rubenarslan.github.io/codebook/articles/codebook.html) for a quick example of an HTML document generated using `codebook`.

Use as a webapp
---------------

If you don't want to install the codebook package, you can just upload an annotated dataset in a variety of formats here: <https://rubenarslan.ocpu.io/codebook/>

Use locally
-----------

### Install

Just run these two commands in R:

``` r
install.packages("devtools")
devtools::install_github("rubenarslan/codebook")
```

### How to use

To see a simple markdown document that you could use to generate a codebook, see the [webapp](https://rubenarslan.ocpu.io/codebook/).

The resulting codebook will be an HTML file, but you can also choose to generate PDFs or Word files.

To generate a results file in the necessary format using the formr package, you can run something like this.

``` r
library(formr)
source(".passwords.R")
formr_connect(email = credentials$email, password = credentials$password)
results = formr_results("s3_daily")
saveRDS(results, 'results.rds')
```

To get a file in this format from SPSS, run e.g. `results = haven::read_sav("path/to/file.sav")`

Generating a codebook is as simple as calling codebook from a chunk in an rmarkdown document.

``` r
codebook(results)
```

[Code of conduct for contributing](CONDUCT.md)
----------------------------------------------
