## Submission
* Resubmission after archival for policy violation (internet resource access).
* All vignette code that downloads from external URLs (OSF) is now wrapped 
  in tryCatch with informative messages and graceful exits via knitr::knit_exit().
* Updated moved OSF URLs to their new canonical format.
* Fixed dead link to Posit keyboard shortcuts documentation.
* Removed rosetta (archived from CRAN), ufs, shinytest, and GGally from 
  Suggests. Deprecated the use_psych parameter in compute_reliabilities()
  which previously allowed switching to rosetta.
* Fixed a few issues

## Test environments
* local macOS (aarch64-apple-darwin20), R 4.5.1
* win-release
* rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

## Particularities
* This package uses rmarkdown partials, i.e. some of the functions are designed
  to render Rmd files as children of larger Rmd files. I put these files in
  the inst/ folder, their names start with _ (suggested convention in the
  rmarkdown documentation).
  I tried to make sure that these were still well-tested and they are part 
  of the testing run. Still, this leads to some packages being imported that are
  only used inside the Rmds (namely graphics and jsonlite) and tests. 
  I tried to put nontrivial R code into functions, but some minor processing 
  happens in the Rmds.
