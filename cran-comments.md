## Submission
* Kurt Hornik told me to fix the fact that codebook has S3 methods it does not 
register via NAMESPACE and instead exports, namely knit_print.alpha, knit_print.htest,
knit_print.multilevel. He/his bot suspected that I do not import the generic to avoid
the knitr dependency, but I actually just made a mistake (I depend on knitr anyway). Anyway,
he urged a fix.

## Test environments
* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci, rhub, and own server), R 3.5.1
* local Windows install, R 3.5.1 and Windows-oldrel on Rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

## Particularities
* This package uses rmarkdown partials, i.e. some of the functions are designed
  to render Rmd files as children of larger Rmd files. I put these files in
  the inst/ folder, their names start with _ (suggested convention in the
  rmarkdown documentation).
  I tried to make sure that these were still well-tested and they are part 
  of the testing run. Still, this leads to some packages being imported that are
  only used inside the Rmds (and tests). I tried to put nontrivial R code into 
  functions, but some minor processing happens in the Rmds.
  
