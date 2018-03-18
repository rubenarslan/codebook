## Resubmission
This is a resubmission. In this version I have:

* changed the LICENSE to MIT and used the appropriate file template (before I had BSD_2_clause and didn't know I had to use a specific file template)

## Test environments
* local OS X install, R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Particularities
* This package uses rmarkdown partials, i.e. some of the functions are designed
  to render Rmd files as children of larger Rmd files. I put these files in
  the inst/ folder, their names start with _ (suggested convention in the
  rmarkdown documentation).
  I tried to make sure that these were still well-tested and they are part 
  of the testing run. Still, this leads to some packages being imported that are
  only used inside the Rmds (and tests). I tried to put nontrivial R code into 
  functions, but some minor processing happens in the Rmds.
  
