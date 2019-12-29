## Submission
* Bugfix release to make compatible with skimr 2.0.0

## Test environments
* local OS X install, R 3.5.2
* Ubuntu 16 (on travis-ci), R-oldrel, R-release, R-devel
* win-builder (devel, release, R-oldrel)
* Rhub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  * Ubuntu Linux 16.04 LTS, R-release, GCC
  
## R CMD check results

0 errors | 0 warnings | 0 notes

## Particularities
* This package uses rmarkdown partials, i.e. some of the functions are designed
  to render Rmd files as children of larger Rmd files. I put these files in
  the inst/ folder, their names start with _ (suggested convention in the
  rmarkdown documentation).
  I tried to make sure that these were still well-tested and they are part 
  of the testing run. Still, this leads to some packages being imported that are
  only used inside the Rmds (and tests, namely graphics and pander). 
  I tried to put nontrivial R code into functions, but some minor processing 
  happens in the Rmds.
  
