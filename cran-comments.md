## Submission
* Compatibility with dplyr 1.0.0, vctrs 0.3.0
* Moved a lot of dependencies to Suggests to make package leaner
  * there are still many dependencies, but these are mainly tidyverse packages, so depending on one, there is little additional cost to depend also on others
  * there are other dependencies I hope to be able to remove soon (e.g., vctrs is only used in testing and for one edge case that a bugfix in haven can eliminate)
* Removed functions related to scaleDiagnosis, which are now implemented in `ufs`
* Updated citation to published paper
* Unfortunately, I have had to wrap some examples using the codebook function in dontrun, because I haven't yet figured out how to make rmdpartials switch to "interactive" mode when testing/checking. Once I fix this problem in rmdpartials, I will remove dontrun
* I also had to wrap the example for `compute_reliabilities` in dontrun, because even a minimal example sometimes takes longer than 10s. I considered using donttest instead, but according to what I could find online donttest is also tested & timed.

## Test environments
* local OS X install, R 4.0.1
* local Linux install, R 3.6.2
* win-builder (release)
* Rhub
  * Windows Server 2008 R2 SP1, R-devel, 32/64 bit

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
  
