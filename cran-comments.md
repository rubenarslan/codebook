## Submission
* fix language not to use underscore
* I got a warning about a dead link on win builder but the link works. false positive.
  - https://support.posit.co/hc/en-us/articles/206382178-Customizing-Keyboard-Shortcuts-in-the-RStudio-IDE

## Test environments
* local OS X install, R 4.4.1
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
  only used inside the Rmds (namely graphics and jsonlite) and tests. 
  I tried to put nontrivial R code into functions, but some minor processing 
  happens in the Rmds.
  
