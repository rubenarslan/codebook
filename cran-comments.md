## Submission
* Compatibility with glue 1.8.0
* Reduced dependencies
  * no longer suggests archived package userfriendlyscience
  * there are still many dependencies, but these are mainly tidyverse packages, so depending on one, there is little additional cost to depend also on others
* Updated citation to published paper
* one declared import (graphics) is used only in one of the rmarkdown partials
  - inst/codebook_scale.Rmd, line 38
* Unfortunately, I have had to wrap some examples using the codebook function in dontrun, because I haven't yet figured out how to make rmdpartials switch to "interactive" mode when testing/checking. Once I fix this problem in rmdpartials, I will remove dontrun

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
  
