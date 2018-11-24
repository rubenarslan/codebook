## 2nd resubmission
* sorry, that error wasn't caught by my tests

## Resubmission
* fix notes about `doi:` type text that had been marked up as broken URLs
* the last submission showed no other problems on win-builder, so it's tested 
there now as well.

## Submission
* haven 2.0.0 introduced a breaking change (new class name), this version
adapts to that and should fix reverse dependency breakage. Currently, installing
the most recent codebook and haven versions from CRAN will result in 
incompatibility. I'm going offline for a few days now, so I hope this submission
fixes that.
* I haven't tested this release on win-builder this time because the haven 2.0.0 
binary hadn't propagated there yet (see above for why I submitted anyway), 
but I had a user test it on a current local Windows install and tested it on 
rhub windows oldrel. Both worked and there is nothing platform-specific in the 
package itself (no compilation required either).
* One vignette uses the labelled package. It was also broken by the haven 2.0.0 
install. The maintainer wanted to submit now as well.

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
  
