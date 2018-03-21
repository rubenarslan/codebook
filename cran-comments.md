## Resubmission
This is the third resubmission. In this version I have:

* added the following code to examples that would have created figures in the user
  work space, so that figures are now written to a tempdir:
  #' old_base_dir <- knitr::opts_knit$get("base.dir")
  #' knitr::opts_knit$set(base.dir = tempdir())
  #' on.exit(knitr::opts_knit$set(base.dir = old_base_dir))

In the second resubmission I

* changed the title to omit "in R"
* changed the description to put 'rmarkdown' in single quotes
* added small executable examples to the codebook functions in the codebook.R. 
  Some of these examples will, by necessity, create files. 
  I know other examples also do this (e.g. knitr), but I'm unsure whether it's 
  desirable. I've noted/warned about file creation in every example.
  * the function `codebook_items` cannot be run in examples. 
    It causes an error during checking, probably because it indirectly includes 
    an htmlwidget. It works fine in non-example use (tests, vignettes, 
    interactively). Therefore, I wrapped the example in dontrun.
* slightly changed the docs to be clearer and use markdown to link functions

In the first resubmission I

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
  
