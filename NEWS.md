# codebook 0.9.2
## Changes
- Moved a lot of dependencies to Suggests to make package leaner
- Removed functions related to scaleDiagnosis, which are now implemented in `ufs`
- Updated citation to published paper

## Bugfixes
- compatibility with skimr 2.1.0
- compatibility with vctrs 0.3.0
- compatibility with dplyr 1.0.0
- several small issues closed

# codebook 0.8.2
## Bugfixes
- compatibility with skimr 2.0.0
- compatibility with forcats 0.4.0

# codebook 0.8.1
## Bugfixes
- When errors occurred in the reliability computation, codebook would fail hard. It now fails with a warning
and continues.
- When variables of the same name existed in two datasets described in the same document, figures were overwritten.
  This is no longer the case, paths are dataset-name-prefixed now. Datasets are made "safe" for paths.

# codebook 0.8.0
## Changes
- removed three vignettes
- calculate reliability using `userfriendlyscience` instead of Cronbach's Alpha and correlations
- make it easier to generate compact codebooks
- hide machine-readable metadata in details tags (toggle to view)
- plot number of characters for character variables
- update explanations in web app slightly
- reduce survey-specific language

## Bugfixes
- make it less likely that unique/private values are disclosed (e.g., free text)

# codebook 0.7.6
## Bugfixes
- changed vignette titles (one was duplicated)

# codebook 0.7.5
## Bugfixes
- import/export knit_print generic from knitr

# codebook 0.7.4

## Additions
- Function `new_codebook_rmd` creates a new file in your working directory
with a codebook template.
- Function `metadata` can be used to set dataset-level metadata before rendering
  a codebook (valid attributes will carry over to JSON-LD representation)
- Compliance with Google Dataset Search, see [examples](https://toolbox.google.com/datasetsearch/search?query=site%3Arubenarslan.github.io)

## Changes
- removed `zap_label` because haven 2.0.0 has this function
- added several functions to add JSON-LD compliant metadata and to show it
  in the codebook
- removed some non-standard attributes from the JSON-LD metadata so that datasets
  will be indexed in Google Dataset Search
- work with haven 2.0.0's changed class names
- play nice with `userfriendlyscience::makeScales` attributes
- improved binning and wrapping in `plot_labelled`
- removed the mice dependency to reduce the number of dependencies

## Bugfixes
- `detect_missing` reset variable label with the new haven version (only between 0.6.3.9000 and 0.7.0, never on CRAN)
- `reverse_labelled_values` mislabelled values, if there were labelled missing values (numbers were correct)

# codebook 0.6.3
## Additions
- Vignettes for
  - documenting the expected attribute structure, how to add metadata in R
  - importing metadata from SPSS or Stata files
  - importing metadata from Qualtrics as made available by `qualtRics` package
- Importing some functions from labelled package to add metadata
- Default method for haven::as_factor when labelled class is absent
  
## Changes
- Changed the scale summary, so that Likert plots and distributions are shown
  on the first tab. Reliability now hidden under "Reliability details".
- removed unnecessary `readr` dependency.

## Bugfixes
- summarising factors in a table
- turning off components of the codebook without empty strings being echoed
- allow using variable and value labels in the absence of the labelled class
  (as imported by rio for example)

# codebook 0.6.2
## Additions
- Three RStudio Addin Shinyapps to browse variable labels and codebook. 

## Bug fix
- Specify a mice dependency that doesn't break degenerate test cases.

# codebook 0.5.9
## Additions
- plot_labelled now makes better plots for numeric variables
- codebook generation has been parallelised using the future package. By calling
  e.g. `plan(multicore(workers = 4))` before the codebook function, the 
  computation of reliabilities and the generation of scale and item summaries
  will happen in parallel. For this to work with plots, you have to choose a 
  graphics device in knitr that supports parallelisation, by calling e.g. 
  `opts_chunk$set(dev = "CairoPNG")`.
- for variables that store multiple multiple choice values comma-separated, 
  we now separate the values before plotting, if that item attribute
  `attributes(item)$item$type` contains "multiple"
- make it easier to trace which variable in a dataset cannot be summarised
- added and document `aggregate_and_document_scale` for people who don't import
  data via formr.org and want reliabilities to be calculated automatically
- use `rio` to import all kinds of file formats in the webapp

## Bug fixes
- fix bugs in plot_labelled
- fix bugs when variables are entirely missings
- escape HTML in various labels, use safe names for anchors, figures
- reliability functions no longer garble names
- require skimr >= 1.0.2 and ggplot2 >= 2.0.0

# codebook 0.5.8
- don't write files into anything but tempdir

# codebook 0.5.7
- changed description and documentation

# codebook 0.5.6
- changed license to MIT

# codebook 0.5.5
- improved documentation
- more tests

# codebook 0.4.4
- wrote some tests
- tried to please goodpractice::gp()
- removed some cruft
