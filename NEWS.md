# codebook 0.6.0
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
