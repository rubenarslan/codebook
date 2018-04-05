# codebook 0.5.8.9000
## Additions
- codebook generation has been parallelised using the future package. By calling
  e.g. `plan(multicore(workers = 4))` before the codebook function, the 
  computation of reliabilities and the generation of scale and item summaries
  will happen in parallel. For this to work, you have to choose a graphics 
  device in knitr that supports parallelisation, by calling e.g. 
  `opts_chunk$set(dev = "CairoPNG")`.

## Bug fixes
- fix bugs in plot_labelled
- fix bugs when variables are entirely missings
- escape HTML in various labels, use safe names for anchors, figures
- reliability functions no longer garble names

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
