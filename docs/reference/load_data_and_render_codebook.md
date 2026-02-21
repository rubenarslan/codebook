# Submit a data file and an rmarkdown template as a file to generate a codebook. Used chiefly in the webapp.

Submit a data file and an rmarkdown template as a file to generate a
codebook. Used chiefly in the webapp.

## Usage

``` r
load_data_and_render_codebook(file, text, remove_file = FALSE, ...)
```

## Arguments

- file:

  path to a file to make codebook from (sav, rds, dta, por, xpt, csv,
  csv2, tsv, etc.)

- text:

  codebook template

- remove_file:

  whether to remove file after rendering

- ...:

  all other arguments passed to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)
