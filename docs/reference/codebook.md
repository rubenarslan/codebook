# Generate rmarkdown codebook

Pass a data frame to this function to make a codebook for that dataset.
If the dataset has metadata (attributes) set on its variables, these
will be used to make the codebook more informative. Examples are item,
value, and missing labels. Data frames imported via
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html),
[`haven::read_sav()`](https://haven.tidyverse.org/reference/read_spss.html),
or from [formr.org](https://formr.org) will have these attributes in the
right format. By calling this function inside a knitr code chunk, the
codebook will become part of the document you are generating.

## Usage

``` r
codebook(
  results,
  reliabilities = NULL,
  survey_repetition = c("auto", "single", "repeated_once", "repeated_many"),
  detailed_variables = TRUE,
  detailed_scales = TRUE,
  survey_overview = TRUE,
  missingness_report = TRUE,
  metadata_table = TRUE,
  metadata_json = TRUE,
  indent = "#"
)
```

## Arguments

- results:

  a data frame, ideally with attributes set on variables

- reliabilities:

  a named list with one entry per scale and one or several printable
  reliability computations for this scale. if NULL, computed on-the-fly
  using compute_reliabilities

- survey_repetition:

  defaults to "auto" which is to try to determine the level of
  repetition from the "session" and "created" variables. Other values
  are: single, repeated_once, repeated_many

- detailed_variables:

  whether to print a graph and summary for each variable

- detailed_scales:

  whether to print a graph and summary for each scale

- survey_overview:

  whether to print an overview of survey entries, durations (depends on
  presence of columns session, created, modified, ended, expired)

- missingness_report:

  whether to print a missingness report. Turn off if this gets too
  complicated and you need a custom solution (e.g. in case of random
  missings).

- metadata_table:

  whether to print a metadata table/tabular codebook.

- metadata_json:

  whether to include machine-readable metadata as JSON-LD (not visible)

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h2

## Examples

``` r
# will generate figures in a temporary directory
if (FALSE) { # \dontrun{
data("bfi")
bfi <- bfi[, c("BFIK_open_1", "BFIK_open_1")]
md <- codebook(bfi, survey_repetition = "single", metadata_table = FALSE)
} # }
```
