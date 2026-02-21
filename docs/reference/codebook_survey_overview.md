# Codebook survey overview

An overview of the number of rows and groups, and of the durations
participants needed to respond (if those data are available).

## Usage

``` r
codebook_survey_overview(results, survey_repetition = "single", indent = "##")
```

## Arguments

- results:

  a data frame which has all the following columns: session, created,
  modified, expired, ended

- survey_repetition:

  defaults to single (other values: repeated_once, repeated_many).
  controls whether internal consistency, retest reliability or
  multilevel reliability is computed

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h2

## Examples

``` r
if (FALSE) { # \dontrun{
data("bfi")
codebook_survey_overview(bfi)
} # }
```
