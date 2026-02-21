# Compute reliabilities

If you pass the object resulting from a call to formr_results to this
function, it will compute reliabilities for each scale. Internally, each
reliability computation is passed to a
[`future::future()`](https://future.futureverse.org/reference/future.html).
If you are calculating multilevel reliabilities, it may be worthwhile to
parallelise this operation using
[`future::plan()`](https://future.futureverse.org/reference/plan.html).
If you don't plan on any complicated parallelisation, you probably do
not need to call this function directly, but can rely on it being
automatically called during codebook generation. If you do plan to do
that, you can pass the results of this operation to the codebook
function.

## Usage

``` r
compute_reliabilities(results, survey_repetition = "single", use_psych = TRUE)
```

## Arguments

- results:

  a formr results table with attributes set on items and scales

- survey_repetition:

  defaults to "single". Can also be "repeated_once" or "repeated_many"

- use_psych:

  deprecated. Previously allowed switching to the rosetta package, which
  has been archived. Now only psych is supported.

## Examples

``` r
data("bfi", package = "codebook")
bfi <- bfi %>% dplyr::select(dplyr::starts_with("BFIK_agree"))
reliabilities <- compute_reliabilities(bfi)
```
