# How many surveys were ended?

Just a simple to check how many times a survey (e.g. diary) was
finished. It defaults to checking the "ended" variable for this.

## Usage

``` r
ended(survey, variable = "ended")
```

## Arguments

- survey:

  which survey are you asking about?

- variable:

  which variable should be filled out, defaults to "ended"

## Examples

``` r
survey <- data.frame(ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"))
ended(survey = survey)
#> [1] 2
```
