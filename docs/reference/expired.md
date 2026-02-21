# How many surveys were expired?

Just a simple to check how many times a survey (e.g. diary) has expired
(i.e. user missed it). It defaults to checking the "expired" variable
for this.

## Usage

``` r
expired(survey, variable = "expired")
```

## Arguments

- survey:

  which survey are you asking about?

- variable:

  which variable should be filled out, defaults to "expired"

## Examples

``` r
survey <- data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
expired(survey = survey)
#> [1] 1
```
