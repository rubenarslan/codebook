# How many surveys were modified?

Just a simple to check how many times a survey (e.g. diary) has expired
(i.e. user missed it). It defaults to checking the "expired" variable
for this.

## Usage

``` r
modified(survey, variable = "modified")
```

## Arguments

- survey:

  which survey are you asking about?

- variable:

  which variable should be filled out, defaults to "modified"

## Examples

``` r
survey <- data.frame(modified = c(NA, "2016-05-29 10:11:00", NA))
modified(survey = survey)
#> [1] 1
```
