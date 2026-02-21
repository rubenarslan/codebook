# Define skimmers for haven_labelled_spss variables

Variables labelled using the haven_labelled_spss class are special
because the underlying data can be numeric or character. This skimmers
summarises both and leaves non-pertinent columns missings.

## Usage

``` r
get_skimmers.haven_labelled_spss(column)
```

## Arguments

- column:

  the column to skim
