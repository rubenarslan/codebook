# Define skimmers for haven_labelled variables

Variables labelled using the haven_labelled class are special because
the underlying data can be numeric or character. This skimmers
summarises both and leaves non-pertinent columns missings.

## Usage

``` r
get_skimmers.haven_labelled(column)
```

## Arguments

- column:

  the column to skim
