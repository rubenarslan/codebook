# Rescue lost attributes

You can use this function if some of your items have lost their
attributes during wrangling Variables have to have the same name (Duh)
and no attributes should be overwritten. But use with care. Similar to
[`labelled::copy_labels()`](https://larmarange.github.io/labelled/reference/copy_labels.html).

You can use this function if some of your items have lost their
attributes during wrangling Variables have to have the same name (Duh)
and no attributes should be overwritten. But use with care. Similar to
[`labelled::copy_labels()`](https://larmarange.github.io/labelled/reference/copy_labels.html).

## Usage

``` r
rescue_attributes(df_no_attributes, df_with_attributes)

rescue_attributes(df_no_attributes, df_with_attributes)
```

## Arguments

- df_no_attributes:

  the data frame with missing attributes

- df_with_attributes:

  the data frame from which you want to restore attributes
