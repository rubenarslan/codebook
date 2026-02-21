# Go from a named list to a key-value data frame or data dictionary and back

Sometimes, you'll want to have variable labels in a data.frame,
sometimes you'll have imported an existing data dictionary and will need
to turn it into a list before setting
[`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html).

## Usage

``` r
list_to_dict(named_list)

dict_to_list(dict)
```

## Arguments

- named_list:

  a named list with one element each (names being variable names,
  elements being labels)

- dict:

  a data frame with the variable names in the first and the labels in
  the second column. If they are named variable and label, they can also
  be in a different order.

## Examples

``` r
data('bfi')
labels <- var_label(bfi)
head(labels, 2)
#> $A1
#> NULL
#> 
#> $A2
#> NULL
#> 
dict <- list_to_dict(labels)
#> Error in dplyr::left_join(tibble::tibble(variable = names(named_list)),     tidyr::gather(tibble::as_tibble(purrr::compact(named_list)),         "variable", "label"), by = "variable"): Join columns in `y` must be present in the data.
#> ✖ Problem with `variable`.
head(dict, 2)
#> Error: object 'dict' not found
head(dict_to_list(list_to_dict(labels)), 2)
#> Error in dplyr::left_join(tibble::tibble(variable = names(named_list)),     tidyr::gather(tibble::as_tibble(purrr::compact(named_list)),         "variable", "label"), by = "variable"): Join columns in `y` must be present in the data.
#> ✖ Problem with `variable`.


```
