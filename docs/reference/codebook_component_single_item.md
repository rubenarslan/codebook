# Codebook component for single items

Codebook component for single items

## Usage

``` r
codebook_component_single_item(
  item,
  item_name = deparse(substitute(item)),
  indent = "##"
)
```

## Arguments

- item:

  an item with attributes set

- item_name:

  the item name

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h2

## Examples

``` r
if (FALSE) { # \dontrun{
data("bfi")
codebook_component_single_item(bfi$BFIK_open_1, "BFIK_open_1")
} # }
```
