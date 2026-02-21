# Codebook component for scales

Codebook component for scales

## Usage

``` r
codebook_component_scale(
  scale,
  scale_name = deparse(substitute(scale)),
  items,
  reliabilities = list(),
  indent = "##"
)
```

## Arguments

- scale:

  a scale with attributes set

- scale_name:

  the variable name of this scale

- items:

  a data.frame with the items constituting the scale

- reliabilities:

  a list with one or several results from calls to psych package
  functions for computing reliability

- indent:

  add \# to this to make the headings in the components lower-level.
  defaults to beginning at h2

## Examples

``` r
# will generate figures in a temporary directory
if (FALSE) { # \dontrun{
data("bfi")
bfi <- bfi[,c("BFIK_open", paste0("BFIK_open_", 1:4))]
codebook_component_scale(bfi[,1], "BFIK_open", bfi[,-1],
   reliabilities = list(BFIK_open = psych::alpha(bfi[,-1])))
} # }
```
